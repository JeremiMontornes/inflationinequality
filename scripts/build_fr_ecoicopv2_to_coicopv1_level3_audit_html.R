devtools::load_all(".")

index_weights_obj <- load_index_weights("FR", level = 3, start_year = 2010, end_year = 2026)

audit_dt <- data.table::copy(index_weights_obj$dt)
data.table::setnames(audit_dt, "year", "weight_year")
audit_dt[, hicp_ecoicopv2 := coicop]
audit_dt[, hicp_coicopv1_raw := recode_coicop_ecoicop2_to_ecoicop1(hicp_ecoicopv2)]
audit_dt[, hicp_coicopv1_level3 := coicop_to_level(hicp_coicopv1_raw, 3)]
audit_dt[, mapping_status := data.table::fifelse(
  hicp_ecoicopv2 == hicp_coicopv1_level3,
  "unchanged",
  data.table::fifelse(
    hicp_coicopv1_raw == hicp_coicopv1_level3,
    "recoded",
    "recoded_and_truncated"
  )
)]
audit_dt[, valid_hicp_ecoicopv2 := hicp::is.coicop(
  paste0("CP", hicp_ecoicopv2),
  settings = list(coicop.version = "ecoicop2.hicp", coicop.prefix = "CP")
)]
audit_dt[, valid_hicp_coicopv1_level3 := hicp::is.coicop(
  paste0("CP", hicp_coicopv1_level3),
  settings = list(coicop.version = "ecoicop1.hicp", coicop.prefix = "CP")
)]
audit_dt[, hicp_ecoicopv2_label := hicp::label(
  paste0("CP", hicp_ecoicopv2),
  settings = list(coicop.version = "ecoicop2.hicp", coicop.prefix = "CP")
)]
audit_dt[, hicp_coicopv1_level3_label := hicp::label(
  paste0("CP", hicp_coicopv1_level3),
  settings = list(coicop.version = "ecoicop1.hicp", coicop.prefix = "CP")
)]

normalize_label <- function(x) {
  x <- tolower(as.character(x))
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("\\([^)]*\\)", "", x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  x
}

labels_equivalent <- function(v2_label, v1_label) {
  v2 <- normalize_label(v2_label)
  v1 <- normalize_label(v1_label)
  if (is.na(v2) || is.na(v1) || v2 == "" || v1 == "") {
    return(NA)
  }
  if (v2 == v1) {
    return(TRUE)
  }

  v2_tokens <- unique(strsplit(v2, " ", fixed = TRUE)[[1]])
  v1_tokens <- unique(strsplit(v1, " ", fixed = TRUE)[[1]])
  stop_words <- c("and", "or", "of", "the", "for", "to", "in", "on", "by", "with", "a", "an")
  v2_tokens <- setdiff(v2_tokens, stop_words)
  v1_tokens <- setdiff(v1_tokens, stop_words)
  if (length(v2_tokens) == 0 || length(v1_tokens) == 0) {
    return(FALSE)
  }

  all(v1_tokens %in% v2_tokens) || all(v2_tokens %in% v1_tokens)
}

audit_dt[, hicp_ecoicopv2_label_key := normalize_label(hicp_ecoicopv2_label)]
audit_dt[, hicp_coicopv1_level3_label_key := normalize_label(hicp_coicopv1_level3_label)]
audit_dt[, label_interpretation_ok := mapply(
  labels_equivalent,
  hicp_ecoicopv2_label,
  hicp_coicopv1_level3_label
)]
audit_dt[, label_interpretation_diff := data.table::fifelse(
  is.na(label_interpretation_ok),
  NA_character_,
  data.table::fifelse(label_interpretation_ok, "ok", "ko")
)]
audit_dt[, n_ecoicopv2_per_v1 := data.table::uniqueN(hicp_ecoicopv2), by = hicp_coicopv1_level3]
audit_dt[
  ,
  doubt_note := paste(
    c(
      if (!valid_hicp_ecoicopv2[1]) "source_not_in_hicp_ecoicopv2_dictionary" else NULL,
      if (!valid_hicp_coicopv1_level3[1]) "target_not_in_hicp_coicopv1_dictionary" else NULL,
      if (identical(label_interpretation_diff[1], "ko")) "label_interpretation_diff" else NULL
    ),
    collapse = "; "
  ),
  by = .(hicp_ecoicopv2, hicp_coicopv1_level3, mapping_status)
]
audit_dt[doubt_note == "", doubt_note := "ok"]

latest_year <- max(audit_dt$weight_year, na.rm = TRUE)
audit_dt <- audit_dt[
  ,
  .(
    hicp_ecoicopv2_label = hicp_ecoicopv2_label[1],
    hicp_coicopv1_level3_label = hicp_coicopv1_level3_label[1],
    valid_hicp_ecoicopv2 = valid_hicp_ecoicopv2[1],
    valid_hicp_coicopv1_level3 = valid_hicp_coicopv1_level3[1],
    label_interpretation_diff = label_interpretation_diff[1],
    doubt_note = doubt_note[1],
    first_weight_year = min(weight_year),
    last_weight_year = max(weight_year),
    latest_weight = sum(weight[weight_year == latest_year], na.rm = TRUE)
  ),
  by = .(hicp_ecoicopv2, hicp_coicopv1_raw, hicp_coicopv1_level3, mapping_status)
][order(hicp_ecoicopv2)]
audit_dt[, latest_weight := round(latest_weight, 4)]

summary_dt <- audit_dt[
  ,
  .(
    n_codes = .N,
    latest_weight = round(sum(latest_weight), 4)
  ),
  by = .(mapping_status, doubt_note)
][order(mapping_status, doubt_note)]

html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

data_table_to_html <- function(dt) {
  header <- paste0("<tr>", paste0("<th>", html_escape(names(dt)), "</th>", collapse = ""), "</tr>")
  rows <- apply(dt, 1, function(row) {
    paste0("<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table>\n<thead>", header, "</thead>\n<tbody>\n", paste(rows, collapse = "\n"), "\n</tbody>\n</table>")
}

out <- "docs/france_hicp_ecoicopv2_to_coicopv1_level3_audit.html"
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

html <- paste0(
  "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
  "<title>France HICP ECOICOP v2 to COICOP v1 bridge, level 3 audit</title>\n",
  "<style>",
  "body{font-family:Arial,sans-serif;margin:24px;color:#222}",
  "table{border-collapse:collapse;width:100%;font-size:12px;margin:14px 0 28px}",
  "th,td{border:1px solid #ddd;padding:6px 8px;text-align:left;vertical-align:top}",
  "th{position:sticky;top:0;background:#f3f3f3}",
  "tr:nth-child(even){background:#fafafa}",
  ".meta{color:#555;margin-bottom:16px}",
  ".summary{max-width:720px}",
  "</style>\n</head>\n<body>\n",
  "<h1>France HICP ECOICOP v2 to COICOP v1 bridge, level 3 audit</h1>\n",
  "<p class=\"meta\">Rows: ", nrow(audit_dt),
  ". Latest weight year: ", latest_year,
  ". Built from French HICP index-weight codes and the package retro-passage function ",
  "<code>recode_coicop_ecoicop2_to_ecoicop1()</code>, then truncated with ",
  "<code>coicop_to_level(..., 3)</code>. The table keeps the original rows and adds labels plus a doubt note only when a code is not found in the HICP dictionaries exposed by the <code>hicp</code> package or when the ECOICOP v2 and COICOP v1 labels differ after simple text normalisation.</p>\n",
  "<h2>Summary</h2>\n<div class=\"summary\">", data_table_to_html(summary_dt), "</div>\n",
  "<h2>Code-level audit</h2>\n", data_table_to_html(audit_dt),
  "\n</body>\n</html>\n"
)

writeLines(html, out, useBytes = TRUE)
print(summary_dt)
cat("saved:", out, "\n")
