devtools::load_all(".")

library(data.table)

euro_area_countries <- c(
  "AT", "BE", "CY", "DE", "EE", "EL", "ES", "FI", "FR",
  "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK"
)

categories <- c("income")

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

bridge_for_case <- function(country, category, requested_level, effective_level) {
  message("Building ", country, " ", category, " requested level ", requested_level,
          " effective level ", effective_level)

  recode <- !(country %in% c("FR", "ES") && effective_level == 3)
  bridge <- build_coicop_bridge(
    country = country,
    category = category,
    level = effective_level,
    start_year = 2026,
    end_year = 2026,
    recode_ecoicop2_to_ecoicop1 = recode
  )
  bridge[, `:=`(
    requested_level = requested_level,
    effective_level = effective_level,
    recode_ecoicop2_to_ecoicop1 = TRUE,
    audit_scope = if (requested_level == effective_level) {
      "requested level"
    } else {
      "fallback to available HBS level"
    }
  )]
  bridge[]
}

build_bridge_table <- function(requested_level) {
  if (requested_level == 2) {
    grid <- CJ(country = euro_area_countries, category_type = categories)
    grid[, effective_level := fifelse(country == "LT", 1L, 2L)]
  } else if (requested_level == 3) {
    grid <- CJ(country = c("ES", "FR"), category_type = categories)
    grid[, effective_level := 3L]
  } else {
    stop("Only requested levels 2 and 3 are supported.")
  }

  rows <- lapply(seq_len(nrow(grid)), function(i) {
    tryCatch(
      bridge_for_case(
        country = grid$country[i],
        category = grid$category_type[i],
        requested_level = requested_level,
        effective_level = grid$effective_level[i]
      ),
      error = function(e) {
        warning(
          "Skipping ", grid$country[i], " ", grid$category_type[i],
          " level ", requested_level, ": ", conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  })

  rbindlist(rows, fill = TRUE)
}

compact_bridge <- function(bridge) {
  latest_year <- max(bridge$weight_year, na.rm = TRUE)
  bridge[
    ,
    .(
      first_weight_year = min(weight_year, na.rm = TRUE),
      last_weight_year = max(weight_year, na.rm = TRUE),
      hbs_years = paste(sort(unique(hbs_year)), collapse = ", "),
      hbs_code_available = all(hbs_code_available),
      latest_hicp_weight = round(sum(hicp_weight[weight_year == latest_year], na.rm = TRUE), 4)
    ),
    by = .(
      country, category_type, requested_level, effective_level,
      recode_ecoicop2_to_ecoicop1, audit_scope,
      hicp_coicopv2, hicp_coicop, hbs_coicop, mapping_status
    )
  ][order(country, category_type, hicp_coicop)]
}

coverage_summary <- function(bridge) {
  compact <- unique(bridge[, .(
    country, category_type, requested_level, effective_level,
    hicp_coicop, hbs_coicop, mapping_status, hbs_code_available
  )])
  compact[
    ,
    .(
      n_hicp_codes = uniqueN(hicp_coicop),
      n_exact = uniqueN(hicp_coicop[mapping_status == "exact"]),
      n_rolled_up = uniqueN(hicp_coicop[mapping_status != "exact"]),
      hicp_covered_after_rollup_rate = round(mean(hbs_code_available), 4)
    ),
    by = .(country, category_type, requested_level, effective_level)
  ][order(country, category_type)]
}

write_audit_html <- function(bridge, requested_level, file) {
  compact <- compact_bridge(bridge)
  summary <- coverage_summary(bridge)

  manual_corrections <- as.data.table(ecoicop_v2_to_v1_bridge)[
    mapping_type != "identity",
    .N
  ]

  title <- paste0(
    "Euro area HICP-HBS COICOP bridge, level ",
    requested_level,
    ", ECOICOP v1 audit"
  )

  level_note <- if (requested_level == 2) {
    paste(
      "Requested package level = 2 for euro-area country calculations.",
      "Lithuania (LT) is included at effective level = 1 because harmonised HBS",
      "data are only available at that level."
    )
  } else {
    paste(
      "Requested package level = 3 is currently available through national",
      "package workflows for France and Spain. Other euro-area countries remain",
      "covered by the harmonised level = 2 workflow."
    )
  }

  metadata <- data.table(
    field = c(
      "Generated on",
      "Requested COICOP level",
      "HICP code version",
      "HBS code version",
      "Bridge source",
      "ECOICOP v2-to-v1 table",
      "Manual/non-identity corrections",
      "Countries in audit",
      "Category in audit",
      "Audit note"
    ),
    value = c(
      as.character(Sys.Date()),
      as.character(requested_level),
      "ECOICOP v2 HICP item weights loaded by load_index_weights()",
      "ECOICOP v1-style HBS COICOP codes loaded by load_hbs() or bundled national HBS objects",
      "build_coicop_bridge(), using the same package matching rules as calculate_weights()/calculate_price_indices()",
      "data/ecoicop_v2_to_v1_bridge.rda via recode_coicop_ecoicop2_to_ecoicop1()",
      as.character(manual_corrections),
      paste(sort(unique(bridge$country)), collapse = ", "),
      paste(sort(unique(bridge$category_type)), collapse = ", "),
      paste(level_note, "The HICP-to-HBS code bridge is audited on income,",
            "which is the reference distribution used for euro-area inequality",
            "tables; category-specific HBS availability remains visible through",
            "build_coicop_bridge().")
    )
  )

  html <- paste0(
    "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<title>", html_escape(title), "</title>\n",
    "<style>",
    "body{font-family:Arial,sans-serif;margin:24px;color:#222}",
    "table{border-collapse:collapse;width:100%;font-size:12px;margin:14px 0 28px}",
    "th,td{border:1px solid #ddd;padding:6px 8px;text-align:left;vertical-align:top}",
    "th{position:sticky;top:0;background:#f3f3f3}",
    "tr:nth-child(even){background:#fafafa}",
    ".meta{color:#555;margin-bottom:16px;max-width:980px}",
    ".summary{max-width:1100px}",
    "</style>\n</head>\n<body>\n",
    "<h1>", html_escape(title), "</h1>\n",
    "<p class=\"meta\">This audit page reports the operational HICP-HBS COICOP mapping used by the package after ECOICOP v2 HICP codes are bridged to ECOICOP v1-style codes. The compact table keeps one row per distinct HICP-to-HBS mapping and the latest HICP-weight coverage metadata.</p>\n",
    "<h2>Audit metadata</h2>\n", data_table_to_html(metadata),
    "<h2>Coverage summary</h2>\n<div class=\"summary\">", data_table_to_html(summary), "</div>\n",
    "<h2>Compact bridge table</h2>\n", data_table_to_html(compact),
    "\n</body>\n</html>\n"
  )

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

level2_bridge <- build_bridge_table(2)
level3_bridge <- build_bridge_table(3)

level2_out <- "docs/euro_area_hicp_hbs_bridge_level2_ecoicopv1_audit.html"
level3_out <- "docs/euro_area_hicp_hbs_bridge_level3_ecoicopv1_audit.html"

write_audit_html(level2_bridge, 2, level2_out)
write_audit_html(level3_bridge, 3, level3_out)

cat("saved:", level2_out, "\n")
cat("saved:", level3_out, "\n")
