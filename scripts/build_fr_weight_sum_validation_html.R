devtools::load_all(".")

country <- "FR"
category <- "income"
level <- 3
start_year <- 2010
end_year <- 2026
tolerance <- 1e-8

custom_hbs <- load_france_insee_hbs_level3()
index_weights_obj <- load_index_weights(
  country,
  level = level,
  start_year = start_year,
  end_year = end_year
)
index_weights_obj <- recode_index_weights_ecoicop2_to_ecoicop1(index_weights_obj, target_level = level)

weights_obj <- calculate_weights(
  country = country,
  category = category,
  level = level,
  custom_index_weights = index_weights(
    data.table::copy(index_weights_obj$dt),
    country = index_weights_obj$country,
    level = index_weights_obj$level,
    base_total = index_weights_obj$base_total
  ),
  custom_hbs = custom_hbs
)

weights_dt <- data.table::copy(weights_obj$dt)

validation_dt <- weights_dt[
  ,
  .(
    n_coicop = data.table::uniqueN(coicop),
    hbs_year = paste(sort(unique(year)), collapse = ", "),
    weight_sum_percent = sum(weighted_consumption, na.rm = TRUE),
    weight_sum_level_1 = sum(weighted_consumption, na.rm = TRUE) / 100
  ),
  by = .(category, weight_year)
][order(category, weight_year)]

validation_dt[, deviation_from_1 := weight_sum_level_1 - 1]
validation_dt[, abs_deviation_from_1 := abs(deviation_from_1)]
validation_dt[, status := data.table::fifelse(abs_deviation_from_1 <= tolerance, "ok", "ko")]

summary_dt <- validation_dt[
  ,
  .(
    n_tests = .N,
    n_ok = sum(status == "ok"),
    n_ko = sum(status == "ko"),
    max_abs_deviation_from_1 = max(abs_deviation_from_1, na.rm = TRUE)
  )
]
summary_dt[, status := data.table::fifelse(n_ko == 0, "ok", "ko")]

category_summary_dt <- validation_dt[
  ,
  .(
    n_tests = .N,
    n_ok = sum(status == "ok"),
    n_ko = sum(status == "ko"),
    min_weight_year = min(weight_year),
    max_weight_year = max(weight_year),
    max_abs_deviation_from_1 = max(abs_deviation_from_1, na.rm = TRUE),
    status = data.table::fifelse(any(status == "ko"), "ko", "ok")
  ),
  by = category
][order(category)]

validation_dt[, weight_sum_percent := round(weight_sum_percent, 10)]
validation_dt[, weight_sum_level_1 := round(weight_sum_level_1, 12)]
validation_dt[, deviation_from_1 := signif(deviation_from_1, 6)]
validation_dt[, abs_deviation_from_1 := signif(abs_deviation_from_1, 6)]
summary_dt[, max_abs_deviation_from_1 := signif(max_abs_deviation_from_1, 6)]
category_summary_dt[, max_abs_deviation_from_1 := signif(max_abs_deviation_from_1, 6)]

display_validation_dt <- data.table::copy(validation_dt)
data.table::setnames(
  display_validation_dt,
  old = c("category", "weight_year", "n_coicop", "hbs_year", "weight_sum_level_1",
          "deviation_from_1", "status"),
  new = c("Groupe", "Annee poids", "Nb COICOP", "Annee HBS", "Somme poids",
          "Ecart a 1", "Resultat")
)
display_validation_dt[, weight_sum_percent := NULL]
display_validation_dt[, abs_deviation_from_1 := NULL]

display_category_dt <- data.table::copy(category_summary_dt)
data.table::setnames(
  display_category_dt,
  old = c("category", "n_tests", "n_ok", "n_ko", "min_weight_year", "max_weight_year",
          "max_abs_deviation_from_1", "status"),
  new = c("Groupe", "Tests", "OK", "KO", "Premiere annee", "Derniere annee",
          "Ecart max a 1", "Resultat")
)

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
    row_status <- if ("status" %in% names(row)) {
      row[["status"]]
    } else if ("Resultat" %in% names(row)) {
      row[["Resultat"]]
    } else {
      ""
    }
    class <- if (identical(row_status, "ko")) " class=\"ko\"" else if (identical(row_status, "ok")) " class=\"ok\"" else ""
    paste0("<tr", class, ">", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table>\n<thead>", header, "</thead>\n<tbody>\n", paste(rows, collapse = "\n"), "\n</tbody>\n</table>")
}

badge <- function(status) {
  paste0("<span class=\"badge ", html_escape(status), "\">", toupper(html_escape(status)), "</span>")
}

card <- function(label, value, detail = NULL, class = "") {
  paste0(
    "<div class=\"card ", class, "\">",
    "<div class=\"card-label\">", html_escape(label), "</div>",
    "<div class=\"card-value\">", value, "</div>",
    if (!is.null(detail)) paste0("<div class=\"card-detail\">", html_escape(detail), "</div>") else "",
    "</div>"
  )
}

out <- "docs/france_weight_sum_validation_level3_insee_2010_2026.html"
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

html <- paste0(
  "<!doctype html>\n<html lang=\"fr\">\n<head>\n<meta charset=\"utf-8\">\n",
  "<title>Validation des poids France niveau 3</title>\n",
  "<style>",
  ":root{color-scheme:light;--ink:#202124;--muted:#5f6368;--line:#dfe3e8;--ok-bg:#e8f5eb;--ok:#146c2e;--ko-bg:#ffe5e5;--ko:#9b1c1c;--soft:#f6f7f9}",
  "body{font-family:Arial,sans-serif;margin:0;color:var(--ink);background:#fff}",
  "main{max-width:1180px;margin:0 auto;padding:28px 32px 40px}",
  "h1{font-size:28px;line-height:1.15;margin:0 0 8px}",
  "h2{font-size:18px;margin:28px 0 10px}",
  ".meta{color:var(--muted);font-size:14px;line-height:1.45;max-width:900px;margin:0 0 20px}",
  ".cards{display:grid;grid-template-columns:repeat(4,minmax(150px,1fr));gap:12px;margin:18px 0 22px}",
  ".card{border:1px solid var(--line);border-radius:8px;padding:14px 16px;background:#fff}",
  ".card.ok{background:var(--ok-bg);border-color:#b8dfc1}",
  ".card.ko{background:var(--ko-bg);border-color:#ffc2c2}",
  ".card-label{font-size:12px;text-transform:uppercase;color:var(--muted);letter-spacing:.03em;margin-bottom:7px}",
  ".card-value{font-size:24px;font-weight:700}",
  ".card-detail{font-size:12px;color:var(--muted);margin-top:5px}",
  ".badge{display:inline-block;padding:3px 8px;border-radius:999px;font-size:12px;font-weight:700;line-height:1}",
  ".badge.ok{background:#d7f0de;color:var(--ok)}",
  ".badge.ko{background:#ffd6d6;color:var(--ko)}",
  ".note{background:var(--soft);border-left:4px solid #adb5bd;padding:11px 14px;font-size:13px;color:#3c4043;margin:12px 0 20px}",
  ".table-wrap{overflow:auto;border:1px solid var(--line);border-radius:8px;margin:10px 0 26px}",
  "table{border-collapse:collapse;width:100%;font-size:12px}",
  "th,td{border-bottom:1px solid var(--line);padding:7px 9px;text-align:left;vertical-align:top;white-space:nowrap}",
  "th{position:sticky;top:0;background:#f1f3f4;font-weight:700}",
  "tbody tr:last-child td{border-bottom:0}",
  "tr:nth-child(even){background:#fbfbfc}",
  "tr.ok td:last-child{color:var(--ok);font-weight:700}",
  "tr.ko td:last-child{color:var(--ko);font-weight:700}",
  "@media(max-width:760px){main{padding:22px 16px}.cards{grid-template-columns:1fr 1fr}th,td{white-space:normal}}",
  "</style>\n</head>\n<body>\n",
  "<main>\n",
  "<h1>Validation de la somme des poids</h1>\n",
  "<p class=\"meta\">Controle des poids produits par <code>calculate_weights()</code> pour la France, groupes de revenu, niveau 3, ",
  start_year, "-", end_year, ". Les poids sont stockes en points de pourcentage dans le package; le rapport les divise par 100 et verifie que leur somme vaut 1.</p>\n",
  "<div class=\"cards\">",
  card("Resultat global", badge(summary_dt$status[1]), NULL, summary_dt$status[1]),
  card("Tests", summary_dt$n_tests[1], paste0(summary_dt$n_ok[1], " ok / ", summary_dt$n_ko[1], " ko")),
  card("Ecart max a 1", format(summary_dt$max_abs_deviation_from_1[1], scientific = TRUE), paste0("Tolerance: ", tolerance)),
  card("Periode", paste0(start_year, "-", end_year), paste0(level, " = niveau COICOP utilise")),
  "</div>\n",
  "<div class=\"note\">Lecture: un resultat <strong>OK</strong> signifie que, pour un groupe de menages et une annee de poids donnes, la somme des poids normalises vaut 1 a la tolerance numerique pres.</div>\n",
  "<h2>Synthese par groupe</h2>\n",
  "<div class=\"table-wrap\">", data_table_to_html(display_category_dt), "</div>\n",
  "<h2>Detail par groupe et annee</h2>\n",
  "<div class=\"table-wrap\">", data_table_to_html(display_validation_dt), "</div>\n",
  "</main>\n</body>\n</html>\n"
)

writeLines(html, out, useBytes = TRUE)
print(summary_dt)
cat("saved:", out, "\n")
