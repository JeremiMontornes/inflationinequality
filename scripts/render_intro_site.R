dir.create("docs", showWarnings = FALSE)
dir.create(file.path("docs", "articles"), showWarnings = FALSE, recursive = TRUE)

article_format <- rmarkdown::html_document(
  toc = TRUE,
  toc_float = TRUE,
  theme = "cosmo",
  df_print = "paged",
  self_contained = TRUE
)

rmarkdown::render(
  input = "vignettes/inflationinequality-intro.Rmd",
  output_format = article_format,
  output_file = "index.html",
  output_dir = "docs",
  quiet = FALSE
)

cat("Rendered docs/index.html from vignettes/inflationinequality-intro.Rmd\n")

articles <- c(
  "advanced-options.Rmd",
  "inflation-burden.Rmd",
  "inflationinequality-intro.Rmd",
  "using-custom-data.Rmd",
  "verifying-calculated-inflation.Rmd"
)

for (article in articles) {
  input <- file.path("vignettes", article)
  output_file <- sub("\\.Rmd$", ".html", article)
  rmarkdown::render(
    input = input,
    output_format = article_format,
    output_file = output_file,
    output_dir = file.path("docs", "articles"),
    quiet = FALSE
  )
  cat("Rendered docs/articles/", output_file, " from ", input, "\n", sep = "")
}
