dir.create("docs", showWarnings = FALSE)

rmarkdown::render(
  input = "vignettes/inflationinequality-intro.Rmd",
  output_format = rmarkdown::html_document(
    toc = TRUE,
    toc_float = TRUE,
    theme = "cosmo",
    df_print = "paged",
    self_contained = TRUE
  ),
  output_file = "index.html",
  output_dir = "docs",
  quiet = FALSE
)

cat("Rendered docs/index.html from vignettes/inflationinequality-intro.Rmd\n")
