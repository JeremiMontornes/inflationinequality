library(data.table)
pkgload::load_all('.', quiet = TRUE)
h <- inflationinequality:::load_italy_level2_hbs_if_available('IT','income',2)
cat('is null:', is.null(h), '\n')
if (!is.null(h)) {
  cat('years:', paste(sort(unique(h$dt$year)), collapse=', '), '\n')
  cat('codes:', paste(sort(unique(h$dt$coicop)), collapse=', '), '\n')
  print(h$dt[, .N, by=coicop][order(coicop)])
}
