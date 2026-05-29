#!/usr/bin/env Rscript

Sys.setenv(INFLATIONINEQUALITY_SKIP_RUN_EXPORTS = "true")
source("scripts/paper_exports/build_paper_figures.R", local = TRUE)

target_file <- "fig_EA_hicp_price_index.png"
idx <- which(vapply(figure_registry, function(x) identical(x$file, target_file), logical(1)))
if (length(idx) != 1) {
  stop(sprintf("Could not find a unique %s builder.", target_file))
}

figure_registry[[idx]]$builder()
cat("saved:", file.path(fig_dir, target_file), "\n")
