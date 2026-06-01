suppressPackageStartupMessages({
  library(data.table)
})

istat_dir <- Sys.getenv(
  "ISTAT_HBS_DIR",
  "C:/Users/jerem/Documents/New project/data/istat_hbs"
)
zip_dir <- file.path("data-raw", "italy_hbs", "zips")
dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
zip_dir <- normalizePath(zip_dir, winslash = "/", mustWork = TRUE)

years <- c(2015L, 2020L)

age_vars <- c(
  "w_anno", "c_c_etacalc_1",
  "d_01", "d_02", "d_03", "d_04_str", "d_05", "d_06_aggr_1",
  "d_07", "d_08", "d_09", "d_10", "d_11", "d_12"
)

latent_vars <- c(
  "c_Ncmp_altro", "ncomp", "NCOMP",
  "c_titstu_1", "TITSTU_PR",
  "c_cond_1", "condFL_1",
  "c_pospro_1", "c_profess1dig_1",
  "Titoccup", "Propabit", "tipabitaz_new",
  "c_Superf", "c_Superf_sec",
  "c_Stanze", "c_Stanze_sec",
  "Possauto", "Numauto_topcod",
  "Posspc", "Numpc_topcod",
  "Internet", "Risecon", "Sitecon",
  "povassc", "poveri", "povassc_rico", "poveri_rico",
  "rgn", "rip"
)

required_vars <- unique(c(age_vars, latent_vars))

zip_member <- function(zip_file, pattern = "MICRODATI/.*\\.txt$") {
  members <- utils::unzip(zip_file, list = TRUE)
  hit <- members$Name[grepl(pattern, members$Name, ignore.case = TRUE)]
  if (length(hit) == 0L) {
    stop("No ZIP member matched pattern: ", pattern, call. = FALSE)
  }
  hit[[1L]]
}

restrict_zip <- function(year) {
  file_name <- sprintf("HBS_%s_IT.zip", year)
  repo_zip <- file.path(zip_dir, file_name)
  source_zip <- file.path(istat_dir, file_name)
  input_zip <- if (file.exists(source_zip)) source_zip else repo_zip
  if (!file.exists(input_zip)) {
    stop("Missing Istat ZIP: ", source_zip, call. = FALSE)
  }

  source_member <- zip_member(input_zip)
  raw_dir <- tempfile(sprintf("hbs_%s_raw_", year))
  slim_dir <- tempfile(sprintf("hbs_%s_slim_", year))
  dir.create(raw_dir)
  dir.create(file.path(slim_dir, dirname(source_member)), recursive = TRUE)
  on.exit(unlink(c(raw_dir, slim_dir), recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(input_zip, files = source_member, exdir = raw_dir, junkpaths = TRUE)
  extracted <- file.path(raw_dir, basename(source_member))
  header <- fread(extracted, nrows = 0L)
  keep <- intersect(required_vars, names(header))
  missing <- setdiff(required_vars, names(header))

  dt <- fread(extracted, select = keep)
  out_member_path <- file.path(slim_dir, source_member)
  fwrite(dt, out_member_path, sep = "\t", na = "")

  old_wd <- setwd(slim_dir)
  tmp_zip <- tempfile(fileext = ".zip")
  utils::zip(tmp_zip, files = source_member, flags = "-r9X")
  setwd(old_wd)
  file.copy(tmp_zip, repo_zip, overwrite = TRUE)

  data.table(
    year = year,
    zip = repo_zip,
    rows = nrow(dt),
    kept_columns = length(keep),
    missing_optional_columns = paste(missing, collapse = ", "),
    size_bytes = file.info(repo_zip)$size
  )
}

summary <- rbindlist(lapply(years, restrict_zip), use.names = TRUE)
print(summary)
