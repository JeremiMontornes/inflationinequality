# BdF-specific
options(rdbnomics.use_readLines = TRUE)

#' Load CPI data
#'
#' @param country 2-digit ISO country (1 country allowed currently)
#' @importFrom dplyr %>%
#' @export
load_cpi <- function(country, level) {
    rdb_mask <- stringr::str_c("M.I05..", country)
    dt_prod_FR <- rdbnomics::rdb("Eurostat", "prc_hicp_midx",
                                 mask = rdb_mask) %>%
        dplyr::filter(period>"2020-12-01") %>%
        select_coicop_level(level) %>%
        dplyr::mutate(year=lubridate::year(period)) %>%
        dplyr::rename(yearmonth=original_period) %>%
        dplyr::select(series_name, coicop, value, year, yearmonth)

    return(dt_prod_FR)
}

#' Load product weights
#'
#' @importFrom dplyr %>%
#' @export
load_weights <- function(country, level) {
    rdb_mask <- stringr::str_c("A..", country)
    dt_weights_FR <- rdbnomics::rdb("Eurostat", "prc_hicp_inw",
                                            mask = rdb_mask) %>%
        dplyr::filter(period>"2020-12-01") %>%
        select_coicop_level(level) %>%
        dplyr::rename(weight=value) %>%
        dplyr::mutate(year=lubridate::year(period)) %>%
        # We don't pick series_name since it's annual (redundant) and has no COICOP code
        dplyr::select(coicop, weight, year)

    return(dt_weights_FR)
}

#' Load HBS (2020 only for now)
#' Note: If we want France with COICOP level 3, we need to use INSEE
#' INSEE cannot be accessed from proxy
#' Also, Germany suffers the same problem on Eurostat, as well as Italy
#' quantile are strings that are either numbers "1" to "10" or "TOT"
#'
#' @export
load_hbs <- function(country, level) {
    if (country == "FR") {
        # This is 2017 data, we cannot find 2020 HBS data so the latest one possible
        # will do
        library(rio)
        df_hbs <- rio::import("https://www.insee.fr/fr/statistiques/fichier/4648335/TF106.csv",
                              setclass = "data.table") %>%
            dplyr::rename(coicop=NOMENCLATURE) %>%
            dplyr::rename(quantile=DECUC) %>%
            dplyr::rename(consumption=CONSO) %>%
            dplyr::filter(stringr::str_length(coicop) == level+1)

        # df_hbs_FR  <- read_xlsx("//adbdf.private/applications/AU_AMIC/LPR_2022_SAMIC/Programme/TF106_3digit.xlsx",
        #                            col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) %>%
        #     dplyr::rename(coicop=IDENT) %>%
        #     dplyr::rename(quantile=DECUC) %>%
        #     dplyr::select(!Regroupement) %>%
        #     dplyr::filter(stringr::str_length(coicop) == 3+1) %>%
        #     dplyr::mutate(country="FR", .before = coicop)
    }
    else {
        rdb_mask <- stringr::str_c("A.PM...", country)
        dt_hbs <- rdbnomics::rdb("Eurostat", "hbs_str_t223",
                                         mask = rdb_mask) %>%
            dplyr::filter(period == "2020-01-01") %>%
            select_coicop_level(3) %>%
            dplyr::rename(expenditure=value) %>%
            dplyr::select(series_name, coicop, expenditure)
    }

    return(df_hbs)
}

#' Load HICP and HBS data
#' Kind of useless
#' Remember that CPI for one product changes monthly, so at least 12 rows
#' So HBS data for one product will be multiplied by at least 12
#' => lots of redundant data points
#' @export
load_data <- function() {
    dt_hicp_FR <- load_hicp()
    df_hbs_FR <- load_hbs()
    dt_data_FR <- merge(dt_hicp_FR,
                        df_hbs_FR,
                        by=c("coicop"))
    return(dt_data_FR)
}

# Select COICOP level
# (This is a helper or private function, it should not be public!)
#' @importFrom dplyr %>%
select_coicop_level <- function(.dt, level) {
    .dt %>%
        dplyr::filter(stringr::str_sub(coicop,1,2) =="CP" ) %>%
        dplyr::mutate(dplyr::across('coicop', stringr::str_replace,
                                    'CP', '')) %>%
        dplyr::filter(stringr::str_length(coicop) == level+1)
}
