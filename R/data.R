#' COICOP 2018 structure
#'
#' The complete 2018 edition of the Classification of Individual Consumption
#' According to Purpose structure
#'
#' @format
#' \describe{
#'   \item{code}{COICOP code}
#'   \item{title}{title of product category}
#' }
#' @source <https://unstats.un.org/unsd/classifications/econ/>
"COICOP_2018"

#' Methodological metadata
#'
#' A compact reference table describing the default methodological choices made
#' by the package, the closest Bruegel-style comparison point, and which choices
#' users can override through function arguments or custom data objects.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{topic}{Methodological topic.}
#'   \item{package_default}{Default choice implemented in this package.}
#'   \item{bruegel_comparison}{Closest corresponding choice in Bruegel's
#'   inflation inequality dataset.}
#'   \item{user_parameter}{Main function argument or object that lets users
#'   change the default.}
#'   \item{user_can_change}{Whether the choice is directly configurable.}
#'   \item{notes}{Notes, caveats, and country-specific issues.}
#' }
#' @source Package authors, based on package defaults and Bruegel's dataset
#' documentation: <https://www.bruegel.org/dataset/inflation-inequality-european-union-and-its-drivers>.
"methodology_metadata"

#' Country-specific COICOP data caveats
#'
#' A compact reference table describing country-specific caveats for matching
#' HBS income groups and national COICOP sources.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{country}{Country name.}
#'   \item{coicop_case}{Country-specific COICOP or data-source case.}
#'   \item{population_group_caveat}{Caveat about the demographic or income
#'   grouping concept.}
#'   \item{bruegel_comparison}{Closest corresponding choice or warning in
#'   Bruegel's inflation inequality dataset.}
#'   \item{package_status}{Current implementation status in this package.}
#' }
#' @source Package authors, based on package defaults, INSEE's definition of
#' "niveau de vie", Portugal INE's IDEF 2015/2016 publication, Eurostat
#' Statistics Explained HBS documentation, and Bruegel's dataset documentation:
#' <https://www.insee.fr/fr/metadonnees/definition/c1890>,
#' <https://www.ine.pt/xurl/pub/277098526>,
#' <https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Household_budget_survey_-_statistics_on_consumption_expenditure&stable=1>,
#' <https://www.bruegel.org/dataset/inflation-inequality-european-union-and-its-drivers>.
"country_coicop_special_cases"

#' ECOICOP v2 to ECOICOP v1 bridge
#'
#' Operational bridge used by the package to map ECOICOP v2 HICP item codes
#' back to ECOICOP v1-style COICOP codes before matching recent HICP data to
#' HBS expenditure weights. The table is used by
#' `recode_coicop_ecoicop2_to_ecoicop1()` and documents manual corrections and
#' ambiguous operational choices.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{country}{Scope of the rule. `"EA"` denotes the general euro-area
#'   operational bridge used for all countries unless a country-specific rule is
#'   added later.}
#'   \item{coicop_v2}{Input ECOICOP v2 code.}
#'   \item{label_v2}{ECOICOP v2 label when explicitly documented.}
#'   \item{coicop_v2_level}{Package-style COICOP level of `coicop_v2`.}
#'   \item{coicop_v1}{Output ECOICOP v1-style code.}
#'   \item{label_v1}{ECOICOP v1 label when explicitly documented.}
#'   \item{coicop_v1_level}{Package-style COICOP level of `coicop_v1`.}
#'   \item{mapping_type}{Rule status: exact, correspondence, rolled_up,
#'   manual_choice, or manual_correction.}
#'   \item{source}{Source or construction note.}
#'   \item{note}{Short explanation for manual corrections or ambiguous cases.}
#' }
#' @source Package authors, based on ECOICOP v2 / COICOP 2018 to ECOICOP v1
#' correspondence checks and package HICP-HBS matching requirements.
"ecoicop_v2_to_v1_bridge"
