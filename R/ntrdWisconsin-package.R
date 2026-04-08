#' ntrdWisconsin: Neuropsychological Test Result Dashboards for ADRC Wisconsin
#'
#' An [ntrd](https://github.com/rmtrane/ntrd) extension that integrates with the Wisconsin
#' Alzheimer's Disease Research Center (WADRC) data infrastructure. The package
#' pulls neuropsych data from WADRC REDCap databases (UDS-2, UDS-3, UDS-4) and
#' biomarker data from the Panda API, then presents results in interactive Shiny
#' dashboards with density plots and threshold-based categorization.
#'
#' @section Key functions:
#' - Data loading: [pull_redcap_data()], [wadrc_source()]
#' - Biomarker visualization: [bio_tab_to_html_table()], [density_plot()],
#'   [create_thresholds_table()]
#' - Biomarker ratios: [lumipulse_pTau217_ABeta42_ratio()],
#'   [lumipulse_ABeta42_ABeta40_ratio()], [categorize_ratio()]
#' - Shiny modules: [extension_server()], [extension_app()],
#'   [data_source_server()], [data_source_ui()]
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom methods formalArgs
#' @importFrom methods missingArg
#' @importFrom stats density
#' @importFrom stats na.omit
#' @importFrom stats setNames
#' @import ntrsTscores
## usethis namespace: end
NULL
