#' Rules for Categorizing Biomarkers
#'
#' Predefined thresholds for categorizing biomarker values (e.g., positive vs.
#' negative) based on published guidelines and FDA documents.
#'
#' @format A nested list indexed by Panda table name (e.g., `"HDX Plasma - pTau217"`,
#'   `"Lumipulse CSF - ABeta"`) and then by biomarker name. Each entry contains:
#'   \describe{
#'     \item{source}{A list with `description` (character) and `link` (URL) indicating
#'       where the categorization is described.}
#'     \item{thresholds}{A `data.frame` with columns `label`, `min`, `max`, and `bin`
#'       describing the categories.}
#'   }
#'
#' @seealso [categorize_ratio()], [create_thresholds_table()]
"biomarker_thresholds"
