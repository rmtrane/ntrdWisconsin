#' CES-D Test Scores
#'
#' @description Create a `CESDTOTAL` object to hold CES-D Total scores
#'
#' @param x Numeric scores.
#'
#' @returns
#' An object of class `CESDTOTAL`.
#'
#' @export
CESDTOTAL <- ntrs::new_npsych_scores(
  "CESDTOTAL",
  label = "CES-D Total Score",
  domain = "Mood",
  range = c(0, 60),
  codes = c()
)
