#' Summarize Braak Stages into Positivity Threshold
#'
#' Takes (vectors) of Braak stages I-VI and returns the corresponding positivity threshold.
#'
#' @param braak_1 numeric vector of 0's or 1's.
#' @param braak_2 numeric vector of 0's or 1's.
#' @param braak_3 numeric vector of 0's or 1's.
#' @param braak_4 numeric vector of 0's or 1's.
#' @param braak_5 numeric vector of 0's or 1's.
#' @param braak_6 numeric vector of 0's or 1's.
#'
#' @export
summarize_braak <- function(
  braak_1,
  braak_2,
  braak_3,
  braak_4,
  braak_5,
  braak_6
) {
  ns <- lengths(list(
    braak_1 = braak_1,
    braak_1 = braak_2,
    braak_1 = braak_3,
    braak_1 = braak_4,
    braak_1 = braak_5,
    braak_1 = braak_6
  ))

  if (length(unique(ns)) > 1) {
    cli::cli_abort("braak_1, ..., braak_6 must all have same length. ")
  }

  data.table::fcase(
    (braak_1 + braak_2 <= 1) & (braak_3 + braak_4 + braak_5 + braak_6 == 0) , "Not Elevated"                  ,
    (braak_1 + braak_2 == 2) & (braak_3 + braak_4 + braak_5 + braak_6 == 0) , "Borderline (MTL only)"         ,
    (braak_1 + braak_2 <= 1) & (braak_3 + braak_4 + braak_5 + braak_6 > 0)  , "Borderline (Neocortical only)" ,
    (braak_1 + braak_2 == 2) & (braak_3 + braak_4 + braak_5 + braak_6 > 0)  , "Elevated (MTL + Neocortical)"
  )
}
