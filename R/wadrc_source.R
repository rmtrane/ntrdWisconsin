#' Wisconsin ADRC data source
#'
#'
#' @returns
#' An S7 object of class `wadrc_source`, inheriting from `data_source`, with
#' `name = "Wisconsin ADRC"` and `id = "wadrc_redcap"`.
#'
#' @export
wadrc_source <- ntrd::new_data_source(
  id = "wadrc_redcap",
  name = "Wisconsin ADRC",
  package = "ntrdWisconsin"
)
