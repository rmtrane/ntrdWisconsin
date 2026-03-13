#' Wisconsin ADRC data source
#'
#'
#' @returns
#' An S7 object of class `wadrc_source`, inheriting from `data_source`, with
#' `name = "Wisconsin ADRC"` and `id = "wadrc_redcap"`.
#'
#' @export
wadrc_source <- S7::new_class(
  "wadrc_source",
  parent = ntrd::data_source,
  constructor = function() {
    S7::new_object(
      ntrd::data_source(
        name = "Wisconsin ADRC",
        id = "wadrc_redcap"
      )
    )
  }
)
