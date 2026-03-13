#' @include wadrc_source.R

data_source_ui <- S7::new_external_generic(
  "ntrd",
  "data_source_ui",
  "source"
)

#' Data source UI for WADRC source
#'
#' @param source An S7 `wadrc_source` object.
#' @param ns A Shiny namespace function.
#'
#' @returns
#' A `shiny::tagList` containing UI elements for configuring REDCap API tokens and a Panda API Key.
#'
#' @name data_source_ui
#'
#' @export
S7::method(data_source_ui, wadrc_source) <- function(source, ns) {
  shiny::tagList(
    shiny::tags$script(HTML(
      "Shiny.addCustomMessageHandler('toggleInput', function(msg) {
        var el = document.getElementById(msg.id);
        if (msg.disable) {
          el.setAttribute('disabled', 'disabled');
        } else {
          el.removeAttribute('disabled');
        }
      });
      "
    )),
    bslib::card(
      bslib::card_title("REDCap API Tokens"),
      api_token_inputs(
        label = "UDS-2:",
        placeholder = "Enter REDCap API Token for UDS-2",
        inputId = ns("uds2_api_token")
      ),
      api_token_inputs(
        label = "UDS-3:",
        placeholder = "Enter REDCap API Token for UDS-3",
        inputId = ns("uds3_api_token")
      ),
      api_token_inputs(
        label = "UDS-4:",
        placeholder = "Enter REDCap API Token for UDS-4",
        inputId = ns("uds4_api_token")
      )
    ),
    bslib::card(
      bslib::card_title("Panda API Key"),
      shiny::uiOutput(ns("pandaNA")),
      api_token_inputs(
        label = "Key:",
        placeholder = "Enter Panda API Key (optional)",
        inputId = ns("panda_api_token")
      )
    )
  )
}


#' API token inputs
#'
#'
#' @param label A single string. Optional.
#' @param placeholder A single string. Optional.
#' @param inputId A single string. Optional.
#'
#' @returns
#' A Shiny `fluidRow` UI element containing a labeled password input field.
#'
#' @keywords internal
api_token_inputs <- function(
  label = "UDS-2:",
  placeholder = "Enter REDCap API Token for UDS-2",
  inputId = "uds_api_token" # ns("uds2_api_token")
) {
  shiny::fluidRow(
    shiny::column(
      2,
      shiny::tags$label(
        label,
        `for` = inputId,
        style = "text-align: right; display: block; padding-top: 7px;"
      )
    ),
    shiny::column(
      6,
      shiny::tagAppendAttributes(
        shiny::passwordInput(
          inputId = inputId,
          label = NULL,
          placeholder = placeholder
        ),
        .cssSelector = "input",
        autocomplete = "current-password"
      )
    )
  )
}
