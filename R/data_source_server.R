data_source_server <- S7::new_external_generic(
  "ntrd",
  "data_source_server",
  "source"
)

#' Data source server for wadrc_source
#'
#' @description
#' A short description...
#'
#' @param source An S7 `wadrc_source` object.
#' @param id A single string, the Shiny module ID.
#'
#' @returns
#' A `shiny::moduleServer` function that returns a list containing:
#' \itemize{
#'   \item `params`: A reactive list of API tokens, requiring `uds2_api_token`,
#'     `uds3_api_token`, and `uds4_api_token` to be present.
#'   \item `restore`: A function to update the API token input fields.
#'   \item `extras`: A `shiny::reactiveValues` object holding `all_values` (data
#'     fetched using `panda_api_token`) and `panda_api_token`.
#'   \item `session`: The Shiny session object.
#' }
#'
#' @name data_source_server
#'
#' @export
S7::method(data_source_server, wadrc_source) <- function(source, id) {
  shiny::moduleServer(id, function(input, output, session) {
    params <- shiny::reactive({
      shiny::req(input$uds2_api_token)
      shiny::req(input$uds3_api_token)
      shiny::req(input$uds4_api_token)

      list(
        uds2_api_token = input$uds2_api_token,
        uds3_api_token = input$uds3_api_token,
        uds4_api_token = input$uds4_api_token,
        panda_api_token = input$panda_api_token
      )
    })

    # Check if panda is accessible
    panda_access <- !inherits(
      try(
        httr2::req_perform(httr2::request("https://panda.medicine.wisc.edu")),
        silent = TRUE
      ),
      "try-error"
    )

    shiny::observe({
      if (!panda_access & !is.null(input$panda_api_token)) {
        session$sendCustomMessage(
          "toggleInput",
          list(id = session$ns("panda_api_token"), disable = TRUE)
        )

        shiny::updateTextInput(
          session,
          "panda_api_token",
          placeholder = "Panda server not accessible."
        )

        output$pandaNA <- shiny::renderUI({
          shiny::p(
            "Panda server could not be reached. If you are not on Campus, make sure you are connected to the SMPH VPN, and restart the application.",
            style = "color: red; font-weight: bold;"
          )
        })
      }
    })

    # fmt: skip
    restore <- function(params, pa = panda_access) {
      shiny::updateTextInput(session, "uds2_api_token", value = params$uds2_api_token)
      shiny::updateTextInput(session, "uds3_api_token", value = params$uds3_api_token)
      shiny::updateTextInput(session, "uds4_api_token", value = params$uds4_api_token)

      if (pa) {
        shiny::updateTextInput(session, "panda_api_token", value = params$panda_api_token)
      }
    }

    extras <- shiny::reactiveValues(
      all_values = NULL,
      panda_api_token = NULL,
      extension_ui = \() extension_ui(id = "biomarker-tables"),
      extension_server = \(ptid, extras) {
        extension_server(id = "biomarker-tables", ptid, extras)
      }
    )

    shiny::observe({
      if (panda_access) {
        extras$panda_api_token <- input$panda_api_token
      }
    })

    all_values_et <- shiny::ExtendedTask$new(
      \(api) {
        mirai::mirai(
          {
            get_all_values(
              api_key = api,
              base_query_file = bq_file
            )
          },
          .args = list(
            get_all_values = get_all_values,
            api = api,
            bq_file = system.file(
              "json/panda_template.json",
              package = "ntrdWisconsin"
            )
          )
        )
      }
    )

    shiny::observe({
      if (all_values_et$status() == "success") {
        extras$all_values <- all_values_et$result()
      }
    }) |>
      shiny::bindEvent(all_values_et$status())

    # # Kick off fetch whenever panda token is available

    shiny::observe({
      shiny::req(input$panda_api_token)
      shiny::req(nzchar(input$panda_api_token))

      if (panda_access) {
        all_values_et$invoke(api = input$panda_api_token)
      }
    })

    shiny::observe({
      if (all_values_et$status() == "success") {
        extras$all_values <- all_values_et$result()
      }
    })

    list(
      params = params,
      restore = restore,
      extras = extras,
      session = session
    )
  })
}
