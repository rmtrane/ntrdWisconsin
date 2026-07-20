#' Biomarker UI
#'
#' @description
#' Shiny UI module that provides a placeholder for the biomarker results table.
#'
#' @param id A string used to namespace the module.
#'
#' @returns
#' A UI definition.
#'
#' @rdname biomarkerModule
#'
#' @keywords internal
extension_ui <- function(id = "wisconsin-extension", pa = TRUE) {
  hr_el <- shiny::tags$hr(
    style = "margin-left: 8px; margin-right: 8px; border-width: 2px;"
  )

  c(
    if (pa) {
      list(bslib::nav_panel(
        title = "Biomarkers",
        shiny::tags$script(shiny::HTML(
          "
      document.addEventListener('DOMContentLoaded', function () {
        var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'));
        tooltipTriggerList.map(function (el) {
          return new bootstrap.Tooltip(el);
        });
      });
    "
        )),
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 8px; margin-left: 12px;",
          shiny::tags$h3("Plasma", style = "margin: 0px;"),
          shiny::tags$a(
            href = "https://panda.medicine.wisc.edu/system/datadictionary2s/1368/original/report_biofluid_status_2026-02.pdf",
            target = "_blank",
            shiny::span(
              shiny::icon("external-link-alt"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "top",
              title = "Opens documentation in a new tab"
            )
          )
        ),
        biomarker_ui(shiny::NS(id, "Plasma")),
        hr_el,
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 8px; margin-left: 12px; margin-top: 24px;",
          shiny::tags$h3("CSF", style = "margin: 0px;"),
          shiny::tags$a(
            href = "https://panda.medicine.wisc.edu/system/datadictionary2s/1368/original/report_biofluid_status_2026-02.pdf",
            target = "_blank",
            shiny::span(
              shiny::icon("external-link-alt"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "top",
              title = "Opens documentation in a new tab"
            )
          )
        ),
        biomarker_ui(shiny::NS(id, "CSF")),
        hr_el,
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 8px; margin-left: 12px;",
          shiny::tags$h3("Visual ratings", style = "margin: 0px;")
        ),
        biomarker_ui(shiny::NS(id, "Visual Ratings"))
      ))
    },
    list(bslib::nav_panel(
      title = "Behavioral Checklist",
      behavioral_ui(shiny::NS(id, "behavior"))
    ))
  )
}

#' Biomarker server
#'
#' @description
#' Shiny server module that fetches biomarker data from the Panda API for
#' a given participant, caches results, and renders the interactive HTML table.
#'
#' @param id A string used to namespace the module.
#' @param ptid A reactive value specifying the ADRC patient ID.
#' @param extras A list
#'
#' @returns
#' NULL.
#'
#' @rdname biomarkerModule
#'
#' @export
extension_server <- function(
  id = "wisconsin-extension",
  ptid,
  dat,
  extras #,
  # base_query_file = system.file(
  #   "json/panda_template.json",
  #   package = "ntrdWisconsin"
  # )
) {
  ###################
  ## BEFORE SERVER

  ##
  ###################

  ###################
  ## START SERVER
  shiny::moduleServer(id, function(input, output, session) {
    panda_api <- shiny::reactive(extras()$panda_api_token)

    biomarker_server(
      "CSF",
      ptid = ptid,
      batch_loading = TRUE,
      api_token = panda_api,
      base_query_file = system.file(
        "json/csf.json",
        package = "ntrdWisconsin"
      )
    )

    biomarker_server(
      "Plasma",
      ptid = ptid, # shiny::reactive(input$current_studyid),
      batch_loading = TRUE,
      api_token = panda_api,
      base_query_file = system.file(
        "json/plasma.json",
        package = "ntrdWisconsin"
      )
    )

    biomarker_server(
      "Visual Ratings",
      ptid = ptid, # shiny::reactive(input$current_studyid),
      batch_loading = FALSE,
      api_token = panda_api,
      base_query_file = system.file(
        "json/visual_ratings.json",
        package = "ntrdWisconsin"
      )
    )

    behavioral_server(
      id = "behavior",
      dat = dat,
      ptid = ptid
    )
  })
}


#' Biomarker app
#'
#' @description
#' Creates a standalone Shiny app for viewing biomarker data, wrapping
#' [extension_ui()] and [extension_server()].
#'
#' @param ptid ADRC ptid. A character vector with patient ID's that can be chosen.
#' @param biomarker_api Panda API key.
#'
#' @returns
#' A `shinyApp` object.
#'
#' @rdname biomarkerModule
#'
#' @export
extension_app <- function(
  ptid = c("adrc00006"),
  dat,
  biomarker_api,
  testing = FALSE
) {
  if (!shiny::is.reactive(biomarker_api)) {
    cli::cli_abort(
      "The `biomarker_api` argument must be a reactive value, such as `shiny::reactive()`."
    )
  }

  ntrd::shinyAddResources()

  # if (mirai::daemons_set()) {
  #   mirai::daemons(0)
  # }

  mirai::daemons(1)
  shiny::onStop(\() mirai::daemons(0))

  ui <- bslib::page_fluid(
    ntrd::shinyApp_header(),
    shiny::selectizeInput(
      inputId = "current_studyid",
      label = "ADRC ptid",
      choices = ptid
    ),
    bslib::navset_card_underline(
      id = "long-trends",
      bslib::nav_panel(
        title = "Diagnoses",
        shiny::h3("Diagnoses go here...")
      )
    )
  )

  server <- function(input, output, session) {
    ext_ui <- extension_ui(id = "ext-module")
    if (inherits(ext_ui, "shiny.tag")) {
      ext_ui <- list(ext_ui)
    }
    for (nav_pan in ext_ui) {
      bslib::nav_insert(
        id = "long-trends",
        nav_pan,
      )
    }
    srv <- extension_server(
      "ext-module",
      ptid = shiny::reactive(input$current_studyid),
      extras = shiny::reactive(
        list(
          panda_api_token = biomarker_api()
        )
      )
    )
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
