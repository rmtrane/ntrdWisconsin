behavioral_ui <- function(id = "behavior") {
  shiny::uiOutput(shiny::NS(id, "table"))
}

behavioral_server <- function(
  id = "behavior",
  dat, # reactive
  ptid = shiny::reactive("adrc00006")
) {
  shiny::moduleServer(id, function(input, output, session) {
    respval_behavioral_dat <- shiny::reactiveVal()
    shiny::observe({
      shiny::req(dat())
      respval_behavioral_dat(respval_behavioral_data(dat()))
    })

    output$table <- shiny::renderUI({
      shiny::req(
        respval_behavioral_dat(),
        ptid() %in% respval_behavioral_dat()$NACCID
      )

      for_tab <- data.table::dcast(
        respval_behavioral_dat()[
          NACCID == ptid()
        ],
        label + sublabel + extra_info ~ VISITDATE,
        value.var = "value"
      )[order(label, -sublabel)][,
        c("sublabel", "extra_info") := list(
          purrr::map2(as.character(sublabel), extra_info, \(x, y) {
            c(list(value = x), if (!is.na(y)) list(extra_info = y))
          }),
          NULL
        )
      ]

      behavioral_table(for_tab, table_id = "behaviorTable")
    })
  })
}

behavioralApp <- function(ptid = shiny::reactive("adrc00006")) {
  redcap_dat <- ntrd::data_load(
    wadrc_source(),
    uds2_api_token = getOption("redcap_adrc_uds2")$token,
    uds3_api_token = getOption("redcap_adrc_uds3")$token,
    uds4_api_token = getOption("redcap_adrc_uds4")$token
  )

  redcap_dat_prepped <- ntrd::prepare_data(redcap_dat)

  ui <- bslib::page_fluid(behavioral_ui(id = "behavior"))

  server <- function(input, output, session) {
    behavioral_server(
      id = "behavior",
      dat = shiny::reactive(redcap_dat_prepped),
      ptid = ptid
    )
  }
  shiny::shinyApp(ui, server)
}
