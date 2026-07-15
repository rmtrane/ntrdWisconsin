behavioral_ui <- function(id = "behavior") {
  shiny::uiOutput(shiny::NS(id, "table"))
}

behavioral_server <- function(
  id = "behavior",
  dat, # reactive
  ptid = shiny::reactive("adrc00006")
) {
  NACCID <- label <- sublabel <- extra_info <- value <- variable <- VISITDATE <- NULL

  shiny::moduleServer(id, function(input, output, session) {
    respval_behavioral_dat <- shiny::reactiveVal()
    shiny::observe({
      shiny::req(dat())
      respval_behavioral_dat(respval_behavioral_data(dat()))
    })

    output$table <- shiny::renderUI({
      shiny::req(
        respval_behavioral_dat() #,
        # ptid() %in% respval_behavioral_dat()$NACCID
      )

      if (!ptid() %in% unique(respval_behavioral_dat()$NACCID)) {
        for_tab <- data.table::data.table()
      } else {
        for_dcast <- respval_behavioral_dat()[
          NACCID == ptid()
        ][,
          value := purrr::map2(as.character(value), extra_info, \(x, y) {
            c(list(value = x), if (!is.na(y)) list(extra_info = y))
          }) |>
            purrr::map_chr(format_date_cell)
        ][,
          list(
            value = ifelse(
              variable %in% c("battery", "notes", "respval"),
              value,
              paste0(
                '<ul style="margin-top: 8px; margin-bottom: 8px;">',
                paste0("<li>", value, "</li>", collapse = ""),
                '</ul>'
              )
            )
          ),
          by = list(NACCID, VISITDATE, label, variable)
        ]

        for_tab <- data.table::dcast(
          for_dcast,
          label + variable ~ VISITDATE,
          value.var = "value"
        )[order(label, -variable)]
      }

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
