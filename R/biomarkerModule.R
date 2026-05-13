biomarker_ui <- function(id = "CSF") {
  shiny::uiOutput(shiny::NS(id, "table"))
}

biomarker_server <- function(
  id = "CSF",
  base_query_file = "inst/json/csf.json",
  batch_loading = NULL,
  ptid = NULL,
  all_values = NULL,
  api_token
) {
  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(batch_loading)) {
      batch_loading <- !grepl("visual_ratings", base_query_file)
    }

    bio_tables <- shiny::reactiveValues()
    mm <- NULL

    bio_dat <- shiny::ExtendedTask$new(
      \(api, pt_id) {
        mm <<- mirai::mirai(
          {
            func(
              api_key = api,
              base_query_file,
              adrc_ptids = pt_id
            )
          },
          .args = list(
            func = get_biomarker_data,
            base_query_file = base_query_file,
            api = api,
            pt_id = pt_id
          )
        )
      }
    )

    all_densities <- shiny::reactiveVal()
    all_cuts <- shiny::reactiveVal()

    # When ptid or biomarker_api is updated, invoke the ExtendedTask
    shiny::observe({
      shiny::req(api_token())

      if (!batch_loading) {
        shiny::req(ptid())
        cur_ptid <- ptid()
      } else {
        cur_ptid <- NULL
      }

      # If the ExtendedTask is already running, stop it.
      if (bio_dat$status() == "running") {
        # shiny::showNotification(ui = "Restarting biomarker pull")
        mirai::stop_mirai(mm)
      }

      if (
        is.null(cur_ptid) | (cur_ptid != "" && !cur_ptid %in% names(bio_tables))
      ) {
        # Invoke, i.e. evaluate the ExtendedTask
        bio_dat$invoke(
          api = api_token(),
          pt_id = cur_ptid
        )
      }
    }) |>
      shiny::bindEvent(
        api_token(),
        if (!batch_loading) ptid()
      )

    shiny::observe({
      # If ExtendedTask successfully ran...
      if (bio_dat$status() == "success") {
        bio_dat_res <- bio_dat$result()

        for (pt in setdiff(
          tolower(unique(bio_dat_res$enumber)),
          names(bio_tables)
        )) {
          bio_tables[[tolower(pt)]] <- bio_dat_res[enumber == pt]
        }

        if (
          bio_dat$status() == "success" &&
            batch_loading &&
            !grepl("visual_ratings", base_query_file)
        ) {
          # if (id == "Plasma") {
          #   browser()
          # }

          all_densities(
            get_all_densities(bio_dat_res)
          )

          all_cuts(
            get_all_cuts(setNames(
              list(bio_dat_res),
              gsub(".json", "", basename(base_query_file))
            ))
          )
        }
      }
    }) |>
      shiny::bindEvent(bio_dat$status())

    # Table to present while getting biomarker data.
    loading_gt <- gt::gt(
      data = data.frame(x = '<span class="loading">Loading data</span>')
    ) |>
      gt::fmt(
        columns = "x",
        fns = \(x) gt::html(x)
      ) |>
      gt::cols_label(x = "") |>
      gt::opt_table_lines("none")

    output$table <- shiny::renderUI({
      if (bio_dat$status() == "running") {
        return(loading_gt)
      }

      # if batch_loading is TRUE, we always want to enter here since names of bio_tables will be all available ptid's. If not, only enter if ptid is in names of bio_tables list, since unavailable ptid's will still show up with NULL entry.
      if (tolower(ptid()) %in% names(bio_tables) || isTRUE(batch_loading)) {
        # if (ptid() == "adrc00006" & id == "Plasma") {
        #   browser()
        # }

        tmp <- bio_tab_to_html_table(
          tab_for_gt = bio_tab_for_gt(bio_tables[[ptid()]]),
          densities = all_densities(),
          cuts = all_cuts()[[1]],
          print_x = F,
          tab_header = "",
          tab_id = id
        )

        session$sendCustomMessage("initiateTooltips", message = list())

        tmp
      } else {
        cli::cli_inform("{bio_dat$status()}")
      }
    })
  })
}


biomarker_app <- function(
  ptid = NULL,
  biomarker_api = shiny::reactive(getOption("panda_api_key")),
  testing = FALSE
) {
  if (!shiny::is.reactive(biomarker_api)) {
    cli::cli_abort(
      "The `biomarker_api` argument must be a reactive value, such as `shiny::reactive()`."
    )
  }

  ntrd::shinyAddResources()

  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }

  mirai::daemons(1)
  shiny::onStop(\() mirai::daemons(0))

  ui <- bslib::page_fluid(
    ntrd::shinyApp_header(),
    shiny::selectizeInput(
      inputId = "current_studyid",
      label = "ADRC ptid",
      choices = ptid
    ),
    shiny::tags$hr(),
    shiny::tags$h4("CSF"),
    biomarker_ui("CSF"),
    shiny::tags$hr(),
    shiny::tags$h4("Plasma"),
    biomarker_ui("Plasma"),
    shiny::tags$hr(),
    shiny::tags$h4("Visual ratings"),
    biomarker_ui("Visual Ratings")
  )

  server <- function(input, output, session) {
    biomarker_server(
      "CSF",
      ptid = shiny::reactive(input$current_studyid),
      batch_loading = TRUE,
      api_token = biomarker_api,
      base_query_file = "inst/json/csf.json"
    )
    biomarker_server(
      "Plasma",
      ptid = shiny::reactive(input$current_studyid),
      batch_loading = TRUE,
      api_token = biomarker_api,
      base_query_file = "inst/json/plasma.json"
    )
    biomarker_server(
      "Visual Ratings",
      ptid = shiny::reactive(input$current_studyid),
      batch_loading = FALSE,
      api_token = biomarker_api,
      base_query_file = "inst/json/visual_ratings.json"
    )
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
