biomarker_ui <- function(
  id = "CSF",
  title = shiny::tags$span(
    style = "color: var(--bs-body-color); font-weight: 600; font-size: 1rem; line-height: 1.3333;",
    class = "nav-link disabled nav-title",
    # style = "display: flex; align-items: center; gap: 8px; margin-left: 12px; margin-top: 24px;",
    "CSF",
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
  )
) {
  if (grepl("Visual", id)) {
    return(bslib::navset_tab(
      bslib::nav_item(title),
      header = shiny::uiOutput(shiny::NS(id, "table"))
    ))
  }

  bslib::navset_tab(
    # title = tags$span(id, class = "h5 mb-0 align-self-center"),
    bslib::nav_item(
      #tags$span(
      title #,
      # class = "h5 mb-0 pe-3 d-flex align-items-center h-100"
      #)
    ),
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Table",
      shiny::uiOutput(shiny::NS(id, "table"))
    ),
    bslib::nav_panel(
      title = "Plot",
      shiny::uiOutput(shiny::NS(id, "long_plot"))
    )
  )
}

biomarker_server <- function(
  id = "CSF",
  base_query_file = "inst/json/csf.json",
  batch_loading = NULL,
  ptid = NULL,
  all_values = NULL,
  api_token
) {
  enumber <- y <- name <- NULL

  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(batch_loading)) {
      batch_loading <- !grepl("visual_ratings", base_query_file)
    }

    bio_tables <- shiny::reactiveValues()
    mm <- NULL

    func <- NULL

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

        # if (id == "Visual Ratings") {
        #   browser()
        # }

        pt <- tolower(ptid())

        for (pt in setdiff(
          tolower(unique(bio_dat_res$enumber)),
          names(bio_tables)
        )) {
          bio_tables[[pt]] <- bio_dat_res[enumber == pt]
        }

        if (
          bio_dat$status() == "success" &&
            batch_loading &&
            !grepl("visual_ratings", base_query_file)
        ) {
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

    empty_gt <- output$table <- shiny::renderUI({
      if (bio_dat$status() == "running") {
        return(loading_gt)
      }

      # if (id == "Visual Ratings" & tolower(ptid()) == "adrc01102") {
      #   browser()
      # }

      # if batch_loading is TRUE, we always want to enter here since names of bio_tables will be all available ptid's. If not, only enter if ptid is in names of bio_tables list, since unavailable ptid's will still show up with NULL entry.
      if (tolower(ptid()) %in% names(bio_tables) || isTRUE(batch_loading)) {
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

    output$long_plot <- shiny::renderUI({
      if (bio_dat$status() == "running") {
        return(loading_gt)
      }

      bio_dat_res <- bio_dat$result()

      if (tolower(ptid()) %in% names(bio_tables) || isTRUE(batch_loading)) {
        plot_vars_labs_cutnames <- data.table::data.table(
          y = c(
            # CSF
            "csf_ratio_lumi_ab42_ab40_fda_raw",
            "csf_ratio_roche_ptau181_ab42_local_raw",
            # Plasma
            "hdx_ptau217_ashton_raw",
            "lumi_ptau217_local_raw"
          ),
          y_lab = c(
            # CSF
            "Fujirebio Lumipulse A\u{03b2}<sub>42</sub>/A\u{03b2}<sub>40</sub> (FDA)",
            "Roche pTau181/A\u{03b2}<sub>42</sub> (local)",
            # Plasma
            "Quanterix HDX pTau217 (Ashton et al.)",
            "Fujirebio Lumipulse pTau217 (local)"
          ),
          cut_name = c(
            # CSF
            "csf_ratio_lumi_ab42_ab40_fda",
            "csf_ratio_roche_ptau181_ab42_local",
            # Plasma
            "hdx_ptau217_ashton",
            "lumi_ptau217_local"
          )
        )

        if (all(!plot_vars_labs_cutnames$y %in% names(bio_dat_res))) {
          return(
            data.table::data.table(
              name = "No values found",
              name_label = "No values found"
            ) |>
              bio_tab_to_html_table(
                tab_header = ""
              )
          )
        }

        n_obs <- bio_dat_res[
          enumber == ptid(),
          sum(unlist(lapply(.SD, \(x) sum(!is.na(x))))),
          .SDcols = intersect(
            plot_vars_labs_cutnames$y,
            names(bio_dat_res)
          )
        ]

        if (n_obs == 0) {
          return(
            data.table::data.table(
              name = "No values found",
              name_label = "No values found"
            ) |>
              bio_tab_to_html_table(
                tab_header = ""
              )
          )
        }

        long_bio_plots <- purrr::pmap(
          plot_vars_labs_cutnames[y %in% names(bio_dat_res)],
          \(y, y_lab, cut_name) {
            long_bio_plot(
              dat = bio_dat_res[enumber == ptid()],
              y_val = y,
              dens = all_densities()[[y]],
              cuts = all_cuts()[[1]][name == cut_name],
              new_id = y
            )
          }
        )

        p <- plotly::subplot(
          unlist(long_bio_plots, F),
          nrows = length(long_bio_plots),
          widths = c(0.85, 0.15),
          shareY = T,
          shareX = T,
          margin = c(0, 0, 0.075, 0.075)
        ) |>
          add_row_titles(
            titles = plot_vars_labs_cutnames[y %in% names(bio_dat_res)]$y_lab
          ) |>
          on_render(long_bio_plot_js)

        p$elementId <- id

        p
      } else {
        cli::cli_inform("{bio_dat$status()")
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

  # ntrd::shinyAddResources()

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
    # shiny::tags$h4("CSF"),
    biomarker_ui("CSF"),
    shiny::tags$hr(),
    # shiny::tags$h4("Plasma"),
    biomarker_ui("Plasma"),
    shiny::tags$hr(),
    # shiny::tags$h4("Visual ratings"),
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
      id = "Visual Ratings",
      ptid = shiny::reactive(input$current_studyid),
      batch_loading = FALSE,
      api_token = biomarker_api,
      base_query_file = "inst/json/visual_ratings.json"
    )
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}
