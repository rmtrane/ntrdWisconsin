#' Biomarker UI
#'
#' @description
#' A short description...
#'
#' @param id A string used to namespace the module.
#'
#' @returns
#' A UI definition.
#'
#' @rdname biomarkerModule
#'
#' @keywords internal
extension_ui <- function(id = "biomarker-tables") {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "biomarker_table"))
  )
}

#' Biomarker server
#'
#' @description
#' A short description...
#'
#' @param id A string used to namespace the module.
#' @param ptid A reactive value specifying the ADRC patient ID.
#' @param biomarker_api A reactive value giving the Panda API token.
#' @param all_values A reactive value
#'
#' @returns
#' NULL.
#'
#' @rdname biomarkerModule
#'
#' @export
extension_server <- function(
  id = "biomarker-table",
  ptid,
  extras,
  base_query_file = system.file(
    "json/panda_template.json",
    package = "ntrdWisconsin"
  )
  # densities,
  # all_cuts
) {
  ###################
  ## BEFORE SERVER

  ##
  ###################

  ###################
  ## START SERVER
  shiny::moduleServer(id, function(input, output, session) {
    # reactiveValues object to hold biomarker tables. This approach
    # lets us retrieve previously generated tables without fetching
    # the data from Panda again.
    biomarker_dat_tables <- shiny::reactiveValues()

    # Prepare to hold mirai object
    mm <- NULL

    # Create ExtendedTask that will evaluate get_biomarker_data
    biomarker_dat <- shiny::ExtendedTask$new(
      \(cur_id, api) {
        mm <<- mirai::mirai(
          {
            get_biomarker_data(
              adrc_ptid = cur_id,
              api_key = api,
              base_query_file = base_query_file #"inst/json/panda_template.json"
            )
          },
          .args = list(
            get_biomarker_data = get_biomarker_data,
            cur_id = cur_id,
            api = api,
            base_query_file = base_query_file
          )
        )

        mm
      }
    )

    all_values <- shiny::reactiveVal()
    all_densities <- shiny::reactiveVal()
    all_cuts <- shiny::reactiveVal()

    biomarker_api <- shiny::reactiveVal()

    shiny::observe({
      shiny::req(extras()$all_values)

      all_values(extras()$all_values)

      biomarker_api(extras()$panda_api_token)
    })

    shiny::observe({
      shiny::req(all_values())
      all_densities(get_all_densities(all_values()))
      all_cuts(get_all_cuts(all_values()))
    })

    shiny::exportTestValues(biomarker_dat = biomarker_dat)

    # When ptid or biomarker_api is updated, invoke the ExtendedTask
    shiny::observe({
      shiny::req(biomarker_api(), ptid())

      # If the ExtendedTask is already running, stop it.
      if (biomarker_dat$status() == "running") {
        shiny::showNotification(ui = "Restarting biomarker pull")
        mirai::stop_mirai(mm)
      }

      if (ptid() != "" && !ptid() %in% names(biomarker_dat_tables)) {
        # Invoke, i.e. evaluate the ExtendedTask
        biomarker_dat$invoke(
          cur_id = ptid(),
          api = biomarker_api()
        )
      }
    }) |>
      shiny::bindEvent(
        ptid(),
        biomarker_api()
      )

    shiny::observe({
      # If ExtendedTask successfully ran...
      if (biomarker_dat$status() == "success") {
        # ... and the table for the ptid has not already been saved
        if (!ptid() %in% names(biomarker_dat_tables)) {
          biomarker_dat_tables[[ptid()]] <- biomarker_dat$result()
        }
      }
    }) |>
      shiny::bindEvent(biomarker_dat$status())

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

    output$biomarker_table <- shiny::renderUI({
      if (is.null(all_densities()) | is.null(all_cuts())) {
        return(loading_gt)
      }

      if (ptid() %in% names(biomarker_dat_tables)) {
        # lapply(biomarker_dat_tables[[ptid()]], \(x) {
        #   bio_tab_for_gt(x, )
        # }) |>
        tmp <- purrr::imap(biomarker_dat_tables[[ptid()]], \(x, idx) {
          bio_tab_for_gt(x, idx)
        }) |>
          bio_tab_to_html_table(
            densities = all_densities(),
            cuts = all_cuts(),
            print_x = F
          )

        session$sendCustomMessage("initiateTooltips", message = list())

        tmp
      } else if (biomarker_dat$status() == "running") {
        loading_gt
      } else {
        cli::cli_inform("{biomarker_dat$status()}")
      }
    })

    # Table for LP visits
    # output$lp_visits_table <- #gt::render_gt(
    #   shiny::renderUI(
    #     {
    #       if (is.null(all_densities) | is.null(all_cuts)) {
    #         return(loading_gt)
    #       }

    #       if (ptid() %in% names(biomarker_dat_tables)) {
    #         lapply(
    #           biomarker_dat_tables[[ptid()]][c(
    #             "HDX Plasma - pTau217",
    #             "Local Roche CSF - Sarstedt freeze, cleaned",
    #             "Local Roche CSF - Sarstedt freeze 2, cleaned",
    #             "Local Roche CSF - Sarstedt freeze 3",
    #             "NTK MultiObs - CSF analytes",
    #             "NTK2 MultiObs - CSF, 20230311"
    #           )],
    #           \(x) bio_tab_for_gt(x, return = "both")
    #         ) |>
    #           bio_tab_to_html_table(
    #             densities = all_densities,
    #             cuts = all_cuts
    #           )
    #       } else if (biomarker_dat$status() == "running") {
    #         loading_gt
    #       } else {
    #         cli::cli_inform("{biomarker_dat$status()}")
    #       }
    #     } #,
    #     #align = "left"
    #   )

    # # Table for PET visits
    # output$pet_visits_table <- # gt::render_gt(
    #   shiny::renderUI(
    #     {
    #       if (ptid() %in% names(biomarker_dat_tables)) {
    #         lapply(
    #           biomarker_dat_tables[[ptid()]][c(
    #             "MK6240_NFT_Rating",
    #             "NAV4694 Visual Ratings",
    #             "PIB Visual Rating 20180126"
    #           )],
    #           \(x) bio_tab_for_gt(x, return = "both")
    #         ) |>
    #           bio_tab_to_html_table()
    #       } else if (biomarker_dat$status() == "running") {
    #         loading_gt
    #       } else {
    #         cli::cli_inform("{biomarker_dat$status()}")
    #       }
    #     } #,
    #     # align = "left"
    #   )
  })
}


#' Biomarker app
#'
#' @description
#' A short description...
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
  ptid,
  biomarker_api,
  all_values,
  base_query_file = system.file(
    "json/panda_template.json",
    package = "ntrdWisconsin"
  ),
  testing = FALSE
) {
  if (!shiny::is.reactive(biomarker_api)) {
    cli::cli_abort(
      "The `biomarker_api` argument must be a reactive value, such as `shiny::reactive()`."
    )
  }

  if (!shiny::is.reactive(all_values)) {
    cli::cli_abort(
      "The `all_values` argument must be a reactive value, such as `shiny::reactive()`."
    )
  }

  ntrd::shinyAddResources()

  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }

  mirai::daemons(1)
  shiny::onStop(\(x) mirai::daemons(0))

  ui <- bslib::page_fluid(
    ntrd::shinyApp_header(),
    shiny::selectizeInput(
      inputId = "current_studyid",
      label = "ADRC ptid",
      choices = ptid
    ),
    extension_ui("biomarker-module")
  )

  server <- function(input, output, session) {
    extension_server(
      "biomarker-module",
      ptid = shiny::reactive(input$current_studyid),
      extras = shiny::reactive(
        list(
          panda_api_token = biomarker_api(),
          all_values = all_values()
        )
      ),
      base_query_file = base_query_file
    )
  }

  shiny::shinyApp(ui, server, options = list(test.mode = testing))
}


if (FALSE) {
  all_vals <- get_all_values(
    api_key = getOption("panda_api_key") #,
    # base_query_file = "inst/json/panda_template.json"
  )

  get_biomarker_data(
    "adrc00006",
    api_key = getOption("panda_api_key") #,
    #base_query_file = "inst/json/panda_template.json"
  )

  # get_all_densities(all_vals)
  # get_all_cuts(all_vals)

  extension_app(
    ptid = c("adrc00006", "adrc00121"),
    biomarker_api = shiny::reactive(getOption("panda_api_key")),
    all_values = shiny::reactive(all_vals) #,
    # base_query_file = "inst/json/panda_template.json"
  )
}
