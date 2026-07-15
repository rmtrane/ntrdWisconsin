#' Convert biomarker data table to HTML table
#'
#' @description
#' Converts a list of biomarker `data.table`s (as returned by [bio_tab_for_gt()])
#' into an interactive HTML table with inline density plots, threshold indicators,
#' and tooltip info icons for use in a Shiny application.
#'
#' @param tab_for_gt A list of `data.table`s, where each `data.table` is named after the Panda table that was queried to get the data.
#' @param densities A list of density data.
#' @param cuts A list or data structure containing cut-off values.
#' @param www_path Path to www/ folder that contains .css and .js files. Defaults to either inst/www or www subdir of NpsychAssessmentTool pacakge.
#' @param print_x For debugging
#' @param tab_id ID for the table, used to create unique cell IDs.
#' @param tab_header Header label for the table.
#'
#' @returns
#' A `shiny::tagList` object representing the HTML table. Will error if `tab_for_gt` is not
#' a list of `data.table`s as expected.
#'
#' @seealso [density_plot()], [create_thresholds_table()]
#'
#' @export
bio_tab_to_html_table <- function(
  tab_for_gt,
  densities,
  cuts,
  www_path = system.file("www", package = "ntrdWisconsin"),
  print_x = FALSE,
  tab_id = "csf",
  tab_header = "method"
) {
  ## To avoid notes in R CMD check
  method <- NULL
  name <- NULL
  name_i <- NULL
  name_label <- NULL
  link <- NULL
  description <- NULL
  thresholds <- NULL

  if (!is.list(tab_for_gt) & !inherits(tab_for_gt, "data.table")) {
    cli::cli_abort(
      "{.arg tab_for_gt} must be a list of {.cls data.table}'s named after the Panda table that was queried to get the data."
    )
  }

  if (is.list(tab_for_gt) && !data.table::is.data.table(tab_for_gt)) {
    tab_for_gt <- tab_for_gt[!unlist(lapply(tab_for_gt, is.null))]

    tab_for_gt <- lapply(tab_for_gt, \(x) {
      if (inherits(x, "try-error")) {
        x <- data.table::data.table(
          name = x[1]
        )
      }

      if (inherits(x, "error-message")) {
        x <- data.table::data.table(name = x)
      }

      x
    })

    ## If some entries are not data.table's, abort.
    if (!all(sapply(tab_for_gt, data.table::is.data.table))) {
      non_dts <- tab_for_gt[!sapply(tab_for_gt, data.table::is.data.table)]

      cli::cli_abort(
        "When {.arg tab_for_gt} is of class {.cls list}, all elements must be of class {.cls data.table}, but {.val {names(non_dts)}} {?is/are} of class {.cls {unlist(sapply(non_dts, class))}}"
      )
    }

    ## Bind tables together with new column giving 'table' from names
    tab_for_gt <- data.table::rbindlist(
      purrr::imap(tab_for_gt, \(x, idx) {
        ## First, add extra row that is simply name of table. This will allow us
        ## to create row group names. But only do this if more than one table included.

        # tmp <- data.table::rbindlist(
        #   list(
        #     data.table::data.table(name = idx),
        #     x
        #   ),
        #   fill = TRUE
        # )
        tmp <- data.table::copy(x)

        tmp[,
          name_i := as.numeric(factor(
            name,
            levels = unique(c(idx, "Age", tmp$name))
          ))
        ]

        tmp
      }),
      fill = TRUE,
      idcol = "table"
    )
  } else {
    tab_for_gt <- data.table::copy(tab_for_gt)

    tab_for_gt[,
      c("table", "name_i") := list(
        "table",
        as.numeric(factor(
          name,
          levels = unique(c("Age", name))
        ))
      )
    ]
  }

  ## Get all visit dates
  visit_dates <- # setdiff(names(tab_for_gt), c("name", "table", "name_i"))
    grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", names(tab_for_gt), value = TRUE)

  ## Arrange columns by visits
  tab_for_gt <- tab_for_gt[,
    c(
      "table",
      "name",
      "name_i",
      "name_label",
      sort(visit_dates)
    ),
    with = F
  ]

  ## Create method column giving the visit type (Plasma, CSF, Visual) (old: LP or PET)
  tab_for_gt[,
    method := data.table::fcase(
      table == "csf"            , "CSF"            ,
      table == "plasma"         , "Plasma"         ,
      table == "visual_ratings" , "Visual Ratings" ,
      default = tab_header
    )
  ]

  ## Get row of ages.
  age_rows <- tab_for_gt[
    tab_for_gt$name == "Age"
  ][,
    # setNames(
    c("table", names(.SD)) := c(
      "age",
      ## Get all unique ages in each column...
      lapply(.SD, \(x) {
        out <- unique(unlist(x, recursive = TRUE, use.names = FALSE))
        if (is.null(out)) {
          return(NA)
        }

        out
      })
    ),
    ## ... by method
    by = method
  ]

  ## Make sure column order is correct.
  data.table::setcolorder(
    age_rows,
    c(
      intersect(
        c(
          "method",
          "table",
          "name"
        ),
        names(age_rows)
      ),
      # "info",
      names(age_rows)[
        !names(age_rows) %in% c("method", "table", "name") #, "info")
      ]
    )
  )

  data.table::setcolorder(
    tab_for_gt,
    c(
      "method",
      "table",
      "name",
      "name_i",
      # "info",
      names(tab_for_gt)[
        !names(tab_for_gt) %in% c("method", "table", "name", "name_i") #, "info")
      ]
    )
  )

  ## Add age rows to table
  tab_for_gt <- data.table::rbindlist(
    list(
      age_rows,
      tab_for_gt[tab_for_gt$name != "Age"]
    ),
    use.names = TRUE
  )

  ## Make method and table factors so we can get correct order
  tab_for_gt[,
    method := factor(
      method,
      levels = unique(c("Plasma", "CSF", "Visual Ratings", tab_header))
    )
    # c("method", "table") := list(
    #   factor(method, levels = c("Plasma", "CSF", "Visual Ratings")),
    #   factor(
    #     table,
    #     levels = c(
    #       "age",
    #       "HDX Plasma - pTau217",
    #       "Lumipulse Plasma - pTau217",
    #       "Lumipulse CSF - ABeta",
    #       "Local Roche CSF - Sarstedt freeze 3",
    #       "Amprion - CSF a-Synuclein",
    #       "Local Roche CSF - Sarstedt freeze 2, cleaned",
    #       "Local Roche CSF - Sarstedt freeze, cleaned",
    #       "NTK MultiObs - CSF analytes",
    #       "NTK2 MultiObs - CSF, 20230311",
    #       "MK6240_NFT_Rating",
    #       "NAV4694 Visual Ratings",
    #       "PIB Visual Rating 20180126"
    #     )
    #   )
    # )
  ]

  data.table::setorder(
    tab_for_gt,
    method,
    table,
    name_i
  )

  tab_for_gt[, name_i := NULL]

  ## After table has been ordered, convert back to character vectors to avoid weird factor behavior.
  tab_for_gt[,
    names(.SD) := lapply(.SD, as.character),
    .SDcols = c("method", "table")
  ]

  ## Add extra column at end of table for formatting
  tab_for_gt[,
    "append" := ""
  ]

  ## Function to create table header. We add two headers, hence the function.
  ## The argument is meant to give all columns that are actually valid visits
  ## for the method we're dealing with.
  table_header <- function(true_visits) {
    shiny::tags$tr(
      lapply(seq_along(tab_for_gt), \(x) {
        x_name <- colnames(tab_for_gt)[x]

        if (x_name == "name") {
          return()
        }

        shiny::tags$th(
          ifelse(
            x_name %in%
              c("method", "table", "name_label", "info", "append") |
              length(true_visits) == 0,
            "",
            x_name
          ),
          class = if (
            !x_name %in%
              c(
                "method",
                "name_label",
                "table",
                "info",
                "append",
                true_visits
              ) &
              length(true_visits) > 0
          ) {
            prev_visit <- colnames(tab_for_gt)[x - 1] %in%
              c("method", "name_label", "table", "info", "append", true_visits)

            next_visit <- colnames(tab_for_gt)[x + 1] %in%
              c("method", "name_label", "table", "info", "append", true_visits)

            if (!prev_visit & !next_visit) {
              "lowered-column center"
            } else if (!prev_visit) {
              "lowered-column right"
            } else if (!next_visit) {
              "lowered-column left"
            } else {
              "lowered-column"
            }
          }
        )
      })
    )
  }

  ## tagList to hold actual table
  shiny::tagList(
    ## Add CSS
    shiny::includeCSS(path = paste(www_path, "biomarkerTable.css", sep = "/")),
    ## Add JS
    shiny::includeScript(
      path = paste(www_path, "biomarkerTable.js", sep = "/")
    ),
    ## Table-container
    shiny::tags$div(
      class = "table-container",
      ## Table
      shiny::tags$table(
        class = "biomarkerTable",
        ## Table body. Created rowwise.
        shiny::tags$tbody(
          lapply(1:nrow(tab_for_gt), \(x) {
            if (print_x) {
              print(x)
            }

            # if (x == 2) {
            #   browser()
            # }

            nam <- tab_for_gt$name[x]
            nam_lab <- tab_for_gt$name_label[x]
            tab <- tab_for_gt$table[x]
            met <- tab_for_gt$method[x]

            ## Get vector of visits that are "true visits" for this method.
            ## This is based on valid age, and chosen among columns named
            ## by dates.
            true_visits <- tab_for_gt[
              method == met & name == "Age",
              lapply(.SD, \(x) {
                if (length(x) > 0 && is.na(unlist(x))) NULL else unlist(x)
              }),
              .SDcols = names(tab_for_gt)[
                grepl("20[0-9]{2}-[0-9]{2}-[0-9]{2}", names(tab_for_gt))
              ]
            ] |>
              names()

            ## Is this the last row of part of table related to this method?
            last_row <- (x == nrow(tab_for_gt)) ||
              (met != tab_for_gt$method[x + 1])

            ## If method is "new" (i.e. different from previous), we want to
            ## add header. If not, `subtable_header` will be NULL and ignored
            subtable_header <- if (!duplicated(tab_for_gt$method)[x]) {
              c(
                if (met != "") {
                  list(shiny::tags$tr(
                    class = "subtable-header",
                    shiny::tags$td(
                      colspan = ncol(tab_for_gt) - 1,
                      ## Add extra padding-top to second header
                      style = if (x > 1) "padding-top: 24px;",
                      shiny::HTML(paste("&nbsp;&nbsp;", met))
                    )
                  ))
                },
                list(table_header(true_visits))
              )
            }

            if (tab == nam) {
              cur_tr <- shiny::tags$tr(
                class = "row-group",
                shiny::tags$td(),
                shiny::tags$td(
                  tab,
                  colspan = ncol(tab_for_gt) - 2
                ),
                shiny::tags$td()
              )
            } else if (grepl("^Error|No values found", nam)) {
              cur_tr <- shiny::tags$tr(
                class = if (last_row) "last-row",
                shiny::tags$td(),
                shiny::tags$td(
                  shiny::HTML(text = nam_lab),
                  class = ifelse(
                    grepl("^Error", nam),
                    "error-message",
                    "no-values"
                  ),
                  colspan = ncol(tab_for_gt) - 3
                ),
                shiny::tags$td()
              )
            } else {
              obs <- tab_for_gt[x, ]
              obs[,
                c("method", "table", "name", "name_label") := list(
                  "",
                  "",
                  name_label,
                  NULL
                )
              ]

              # browser()

              ## Is this the first row in a group of rows?
              first_row_in_group <- (nam == "Age") ||
                (x > 2 && tab != tab_for_gt$table[x - 2])

              ## Is this the last row in a group of rows?
              last_row_in_group <- (x == nrow(tab_for_gt)) ||
                (tab != tab_for_gt$table[x + 1])

              cur_tr <- shiny::tags$tr(
                class = paste(
                  c(
                    if (last_row) "last-row",
                    if (first_row_in_group) "first-row-in-group",
                    if (last_row_in_group) "last-row-in-group"
                  ),
                  collapse = " "
                ),
                # class = if (last_row) "last-row",
                unname(purrr::imap(
                  obs,
                  \(y, idy) {
                    # if (x == 2 & idy == "2023-01-20") {
                    #   browser()
                    # }

                    if (!is.null(tab) && tab %in% names(densities)) {
                      cur_dens <- densities[[tab]][[paste(
                        nam,
                        "raw",
                        sep = "_"
                      )]]
                    } else {
                      cur_dens <- densities[[paste(nam, "raw", sep = "_")]]
                    }

                    if (!is.null(tab) && tab %in% names(cuts)) {
                      cur_cut <- cuts[[tab]][name == nam]
                      # [[paste(
                      #   nam,
                      #   "raw",
                      #   sep = "_"
                      # )]]
                    } else {
                      cur_cut <- cuts[name == nam] # [[paste(nam, "raw", sep = "_")]]
                    }

                    if (idy == "name") {
                      y <- shiny::HTML(y)
                    }

                    create_td(
                      y,
                      idy,
                      cell_id = paste(tab_id, x, idy, sep = "_"),
                      is_age = nam == "Age",
                      class = NULL,
                      true_visits,
                      prev_col = colnames(tab_for_gt)[
                        which(colnames(tab_for_gt) == idy) - 1
                      ],
                      next_col = colnames(tab_for_gt)[
                        which(colnames(tab_for_gt) == idy) + 1
                      ],
                      plot_title = list(
                        shiny::h5(nam, style = "font-weight: bold;"),
                        shiny::p(tab)
                      ),
                      # fmt: skip
                      cur_dens =  cur_dens, # if (tab %in% names(densities)) densities[[tab]][[paste(nam, "raw", sep = "_")]],
                      cur_cut = cur_cut # if (tab %in% names(cuts)) {
                      #   cuts[[tab]][name == nam, ]
                      # }
                    )
                  }
                ))
              )
            }

            shiny::tagList(
              subtable_header,
              cur_tr
            )
          })
        )
      )
    )
  )
}


#' Create a table cell (`<td>`) element
#'
#' Builds an HTML `<td>` element for a single cell in the biomarker table,
#' handling special rendering for different column types (name, method, info,
#' age, numeric with density plots, etc.).
#'
#' @param y Cell value.
#' @param idy Column name.
#' @param cell_id Unique identifier for this cell.
#' @param is_age Logical; whether this column represents an age value.
#' @param class CSS class string for the cell.
#' @param true_visits Character vector of column names that represent actual visits.
#' @param prev_col Previous column value (for border logic).
#' @param next_col Next column value (for border logic).
#' @param plot_title Optional plot title for density plots.
#' @param cur_dens Density data for this biomarker.
#' @param cur_cut Cut-off data for this biomarker.
#'
#' @returns A `shiny::tags$td` element.
#'
#' @keywords internal
create_td <- function(
  y,
  idy,
  cell_id,
  is_age,
  class,
  true_visits,
  prev_col,
  next_col,
  plot_title,
  cur_dens = densities[[tab]][[paste(
    nam,
    "raw",
    sep = "_"
  )]],
  cur_cut = all_cuts[[tab]][name == nam, ]
) {
  # if (cell_id == "4_2009-12-02") {
  #   browser()
  # }

  # To avoid notes in R CMD check
  densities <- NULL
  all_cuts <- NULL
  tab <- NULL
  nam <- NULL
  name <- NULL

  ## Check if this is an LP/PET visit by checking if age is NA
  true_visit <- idy %in%
    c("method", "table", "name", "info", "append", true_visits)

  if (!true_visit) {
    # fmt: skip
    prev_visit <- isTRUE(prev_col %in% c("method", "table", "name", "info", "append", true_visits))

    # fmt: skip
    next_visit <- isTRUE(next_col %in% c("method", "table", "name", "info", "append", true_visits))

    lowered_class <- if (!prev_visit & !next_visit) {
      "lowered-column center"
    } else if (!prev_visit) {
      "lowered-column right"
    } else if (!next_visit) {
      "lowered-column left"
    } else {
      "lowered-column"
    }

    return(
      shiny::tags$td(
        " ",
        class = paste(
          c(
            lowered_class,
            class
          ),
          collapse = " "
        )
      )
    )
  }

  if (idy %in% c("name", "table", "method", "append")) {
    return(
      shiny::tags$td(
        y,
        class = class,
        style = paste(
          "text-align: left;"
        )
      )
    )
  }

  if (idy == "info") {
    y1 <- y[[1]]

    if (is.null(y1) | (length(y1) == 1 && is.na(y1))) {
      return(shiny::tags$td())
    }

    return(
      shiny::tags$td(
        shiny::tags$span(
          class = "info-icon",
          style = "color: white; background-color: grey;",
          shiny::icon("info-sign", lib = "glyphicon"),
          # `data-toggle` = "tooltip",
          # `data-html` = 'true',
          `data-bs-toggle` = "tooltip",
          `data-bs-container` = "body",
          `data-bs-placement` = "right",
          `data-bs-html` = 'true',
          `data-bs-title` = shiny::HTML(paste(
            y[[1]]$description,
            "Click icon for reference.<br>",
            "<u>Thresholds applied:</u>",
            y[[1]]$thres,
            sep = "<br>"
          )),
          onclick = paste0("window.open('", y[[1]]$link, "', '_blank')")
        )
        # shiny::tags$span(
        #   class = "plot-icon",
        #   shiny::icon("info-circle"),
        #   `data-tooltip` = shiny::HTML(paste(
        #     y[[1]]$description,
        #     "Click for reference.",
        #     sep = "<br>"
        #   )),
        #   `data-html` = 'true',
        #   onclick = paste0("window.open('", y[[1]]$link, "', '_blank')")
        # )
      )
      # shiny::HTML(
      #   paste0(
      #     "<td><span class='plot-icon' data-tooltip='",
      #     y,
      #     "'><i class='fas fa-circle-info' role='presentation' aria-label='circle-info icon'></i></span></td>'"
      #   )
      # )
    )
  }

  if (is_age) {
    return(
      shiny::tags$td(
        y,
        class = class
      )
    )
  }

  if (is.null(unlist(y)) || is.na(y)) {
    return(
      shiny::tags$td(
        shiny::HTML("&mdash;"),
        class = class
        # class = paste(
        #   c(
        #     if (last_row) "last-row",
        #     if (first_row_in_group) "first-row-in-group",
        #     if (last_row_in_group) "last-row-in-group"
        #   ),
        #   collapse = " "
        # )
      )
    )
  }

  return(
    shiny::tags$td(
      class = paste(
        c(
          "flex-cell",
          class
        ),
        collapse = " "
      ),
      cell_content(
        cell = c(raw = y[[1]]$raw, y[[1]]$cat),
        cell_id = cell_id,
        plot_title,
        cur_dens,
        cur_cut
      )
    )
  )
}


#' Render cell content with icon, value, and density plot
#'
#' Creates the inner content of a biomarker table cell, including a status
#' icon, the formatted text value, and an optional inline density plot
#' triggered on click.
#'
#' @param cell A list with elements `icon`, `text`, and optionally `raw`.
#' @param cell_id Unique identifier for this cell.
#' @param plot_title Title for the density plot hover box.
#' @param dens Density data for the biomarker.
#' @param cur_cuts Cut-off thresholds for the biomarker.
#'
#' @returns A `shiny::tags$div` element.
#'
#' @keywords internal
cell_content <- function(
  cell,
  cell_id,
  plot_title,
  dens,
  cur_cuts
) {
  shiny::tags$div(
    class = "flex-cell-wrapper",
    shiny::tags$span(
      class = "flex-cell-left",
      if (!is.null(cell$icon)) shiny::HTML(cell$icon)
    ),
    shiny::tags$span(
      class = "flex-cell-center",
      shiny::HTML(gsub(
        pattern = "/Indeterminate",
        replacement = "/&#8203;Indeterminate",
        x = cell$text
      ))
    ),
    if (!is.null(cell$raw) && !is.na(cell$raw)) {
      # if (cell_id == "3_2023-05-03") {
      #   browser()
      # }

      shiny::tags$span(
        class = "flex-cell-right plot-icon",
        shiny::icon("chart-line"),
        shiny::tags$div(
          id = paste0("hoverBox_", cell_id),
          class = "hover-box",
          if (!missingArg(plot_title)) plot_title,
          density_plot(
            obs = cell$raw,
            dens = dens,
            cuts = cur_cuts,
            height = 200,
            width = 400,
            new_id = paste("density-plotly", cell_id, sep = "-")
          )
        ),
        `data-tooltip` = shiny::HTML(sprintf(
          "Raw value: %.2f (Click icon for graph.)",
          cell$raw
        )),
        onclick = sprintf(
          "toggleHoverBox(event, '%s')",
          cell_id
        )
      )
    } else {
      shiny::tags$span(
        class = "flex-cell-right",
        style = "width: 24px;"
      )
    }
  )
}
