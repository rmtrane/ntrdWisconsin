#' Format a table for gt
#'
#' @description
#' Takes a table as those in the entries of the list provided by `get_biomarker_data`,
#' and formats this for "pretty printing" using `gt::gt`.
#'
#' @param tab A `data.table`.
#'
#' @returns
#' A `data.table` with modified column names and values, formatted for use with `gt::gt`.
#' Returns `NULL` if the input `tab` is `NULL`.
#'
#' @keywords internal
bio_tab_for_gt <- function(
  tab
) {
  if (inherits(tab, "try-error")) {
    return(tab)
  }

  no_val_dt <- data.table::data.table(
    name = "No values found",
    name_label = "No values found"
  )

  if (is.null(tab)) {
    return(no_val_dt)
  }

  # Remove missing dates
  tab <- tab[!is.na(date)]

  if (nrow(tab) == 0) {
    return(no_val_dt)
  }

  if (inherits(tab, "error-message")) {
    return(data.table::data.table(name = unclass(tab)))
  }

  if (!inherits(tab, "data.table")) {
    cli::cli_abort(
      "{.arg tab} must be of class {.cls data.table}, but is of class {.cls {class(tab)}}"
    )
  }

  tab <- data.table::copy(tab)

  # Rename age column
  names(tab)[which(names(tab) == "age")] <- "Age_raw"

  # To avoid "no visible binding for global variable" in devtools::check()
  name <- value.name <- bin <- variable <- value <- NULL

  if (!lubridate::is.Date(tab$date)) {
    tab[, date := as.Date(date)]
  }

  tab[,
    names(.SD) := lapply(.SD, as.character),
    .SDcols = grepv("_cat$", names(tab))
  ]

  if (!"date" %in% colnames(tab)) {
    tab$date <- paste("Visit", seq_along(tab$age_raw))
  }

  ## In rare cases, entries might be present with no raw or bin values. We remove these
  rows_include <- rowSums(
    !(tab[,
      lapply(.SD, is.na),
      .SDcols = colnames(tab)[
        grepl("_raw$|_cat$", colnames(tab)) & !grepl("Age", colnames(tab))
      ]
    ])
  ) >
    0

  tab <- data.table::melt(
    tab[rows_include],
    id.vars = c("date"),
    measure.vars = data.table::measure(
      name,
      value.name,
      pattern = "(.*)_(raw|cat)"
    )
  )

  tab[,
    c("cat", "name_label") := list(
      html_cat(cat, name),
      data.table::fcase(
        name == "csf_amprion_asyn"                   , "Amprion aSyn SAA"                              ,
        name == "csf_ratio_lumi_ab42_ab40_fda"       , "Fujirebio Lumipulse A&beta;42/A&beta;40 (FDA)" ,
        name == "csf_ratio_roche_ptau181_ab42_local" , "Roche pTau181/A&beta;42 (local)"               ,
        name == "hdx_ptau217_ashton"                 , "Quanterix HDX pTau217 (Ashton et al.)"         ,
        name == "hdx_ptau217_local"                  , "Quanterix HDX pTau217 (local)"                 ,
        name == "lumi_ptau217_local"                 , "Fujirebio Lumipulse pTau217 (local)"           ,
        name == "lumi_ptau217_over_ab42_fda"         , "Fujirebio Lumipulse pTau217/A&beta;42 (FDA)"   ,
        name == "braak_1"                            , "Stage I"                                       ,
        name == "braak_2"                            , "Stage II"                                      ,
        name == "braak_3"                            , "Stage III"                                     ,
        name == "braak_4"                            , "Stage IV"                                      ,
        name == "braak_5"                            , "Stage V"                                       ,
        name == "braak_6"                            , "Stage VI"                                      ,
        name == "comment"                            , "Braak Comment"                                 ,
        name == "nav4694_visual_ratings"             , "NAV4694 Scan Rating"                           ,
        name == "pib_visual_ratings_20180126"        , "PiB Visual Rating"                             ,
        default = name
      )
    )
  ]

  tab[,
    c("name", "values", "raw", "cat") := list(
      # list(
      name = factor(name, levels = sort(unique(name))),
      values = mapply(
        raw,
        cat,
        FUN = \(x, y) {
          c(
            if (!is.na(x)) list(raw = x),
            list(cat = y)
          )
        },
        SIMPLIFY = FALSE
      ),
      NULL,
      NULL
    )
  ]

  tab <- data.table::dcast(
    tab,
    name + name_label ~ date,
    value.var = "values"
  )

  tab$name <- as.character(tab$name)

  tab
}


html_cat <- function(x, name) {
  negative_html <- list(
    icon = '<i class="glyphicon glyphicon-minus-sign" style="color:green;"></i>',
    text = 'Negative'
  )

  positive_html <- list(
    icon = '<i class="glyphicon glyphicon-plus-sign" style="color:red;"></i>',
    text = 'Positive'
  )

  likely_positive_html <- list(
    icon = '<i class="glyphicon glyphicon-plus-sign" style="color:#ffa9a9;"></i>',
    text = 'Likely Positive'
  )

  data.table::fcase(
    name == "comment" & !is.na(x) & x != ""       , as.list(x)                                                               ,
    grepl("braak", name) & x == 0                 , list(list(icon = negative_html$icon, text = "Clearly negative (0)"))     ,
    grepl("braak", name) & x == 1                 , list(list(icon = negative_html$icon, text = "Clearly negative (1)"))     ,
    grepl("braak", name) & x == 2                 , list(list(text = "Ambiguous/Indeterminate"))                             ,
    grepl("braak", name) & x == 3                 , list(positive_html)                                                      ,
    grepl("nav4694_visual_rating", name) & x == 0 , list(negative_html)                                                      ,
    grepl("nav4694_visual_rating", name) & x == 1 , list(positive_html)                                                      ,
    grepl("pib_visual_ratings", name) & x == 0    , list(list(icon = negative_html$icon, text = "Clearly PiB negative (0)")) ,
    grepl("pib_visual_ratings", name) & x == 1    , list(list(icon = negative_html$icon, text = "Clearly PiB negative (1)")) ,
    grepl("pib_visual_ratings", name) & x == 2    , list(list(text = "Ambiguous/Indeterminate"))                             ,
    grepl("pib_visual_ratings", name) & x == 3    , list(list(icon = positive_html$icon, text = "PiB+"))                     ,
    x %in% c("Positive", "SAA+")                  , list(positive_html)                                                      ,
    x %in% c("Negative", "SAA-")                  , list(negative_html)                                                      ,
    x == "Indeterminate"                          , list(list(text = "Indeterminate"))                                       ,
    x == "Unavailable"                            , list(list(text = "Unavailable"))                                         ,
    x == "Likely Positive"                        , list(likely_positive_html)                                               ,
    default = list(list())
  )

  # if (all(x %in% 0:5)) {
  #   out <- list(
  #     "0" = list(
  #       icon = negative_html$icon,
  #       text = "Clearly negative (0)"
  #     ),
  #     "1" = list(
  #       icon = negative_html$icon,
  #       text = "Clearly negative (1)"
  #     ),
  #     "2" = list(
  #       text = 'Ambiguous/Indeterminate'
  #     ),
  #     "3" = positive_html,
  #     "5" = list()
  #   )[as.character(x)]
  # }

  # if (x %in% c("Positive", "SAA+")) {
  #   return(positive_html)
  # }
  # if (x %in% c("Negative", "SAA-")) {
  #   return(negative_html)
  # }
  # if (x == "Likely Positive") {
  #   return(likely_positive_html)
  # }

  # NA
}

#' Transform a table to a gt object
#'
#' @description
#' Converts a formatted biomarker `data.table` (or list of `data.table`s) into
#' a styled [gt::gt()] table, using list names as row groups when applicable.
#'
#' @param tab_for_gt A `data.table` or a `list` of `data.table`s.
#'
#' @returns
#' A `gt::gt` table. If `tab_for_gt` is a list, the names of the list are used as
#' grouping variable for table.
#'
#' @keywords internal
bio_tab_to_gt <- function(tab_for_gt) {
  if (is.list(tab_for_gt) & !inherits(tab_for_gt, "data.table")) {
    tab_for_gt <- tab_for_gt[!unlist(lapply(tab_for_gt, is.null))]

    tab_for_gt <- lapply(tab_for_gt, \(x) {
      if (inherits(x, "try-error")) {
        x <- data.table::data.table(
          name = x[1]
        )
      }

      x
    })

    if (!all(sapply(tab_for_gt, data.table::is.data.table))) {
      non_dts <- tab_for_gt[!sapply(tab_for_gt, data.table::is.data.table)]

      cli::cli_abort(
        "When {.arg tab_for_gt} is of class {.cls list}, all elements must be of class {.cls data.table}, but {.val {names(non_dts)}} {?is/are} of class {.cls {unlist(sapply(non_dts, class))}}"
      )
    }

    tab_for_gt <- data.table::rbindlist(
      tab_for_gt,
      fill = TRUE,
      idcol = "table"
    )

    visit_dates <- setdiff(names(tab_for_gt), c("name", "table"))

    tab_for_gt <- tab_for_gt[, c("table", "name", sort(visit_dates)), with = F]
  }

  group <- if ("table" %in% names(tab_for_gt)) "table"

  if (!is.null(group)) {
    age_rows <- tab_for_gt[tab_for_gt$name == "Age"]

    age_rows$table <- NULL
    age_rows <- unique(age_rows[,
      c("table", names(.SD)) := c(
        "",
        lapply(.SD, \(x) unique(na.omit(x)))
      )
    ])

    tab_for_gt <- data.table::rbindlist(
      list(age_rows, tab_for_gt[tab_for_gt$name != "Age"]),
      fill = TRUE
    )
  }

  gt::gt(
    id = "biomarker-table",
    tab_for_gt,
    rowname_col = "name",
    groupname_col = group
  ) |>
    gt::tab_stub_indent(
      rows = 1:nrow(tab_for_gt),
      indent = if (!is.null(group)) 4
    ) |>
    gt::fmt(
      fns = \(x) sapply(x, gt::html),
      rows = tab_for_gt$name != "Age"
    ) |>
    gt::fmt(
      fns = \(x) paste(floor(as.numeric(x)), "years"),
      rows = tab_for_gt$name == "Age",
      columns = -"name"
    ) |>
    gt::cols_align(align = "left") |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "right",
          style = "hidden"
        ),
        gt::cell_text(align = "left")
      ),
      locations = gt::cells_stub()
    ) |>
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::sub_missing()
}


#' Fetch all biomarker values from Panda
#'
#' Queries the Panda API for all biomarker data across all tables (excluding
#' Participants, Appointments, and Visual Rating tables). Used to compute
#' population-level density estimates and threshold cut-offs.
#'
#' @param api_key A single string. Panda API key.
#' @param base_query_file Path to the JSON query template file.
#' @param adrc_ptids Vector of ID's of ADRC participants for which biomarker
#'   data should be pulled. If NULL (default), data for all participants are
#'   pulled.
#'
#' @returns A named list of `data.table`s, one per biomarker table.
#'
#' @keywords internal
get_biomarker_data <- function(
  api_key = getOption("panda_api_key"),
  base_query_file = "inst/json/plasma.json",
  #   system.file(
  #   "json/csf.json",
  #   package = "ntrdWisconsin"
  # ),
  adrc_ptids = NULL
) {
  # To avoid notes in R CMD check
  name <- NULL

  base_query <- readLines(base_query_file) |>
    jsonlite::fromJSON()

  if (!is.null(adrc_ptids)) {
    all_tabs <- base_query$query$tables

    if (length(adrc_ptids) == 1) {
      oper <- "="
      vals <- paste0("\"'", adrc_ptids, "'\"")
    } else {
      oper <- "in"
      vals <- paste0("\"", adrc_ptids, "\"", collapse = ",")
    }

    json_text <- paste0('[{"operator":"', oper, '","values":[', vals, ']}]')

    all_tabs[all_tabs$name == "Enrollments", ]$columns[[1]]$constraints[[
      1
    ]] <- jsonlite::fromJSON(json_text)

    all_tabs[all_tabs$name == "Enrollments", "join"] <- "inner"

    base_query$query$tables <- all_tabs
  }

  base_request <- httr2::request(
    base_url = 'https://panda.medicine.wisc.edu/api/search/search'
  ) |>
    # Next, add authorization piece (this is where the API key is needed)
    httr2::req_headers(
      Authorization = paste("Bearer", api_key)
    ) |>
    # Finally, specify request method
    httr2::req_method("POST")

  cur_req <- base_request |>
    httr2::req_body_json(
      data = base_query
    )

  resp <- try(httr2::req_perform(cur_req), TRUE)

  if (inherits(resp, "try-error")) {
    return(resp)
  }

  if (resp$status != 200) {
    return(resp$status)
  }

  resp <- httr2::resp_body_json(resp)$data

  resp <- jsonlite::fromJSON(resp) |>
    data.table::as.data.table() |>
    _[grepl("^adrc", enumber, ignore.case = TRUE)]

  if (any(grepl("csf", base_query$query$tables$name, ignore.case = TRUE))) {
    table_nam <- "csf"
  }

  if (any(grepl("plasma", base_query$query$tables$name, ignore.case = TRUE))) {
    table_nam <- "plasma"
  }

  # fmt: skip
  if (any(grepl("visual ratings", base_query$query$tables$name, ignore.case = TRUE))) {
    table_nam <- "visual_ratings"
  }

  # resp

  clean_biomarker_data(
    as_df = resp,
    table_name = table_nam,
    query = base_query
  )
}


#' Extract cut-off thresholds from biomarker data
#'
#' For biomarkers with predefined thresholds in [biomarker_thresholds], those
#' are used. Otherwise, thresholds are inferred from the data by finding
#' boundaries between bin categories.
#'
#' @param all_values A named list of `data.table`s as returned by `get_all_values()`.
#'
#' @returns A named list of `data.table`s with columns `name`, `bin`, `color`,
#'   `min_obs`, and `max_obs`.
#'
#' @keywords internal
get_all_cuts <- function(all_values) {
  # To avoid notes in R CMD check
  name <- NULL
  value.name <- NULL
  bin <- NULL
  min_obs <- NULL
  max_obs <- NULL

  purrr::imap(all_values, \(x, idx) {
    if (idx == "Amprion - CSF a-Synuclein") {
      return()
    }

    ## If we know the thresholds, use these.
    if (idx %in% names(biomarker_thresholds)) {
      out <- lapply(
        biomarker_thresholds[[idx]],
        \(y) {
          y$thresholds
        }
      ) |>
        data.table::rbindlist(
          use.names = T,
          idcol = "name"
        )

      out <- out[,
        list(
          name = name,
          bin = bin,
          # fmt: skip
          color = data.table::fcase(
          bin == 0, "rgba(0, 100, 0, alpha)",
          bin == 1, "rgba(139, 0, 0, alpha)",
          bin == 0.5, "rgba(216, 216, 216, 0.5)"
        ),
          min_obs = min,
          max_obs = max
        )
      ]

      return(out)
    }

    ## Otherwise, we infer from data.

    ## If there are no raw scores, abort
    if (!any(grepl("_raw$", names(x)))) {
      return()
    }

    if (any(grepl("_cat$", colnames(x)))) {
      colnames(x) <- gsub("_cat", "_bin", colnames(x))
    }

    data.table::melt(
      x,
      measure.vars = data.table::measure(
        name,
        value.name,
        pattern = "(.*)_(raw|bin)"
      )
    )[
      !is.na(bin),
      list(
        # cut = ifelse(bin == 0, max(raw, na.rm = T), min(raw, na.rm = T))
        # .N,
        min_obs = min(raw, na.rm = T),
        max_obs = max(raw, na.rm = T) #,
        # range = list(range(raw, na.rm = T))
      ),
      by = c("name", "bin")
    ][
      order(name, min_obs)
    ][,
      list(
        bin = bin / max(bin),
        min_obs = ((min_obs + data.table::shift(max_obs)) / 2) |>
          data.table::nafill(type = "const", fill = 0),
        max_obs = ((data.table::shift(min_obs, n = -1) + max_obs) / 2) |>
          data.table::nafill(type = "const", fill = Inf)
      ),
      by = "name"
    ][,
      list(
        name = name,
        bin = bin,
        # fmt: skip
        color = data.table::fcase(
          bin == 0, "rgba(0, 100, 0, alpha)",
          bin == 1, "rgba(139, 0, 0, alpha)",
          bin == 0.5, "rgba(216, 216, 216, 0.5)"
        ),
        min_obs = min_obs,
        max_obs = max_obs
      )
    ]
  })
}


#' Compute kernel density estimates for all biomarkers
#'
#' Calculates Gaussian kernel density estimates for each raw biomarker column,
#' using Sheather-Jones bandwidth selection.
#'
#' @param all_values A named list of `data.table`s as returned by `get_biomarker_data()`.
#'
#' @returns A named list of density objects, nested by table and biomarker name.
#'
#' @keywords internal
get_all_densities <- function(x) {
  if (is.null(x) | nrow(x) == 0 | inherits(x, "try-error")) {
    return(NULL)
  }

  x <- x[, grepv(pattern = "_raw$", names(x)), with = F]

  if (ncol(x) == 0) {
    return(NULL)
  }

  dens <- purrr::imap(x, \(y, idy) {
    if (!grepl(pattern = "_raw$", idy)) {
      return(NULL)
    }

    y <- na.omit(y)

    density(
      y,
      from = 0,
      kernel = "gaussian",
      bw = "SJ-ste",
      adjust = ifelse(length(y) > 500, 1, 1.5),
      na.rm = T
    )
  })

  dens[!unlist(lapply(dens, is.null))]
  # }) |>
  #   purrr::discard(purrr::is_null)
}


#' Check Panda API connectivity
#'
#' Makes a lightweight test request to the Panda API to verify that the
#' server is reachable and the API key is valid.
#'
#' @param api_key A single string. Panda API key.
#' @param timeout Timeout in seconds for the test request.
#'
#' @returns A list with `connected` (logical), `status` (HTTP status code or
#'   `NA`), and `message` (character).
#'
#' @keywords internal
check_connection <- function(
  api_key = getOption("panda_api_key"),
  timeout = 5
) {
  # Try to make a simple request to the URL
  tryCatch(
    {
      response <- httr2::request(
        base_url = 'https://panda.medicine.wisc.edu/api/search/search'
      ) |>
        httr2::req_timeout(timeout) |>
        # Next, add authorization piece (this is where the API key is needed)
        httr2::req_headers(
          Authorization = paste("Bearer", api_key)
        ) |>
        # Finally, specify request method
        httr2::req_method("POST") |>
        httr2::req_body_json(
          jsonlite::fromJSON(
            '{"query":{"tables":[{"name":"Participants","id":679,"class":"cg_tn","join":"left","columns":[{"name":"Adrc Number","id":690,"class":"cg_tn_cn","show":true,"constraints":[{"operator":"=","values":["\'adrc00001\'"]}]}]}],"offset":0,"limit":-1,"format":"csv","row_format":"participant_centric","name":null,"save":null}}'
          )
        ) |>
        httr2::req_perform()

      # If we get here, connection succeeded
      return(list(
        connected = TRUE,
        status = httr2::resp_status(response),
        message = "Connection successful"
      ))
    },
    error = function(e) {
      # Connection failed
      return(list(
        connected = FALSE,
        status = NA,
        message = paste("Connection failed:", e$message)
      ))
    }
  )
}
