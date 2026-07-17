#' @include wadrc_source.R

data_load <- S7::new_external_generic("ntrd", "data_load", "source")

#' Load WADRC data
#'
#' @description
#' Pulls UDS-2, UDS-3, and UDS-4 data from the WADRC REDCap databases,
#' prepares each dataset, combines them, and computes derived neuropsych scores.
#'
#' @param source An object of class `wadrc_source`.
#' @param uds2_api_token A single string.
#' @param uds3_api_token A single string.
#' @param uds4_api_token A single string.
#' @param ... Currently unused; must be empty.
#'
#' @returns
#' An object of class `data_nacc` if data is successfully retrieved and combined.
#' If no data is retrieved from any UDS, `NULL` is returned and an error
#' notification is shown in a Shiny context. Notifications are displayed within a
#' Shiny application to indicate data loading progress and completion.
#'
#' @seealso [pull_redcap_data()], [wadrc_source()]
#'
#' @name data_load
#'
#' @keywords internal
S7::method(data_load, wadrc_source) <- function(
  source,
  uds2_api_token,
  uds3_api_token,
  uds4_api_token,
  ...
) {
  prepped_list <- Filter(
    Negate(is.null),
    list(
      pull_redcap_data(
        token = uds2_api_token,
        fields = wadrc_uds2_redcap_fields,
        uds = 2
      ),
      pull_redcap_data(
        token = uds3_api_token,
        fields = wadrc_uds3_redcap_fields,
        uds = 3
      ),
      pull_redcap_data(
        token = uds4_api_token,
        fields = wadrc_uds4_redcap_fields,
        uds = 4
      )
    )
  )

  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())

  if (length(prepped_list) == 0) {
    if (in_shiny) {
      shiny::showNotification("No data retrieved.", type = "error")
    }

    return(NULL)
  }

  if (in_shiny) {
    shiny::showNotification(
      "Combining data sets",
      duration = NULL,
      id = "combining"
    )
  }

  combined <- prepare_combined(prepped_list)

  if (in_shiny) {
    shiny::removeNotification(id = "combining")
    shiny::showNotification("REDCap data ready!", type = "message")
  }

  ntrd::data_nacc(data = combined)
}


#' Combine and prepare prepped WADRC UDS pulls
#'
#' @description
#' Combines the per-UDS prepared tables into a single NACC-format table: binds
#' them, fills participant-level fields across visits, drops rows with missing
#' `SEX`, tags neuropsych-score columns, and computes derived scores. Pure with
#' respect to Shiny (no UI side effects), so it can be tested directly on
#' simulated pulls without mocking the REDCap calls.
#'
#' @param prepped_list A non-empty list of prepared `data.table`s, each as
#'   returned by [pull_redcap_data()].
#'
#' @returns A `data.table` ready to be wrapped by [ntrd::data_nacc()].
#'
#' @keywords internal
prepare_combined <- function(prepped_list) {
  SEX <- NULL # due to NSE note in R CMD check

  combined <- unique(data.table::rbindlist(prepped_list, fill = TRUE))

  combined <- fill_data_downup(
    out = combined,
    ptid = "NACCID",
    visityr = "VISITYR",
    visitmo = "VISITMO",
    visitday = "VISITDAY",
    educ = "EDUC",
    constant_across_visits = c("BIRTHYR", "BIRTHMO", "SEX", "RACE", "HANDED")
  )

  ## Drop rows with missing SEX
  n_dropped <- sum(is.na(combined$SEX))
  if (n_dropped > 0) {
    cli::cli_warn("Dropping {n_dropped} row{?s} with missing SEX.")
  }
  combined <- combined[!is.na(SEX)]

  ## 'Tag' npsych_scores
  for (col in intersect(colnames(combined), ntrs::list_npsych_scores())) {
    combined[[col]] <- ntrs::get_npsych_scores(col)(as.numeric(combined[[col]]))
  }

  ## Derived scores
  combined$REYAREC <- with(combined, ntrs::calc_REYAREC(REYTCOR, REYFPOS))
  combined$FAS <- with(
    combined,
    ntrs::calc_FAS(
      BILLS,
      TAXES,
      SHOPPING,
      GAMES,
      STOVE,
      MEALPREP,
      EVENTS,
      PAYATTN,
      REMDATES,
      TRAVEL
    )
  )
  combined$MOCACLOCK <- with(
    combined,
    ntrs::calc_MOCACLOCK(MOCACLOC, MOCACLON, MOCACLOH)
  )
  combined$REYTOTAL <- with(
    combined,
    ntrs::calc_REYTOTAL(REY1REC, REY2REC, REY3REC, REY4REC, REY5REC)
  )

  combined
}


#' Pull REDCap data
#'
#' @description
#' Pulls data from a WADRC REDCap database using the provided API token,
#' then prepares it via [wadrc_data_prep()].
#'
#' @param token A single string, the REDCap API token.
#' @param fields A character vector of field names to pull.
#' @param uds A single string or number representing the UDS version.
#'
#' @returns
#' A prepared data table from `wadrc_data_prep()`. If the data pull fails,
#' it returns `NULL` and will show a notification in a Shiny app or emit a warning.
#'
#' @seealso [wadrc_source()], [wadrc_uds3_redcap_fields]
#'
#' @export
pull_redcap_data <- function(token, fields, uds) {
  # If shiny running, display notification
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::showNotification(
      paste0("Pulling data from REDCap UDS-", uds, " database..."),
      duration = NULL,
      type = "message",
      id = "pulling_from_redcap"
    )
  }
  res <- REDCapR::redcap_read_oneshot(
    redcap_uri = "https://redcap.medicine.wisc.edu/api/",
    token = token,
    fields = fields,
    guess_max = Inf
  )

  if (!res$success) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::removeNotification(id = "pulling_from_redcap")
      shiny::showNotification(
        paste0("Failed to pull UDS-", uds, " data from REDCap"),
        duration = NULL
      )
    } else {
      cli::cli_warn("Failed to pull UDS-{uds} data from REDCap")
    }
    return(NULL)
  }

  if (nrow(res$data) == 0) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::removeNotification(id = "pulling_from_redcap")
      shiny::showNotification(
        paste0("No data retrieved from REDCap UDS-", uds, " database"),
        duration = NULL
      )
    } else {
      cli::cli_warn("No data retrieved from REDCap UDS-{uds} database")
    }
    return(NULL)
  }

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::removeNotification(id = "pulling_from_redcap")
    shiny::showNotification(
      paste0("Preparing UDS-", uds, " data"),
      duration = NULL,
      id = "preparing_uds"
    )
  }

  out <- wadrc_data_prep(
    adrc_data = data.table::as.data.table(res$data),
    uds = paste0("uds", uds)
  )

  # Fix some discrepancies manually
  if (uds == 2) {
    out$TRAILARR[out$TRAILARR == 88] <- -4
    out$TRAILALI[out$TRAILALI == 88] <- -4
    out$TRAILBRR[out$TRAILBRR == 88] <- -4
    out$TRAILBLI[out$TRAILBLI == 88] <- -4
    out$MEMTIME[out$MEMTIME == 88] <- -4
  }

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::removeNotification(id = "preparing_uds")
  }

  out
}
