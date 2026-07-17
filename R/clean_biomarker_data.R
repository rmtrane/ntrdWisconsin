#' Clean biomarker data
#'
#' @description
#' Clean up biomarker data from Panda.
#'
#' @param as_df A data.table of biomarker data. Should be the result of `jsonlite::fromJSON` applied to the result of API call.
#' @param table_name A single string indicating the name of the table from which the data originated.
#' @param query List obtained from calling `jsonlite::fromJSON` on the API query
#'
#' @returns
#' A cleaned `data.table` of biomarker data. The function may return an
#' error message (a string with class `"error-message"`) if no visits
#' could be matched to dates or if no non-missing biomarker data is found.
#'
#' @export
clean_biomarker_data <- function(
  as_df,
  table_name,
  query
) {
  as_df <- data.table::copy(as_df)

  name <- NULL
  analyte <- NULL

  # fmt: skip
  levs <- list(
    status_csf_lumi_ratio_fda = c("Negative", "Likely Positive", "Positive", "Unavailable"),
    status_csf_roche_ratio_local = c("Negative", "Positive", "Unavailable"),
    status_csf_amprion_asyn = c("SAA-", "SAA+", "Unavailable"),
    status_plasma_hdx_ptau_local = c("Negative", "Indeterminate", "Positive", "Unavailable"),
    status_plasma_hdx_ptau_ashton = c("Negative", "Indeterminate", "Positive", "Unavailable"),
    status_plasma_lumi_ptau_local = c("Negative", "Indeterminate", "Positive", "Unavailable"),
    status_plasma_lumi_ratio_fda = c("Negative", "Indeterminate", "Positive", "Unavailable")
  )

  as_df[,
    names(.SD) := lapply(.SD, as.Date),
    .SDcols = grepv("date", names(as_df))
  ]

  ## Fix column names
  data.table::setnames(
    as_df,
    old = names(as_df),
    new = gsub(
      pattern = paste(
        c(
          "cg_csf_core1_status_",
          "cg_plasma_core1_status_",
          "view_cg_mk6240_braak_",
          "view_petscan_appts_",
          "_rating_0_1_2_3",
          "cg_"
        ),
        collapse = "|"
      ),
      replacement = "",
      x = names(as_df)
    ),
    skip_absent = TRUE
  )

  ## Fix binary variables to 0/1
  as_df[,
    names(.SD) := lapply(.SD, \(y) {
      y[y %in% c("TRUE", "true", "True", "Y")] <- 1
      y[y %in% c("FALSE", "false", "False", "N")] <- 0

      y[!y %in% c(0, 1)] <- NA

      as.numeric(y)
    }),
    .SDcols = grep(pattern = "_bin$|braak", x = names(as_df), value = T)
  ][,
    names(.SD) := purrr::imap(
      .SD,
      \(y, idy) {
        if (idy == "enumber") {
          return(y)
        }

        y[y %in% c("NA", "nan")] <- NA

        if (idy %in% names(levs)) {
          y <- factor(y, levels = levs[[idy]])

          return(y)
        }

        is_num <- all(grepl("^\\d*\\.?\\d*$", y) | is.na(y))

        if (is_num) {
          return(as.numeric(y))
        }

        y
      }
    ),
    .SDcols = \(z) !is.numeric(z) && !lubridate::is.Date(z)
  ]

  if (nrow(as_df) == 0) {
    out <- "Error: visits could not be matched to dates."

    class(out) <- "error-message"

    return(out)
  }

  for (i in colnames(as_df)) {
    if (paste0(i, "_bin") %in% colnames(as_df)) {
      colnames(as_df)[colnames(as_df) == i] <- paste0(i, "_raw")
    }
  }

  ##
  ## Table specific adjustments
  ##

  ## If CSF - Core 1 AD Status
  if (table_name == "csf") {
    # old = new
    renaming_vec <- c(
      "date_csf" = "date",
      "age_at_appointment" = "age",
      "status_csf_lumi_ratio_fda" = "csf_ratio_lumi_ab42_ab40_fda_cat",
      "status_csf_roche_ratio_local" = "csf_ratio_roche_ptau181_ab42_local_cat",
      "status_csf_amprion_asyn" = "csf_amprion_asyn_cat",
      "csf_ratio_lumi_ab42_ab40" = "csf_ratio_lumi_ab42_ab40_fda_raw",
      "csf_ratio_roche_ptau181_ab42" = "csf_ratio_roche_ptau181_ab42_local_raw"
    )
  }

  if (table_name == "plasma") {
    as_df$hdx_ptau217_local_raw <- as_df$plasma_ptau217_hdx

    renaming_vec <- c(
      # old = new
      c(
        "date_plasma" = "date",
        "age_at_appointment" = "age",
        "status_plasma_lumi_ratio_fda" = "lumi_ptau217_over_ab42_fda_cat",
        "plasma_ratio_lumi_ptau_ab42" = "lumi_ptau217_over_ab42_fda_raw",
        "status_plasma_hdx_ptau_ashton" = "hdx_ptau217_ashton_cat",
        "plasma_ptau217_hdx" = "hdx_ptau217_ashton_raw",
        "status_plasma_hdx_ptau_local" = "hdx_ptau217_local_cat",
        "status_plasma_lumi_ptau_local" = "lumi_ptau217_local_cat",
        "plasma_ptau217_lumi" = "lumi_ptau217_local_raw"
      )
    )
  }

  if (table_name == "visual_ratings") {
    # old = new
    renaming_vec <- c(
      "petscan_date" = "date",
      "age_at_appointment" = "age",
      "comment" = "comment_cat",
      "braak_1" = "braak_1_cat",
      "braak_2" = "braak_2_cat",
      "braak_3" = "braak_3_cat",
      "braak_4" = "braak_4_cat",
      "braak_5" = "braak_5_cat",
      "braak_6" = "braak_6_cat",
      "nav4694_visual_ratings" = "nav4694_visual_ratings_cat",
      "pib_visual_ratings_20180126" = "pib_visual_ratings_20180126_cat"
    )
  }

  data.table::setnames(
    as_df,
    old = names(renaming_vec),
    new = unname(renaming_vec),
    skip_absent = TRUE
  )

  if (
    all(is.na(
      as_df[,
        !grepl(pattern = "age_|_date", colnames(as_df)),
        with = F
      ]
    ))
  ) {
    out <- "Error: no non-missing biomarker data found."
    class(out) <- "error-message"

    return(out)
  }

  as_df
}
