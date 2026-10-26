# test-clean_biomarker_data.R  (plan item 10)
#
# Covers R/clean_biomarker_data.R -> clean_biomarker_data(as_df, table_name, query)
#
# STATUS: DRAFT — not yet executed. Expected values traced from source.
#
# NOTE: `query` is accepted but unused by the body -> passed as NULL.
# NOTE: relies on the package-internal `grepv()` (used in production code).
# NOTE: fixtures use post-prefix-strip column names (no "cg_*" prefixes).

# ---------------------------------------------------------------------------
# error-message branches
# ---------------------------------------------------------------------------

test_that("clean_biomarker_data() errors when no rows (visits unmatched)", {
  out <- clean_biomarker_data(make_biomarker_empty(), "csf", NULL)
  expect_s3_class(out, "error-message")
  expect_match(unclass(out), "visits could not be matched", ignore.case = TRUE)
})

test_that("clean_biomarker_data() errors when all biomarker values are NA", {
  out <- clean_biomarker_data(make_biomarker_all_na(), "csf", NULL)
  expect_s3_class(out, "error-message")
  expect_match(
    unclass(out),
    "no non-missing biomarker data",
    ignore.case = TRUE
  )
})

test_that("clean_biomarker_data() ignores id/date/age when checking for data", {
  fx <- make_biomarker_plasma()[, 4:10 := NA_character_]
  out <- clean_biomarker_data(fx, "plasma", NULL)
  expect_s3_class(out, "error-message")
})

# ---------------------------------------------------------------------------
# happy path (csf) — robust structural transforms
# ---------------------------------------------------------------------------

test_that("clean_biomarker_data() returns a cleaned data.table for valid csf input", {
  out <- clean_biomarker_data(make_biomarker_csf(), "csf", NULL)

  expect_false(inherits(out, "error-message"))
  expect_s3_class(out, "data.table")
})

test_that("clean_biomarker_data() converts date columns to Date and renames", {
  out <- clean_biomarker_data(make_biomarker_csf(), "csf", NULL)
  expect_true("date" %in% names(out))
  expect_s3_class(out$date, "Date")
  expect_true("age" %in% names(out))
})

test_that("clean_biomarker_data() maps status columns to factors with fixed levels", {
  out <- clean_biomarker_data(make_biomarker_csf(), "csf", NULL)
  # status_csf_lumi_ratio_fda -> csf_ratio_lumi_ab42_ab40_fda_cat (factor).
  col <- out$csf_ratio_lumi_ab42_ab40_fda_cat
  expect_s3_class(col, "factor")
  expect_identical(
    levels(col),
    c("Negative", "Likely Positive", "Positive", "Unavailable")
  )
  expect_identical(as.character(col), "Positive")
})

test_that("clean_biomarker_data() coerces numeric-looking raw columns to numeric", {
  out <- clean_biomarker_data(make_biomarker_csf(), "csf", NULL)
  # csf_ratio_lumi_ab42_ab40 -> csf_ratio_lumi_ab42_ab40_fda_raw (numeric).
  col <- out$csf_ratio_lumi_ab42_ab40_fda_raw
  expect_type(col, "double")
  expect_equal(col, 0.052)
})

# ---------------------------------------------------------------------------
# happy path (plasma)
# ---------------------------------------------------------------------------

test_that("clean_biomarker_data() renames plasma columns", {
  out <- clean_biomarker_data(make_biomarker_plasma(), "plasma", NULL)

  expect_s3_class(out, "data.table")
  expect_named(
    out,
    c(
      "enumber",
      "date",
      "age",
      "lumi_ptau217_over_ab42_fda_cat",
      "lumi_ptau217_over_ab42_fda_raw",
      "hdx_ptau217_ashton_cat",
      "hdx_ptau217_ashton_raw",
      "hdx_ptau217_local_cat",
      "hdx_ptau217_local_raw",
      "lumi_ptau217_local_cat",
      "lumi_ptau217_local_raw"
    ),
    ignore.order = TRUE
  )
  expect_s3_class(out$date, "Date")
  expect_equal(out$age, 68)
})

test_that("clean_biomarker_data() makes plasma status columns factors", {
  out <- clean_biomarker_data(make_biomarker_plasma(), "plasma", NULL)
  plasma_levels <- c("Negative", "Indeterminate", "Positive", "Unavailable")

  expected <- c(
    lumi_ptau217_over_ab42_fda_cat = "Negative",
    hdx_ptau217_ashton_cat = "Positive",
    hdx_ptau217_local_cat = "Indeterminate",
    lumi_ptau217_local_cat = "Unavailable"
  )
  for (col in names(expected)) {
    expect_s3_class(out[[col]], "factor")
    expect_identical(levels(out[[col]]), plasma_levels, label = col)
    expect_identical(as.character(out[[col]]), expected[[col]], label = col)
  }
})

test_that("clean_biomarker_data() makes plasma raw values numeric ('nan' -> NA)", {
  out <- clean_biomarker_data(make_biomarker_plasma(), "plasma", NULL)
  expect_equal(out$lumi_ptau217_over_ab42_fda_raw, 0.0071)
  expect_equal(out$hdx_ptau217_ashton_raw, 0.63)
  expect_identical(out$lumi_ptau217_local_raw, NA_real_)
})

test_that("clean_biomarker_data() copies HDX pTau217 to the local raw column", {
  out <- clean_biomarker_data(make_biomarker_plasma(), "plasma", NULL)
  expect_equal(out$hdx_ptau217_local_raw, out$hdx_ptau217_ashton_raw)
})

test_that("clean_biomarker_data() skips the HDX local copy without its status", {
  fx <- make_biomarker_plasma()[, status_plasma_hdx_ptau_local := NULL]
  out <- clean_biomarker_data(fx, "plasma", NULL)
  expect_false("hdx_ptau217_local_raw" %in% names(out))
})

# ---------------------------------------------------------------------------
# happy path (visual ratings)
# ---------------------------------------------------------------------------

test_that("clean_biomarker_data() renames visual-ratings columns", {
  out <- clean_biomarker_data(
    make_biomarker_visual_ratings(),
    "visual_ratings",
    NULL
  )

  expect_s3_class(out, "data.table")
  expect_named(
    out,
    c(
      "date",
      "age",
      "enumber",
      paste0("braak_", 1:6, "_cat"),
      "braak_comment_cat",
      "nav4694_visual_ratings_cat",
      "pib_visual_ratings_20180126_cat",
      "braak_positive_cat"
    ),
    ignore.order = TRUE
  )
  expect_s3_class(out$date, "Date")
  expect_equal(out$age, c(70, 71.5, 73))
})

test_that("clean_biomarker_data() recodes Braak ratings to 0/1 (invalid -> NA)", {
  out <- clean_biomarker_data(
    make_biomarker_visual_ratings(),
    "visual_ratings",
    NULL
  )
  expect_identical(out$braak_1_cat, c(1, 1, NA))
  expect_identical(out$braak_2_cat, c(1, 1, 0))
  expect_identical(out$braak_3_cat, c(0, 1, 0))
  expect_identical(out$braak_6_cat, c(0, 0, 0))
})

test_that("clean_biomarker_data() summarizes Braak stages per row", {
  out <- clean_biomarker_data(
    make_biomarker_visual_ratings(),
    "visual_ratings",
    NULL
  )
  expect_identical(
    out$braak_positive_cat,
    c("Borderline (MTL only)", "Elevated (MTL + Neocortical)", NA)
  )
})

test_that("clean_biomarker_data() keeps text ratings as character ('NA'/'nan' -> NA)", {
  out <- clean_biomarker_data(
    make_biomarker_visual_ratings(),
    "visual_ratings",
    NULL
  )
  expect_identical(out$braak_comment_cat, c("Tau in MTL", NA, NA))
  expect_identical(
    out$nav4694_visual_ratings_cat,
    c("Positive", "Negative", NA)
  )
  expect_identical(
    out$pib_visual_ratings_20180126_cat,
    c("Negative", "Positive", NA)
  )
})
