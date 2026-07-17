# test-clean_biomarker_data.R  (plan item 10)
#
# Covers R/clean_biomarker_data.R -> clean_biomarker_data(as_df, table_name, query)
#
# STATUS: DRAFT — not yet executed. Error branches traced from source; happy-path
# assertions target the robust transformations only.
#
# NOTE: `query` is accepted but unused by the body -> passed as NULL.
# NOTE: relies on the package-internal `grepv()` (used in production code).

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
