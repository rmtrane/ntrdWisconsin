# test-wadrc_data_prep.R  (plan item 8)
#
# Covers R/wadrc_data_prep.R:
#   - return_single()      [pure helper]        -> full coverage
#   - fill_data_downup()   [internal transform] -> full coverage
#   - wadrc_data_prep()    [main transform]     -> abort only (see note)
#
# STATUS: DRAFT — not yet executed. Expected values traced from source, not run.
#
# NOTE (plan drift): the handoff lists "88 -> -4 recodes" under this item, but
# that logic lives in `pull_redcap_data()` (R/data_load.R, token-gated/deferred),
# NOT in wadrc_data_prep(). It is intentionally NOT tested here.
#
# NOTE (end-to-end deferred): the wadrc_data_prep() happy path (udsd/race/final
# column selection) requires a full-width UDS export as input — the uds2 path
# selects `out[, cols_wanted, with = FALSE]` over the entire nacc_to_wadrc_uds2
# field set, and fill_data_downup() requires an `educ` column. A minimal
# hand-built fixture cannot satisfy this, so a captured/synthetic .rds fixture is
# needed (see helper-fixtures.R). Left as a skipped placeholder below.

# ---------------------------------------------------------------------------
# return_single()
# ---------------------------------------------------------------------------

test_that("return_single() returns the single unique non-NA value", {
  expect_equal(unname(return_single(c(NA, 5, NA, 5))), 5)
  expect_equal(unname(return_single(c("a", NA, "a"))), "a")
})

test_that("return_single() returns NA when all values are NA", {
  expect_true(is.na(return_single(c(NA, NA))))
  expect_true(is.na(return_single(NA)))
})

test_that("return_single() returns if_multiple when >1 unique non-NA value", {
  # default if_multiple = NA
  expect_true(is.na(return_single(c(1, 2))))
  expect_equal(return_single(c(1, 2, 3), if_multiple = -99L), -99L)
})

test_that("return_single(if_multiple = NULL) returns x unchanged on conflict", {
  # The `!is.null(if_multiple)` branch: NULL falls through to `return(x)`.
  x <- c(1, 2, 3)
  expect_equal(return_single(x, if_multiple = NULL), x)
})

# ---------------------------------------------------------------------------
# fill_data_downup()
# ---------------------------------------------------------------------------

test_that("fill_data_downup() fills within/across visits and collapses dups", {
  out <- fill_data_downup(make_fill_fixture())

  # Rows 1 & 2 shared a visit and become identical after filling -> collapsed.
  expect_equal(nrow(out), 2L)

  # Sorted by ptid, visityr, visitmo, visitday: 2020 visit then 2021 visit.
  data.table::setorder(out, visityr, visitmo, visitday)

  # educ: single non-missing (16) copied to all rows.
  expect_equal(out$educ, c(16, 16))

  # sex: single non-missing (1) filled by ptid.
  expect_equal(out$sex, c(1, 1))

  # constant-by-ptid columns fully filled.
  expect_equal(out$race, c(1, 1))
  expect_equal(out$birthyr, c(1950, 1950))
  expect_equal(out$birthmo, c(3, 3))
  expect_equal(out$handed, c(2, 2))

  # within-visit numeric fill: first visit had {10, NA} -> 10; later visit NA.
  expect_equal(out$somescore, c(10, NA_real_))
})

test_that("fill_data_downup() scrubs SEX 8/9 before resolving a single value", {
  out <- fill_data_downup(make_sex_scrub_fixture())
  # 8 scrubbed to NA, leaving a single valid value (1) filled across visits.
  expect_true(all(out$sex == 1))
})

test_that("fill_data_downup() returns a data.table", {
  expect_s3_class(fill_data_downup(make_fill_fixture()), "data.table")
})

# ---------------------------------------------------------------------------
# wadrc_data_prep()
# ---------------------------------------------------------------------------

test_that("wadrc_data_prep() aborts when input is not a data.table", {
  withr::local_options(list(cli.num_colors = 1))
  local_reproducible_output()
  expect_error(
    wadrc_data_prep(data.frame(x = 1), uds = "uds2"),
    regexp = "data.table"
  )
})

test_that("wadrc_data_prep() end-to-end (needs full-width fixture)", {
  skip(
    "Requires a captured/synthetic full-width UDS export fixture; see helper-fixtures.R."
  )
  # Once tests/testthat/fixtures/uds2_min.rds exists:
  #   out <- wadrc_data_prep(readRDS(test_path("fixtures/uds2_min.rds")), uds = "uds2")
  #   expect_true(all(out$udsd %in% 1:4))
  #   ... (race fcase, column contract, etc.)
})
