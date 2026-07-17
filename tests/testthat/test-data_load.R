# test-data_load.R
#
# Covers the refactored R/data_load.R:
#   prepare_combined()  [pure helper]     -> combine/fill/drop/derive
#   data_load()         [S7 method]       -> pulls -> prepare_combined -> data_nacc
#   ntrd::data_nacc()   [contract]        -> ntrdWisconsin output is a valid data_nacc
#
# STATUS: DRAFT -- not yet executed. Requires ntrd + ntrs loaded (test_check
# provides this). The fixture carries every derived-score input column, so the
# `with(...)` derivations exercise rather than error.
#
# FIRST-RUN CHECKS:
#  * The S7 class string is assumed to be "ntrd::data_nacc"; adjust if S7 names
#    it differently.
#  * data_nacc()'s exact required columns/types are validated inside ntrd -- if
#    construction errors, the message names the offending column and the fixture
#    gains one line (this is the point of the smoke test).
#  * The npsych-tagging + calc_* chain runs on made-up values; an error there is
#    almost certainly fixture coding, not the refactor.

# ---------------------------------------------------------------------------
# prepare_combined() -- pure, no mocking needed
# ---------------------------------------------------------------------------

test_that("prepare_combined() drops all-NA-SEX rows and warns", {
  expect_warning(
    out <- prepare_combined(list(make_prepped_pull())),
    regexp = "missing SEX"
  )
  expect_s3_class(out, "data.table")
  expect_false("p2" %in% out$NACCID) # p2 had SEX NA on its only visit
  expect_true("p1" %in% out$NACCID)
})

test_that("prepare_combined() fills participant-level fields across visits", {
  out <- suppressWarnings(prepare_combined(list(make_prepped_pull())))
  p1 <- out[NACCID == "p1"]
  expect_true(all(!is.na(p1$SEX))) # SEX carried from visit 1 to visit 2
  expect_true(all(p1$SEX == 1))
  expect_true(all(p1$EDUC == 16)) # single EDUC copied across visits
})

test_that("prepare_combined() computes the derived neuropsych scores", {
  # Reaching this without error also proves every derivation input column is
  # present in a standard prepped pull.
  out <- suppressWarnings(prepare_combined(list(make_prepped_pull())))
  expect_true(all(c("REYAREC", "FAS", "MOCACLOCK", "REYTOTAL") %in% names(out)))
})

# ---------------------------------------------------------------------------
# data_load() -- mock the REDCap pulls
# ---------------------------------------------------------------------------

test_that("data_load() wires pulls through to a data_nacc object", {
  testthat::local_mocked_bindings(
    pull_redcap_data = function(token, fields, uds) {
      if (uds == 2) {
        readr::read_rds("sim-dat/redcap_uds2.Rds")
      }
      if (uds == 3) {
        readr::read_rds("sim-dat/redcap_uds3.Rds")
      }
      if (uds == 4) readr::read_rds("sim-dat/redcap_uds4.Rds")
    }
  )
  res <- suppressWarnings(ntrd::data_load(
    wadrc_source(),
    uds2_api_token = "x",
    uds3_api_token = "y",
    uds4_api_token = "z"
  ))

  expect_s3_class(res, "ntrd::data_nacc")
})

test_that("data_load() returns NULL when no data is retrieved", {
  testthat::local_mocked_bindings(
    pull_redcap_data = function(token, fields, uds) NULL
  )
  # The empty-path shows an (unconditional) notification; outside a Shiny
  # session that warns rather than errors. If it errors on your setup, wrap the
  # call in shiny::withReactiveDomain(shiny::MockShinySession$new(), ...).
  res <- suppressWarnings(ntrd::data_load(
    wadrc_source(),
    uds2_api_token = "x",
    uds3_api_token = "y",
    uds4_api_token = "z"
  ))
  expect_null(res)
})

# ---------------------------------------------------------------------------
# ntrd::data_nacc() -- contract between ntrdWisconsin and ntrd
# ---------------------------------------------------------------------------

test_that("prepare_combined() output constructs a valid ntrd::data_nacc", {
  out <- suppressWarnings(prepare_combined(list(make_prepped_pull())))
  expect_no_error(ntrd::data_nacc(data = out))
  nacc <- ntrd::data_nacc(data = out)
  expect_s3_class(nacc, "ntrd::data_nacc")
})

test_that("the NACC column contract is stable (review snapshot on first run)", {
  out <- suppressWarnings(prepare_combined(list(make_prepped_pull())))
  contract <- data.frame(
    column = names(out),
    class = vapply(out, function(x) class(x)[1], character(1)),
    row.names = NULL
  )
  contract <- contract[order(contract$column), ]
  expect_snapshot(print(contract, row.names = FALSE))
})

# ---------------------------------------------------------------------------
# pull_redcap_data: local only since API tokens are needed
# ---------------------------------------------------------------------------

test_that("pull_redcap_data works for UDS-2", {
  skip_if(is.null(getOption("redcap_adrc_uds2")))

  uds2_pull <- pull_redcap_data(
    getOption("redcap_adrc_uds2")$token,
    fields = wadrc_uds2_redcap_fields,
    uds = 2
  )[
    as.Date(paste(VISITYR, VISITMO, VISITDAY, sep = "-")) <
      as.Date("2026-07-17")
  ]

  scramble_uds2 <- simulate_data_table(
    uds2_pull,
    id_col = "NACCID",
    constant_cols = c("SEX", "EDUC", "RACE", "HANDED", "BIRTHYR", "BIRTHMO"),
    date_parts = c("VISITYR", "VISITMO", "VISITDAY"),
    seed = 1
  )

  expect_snapshot(scramble_uds2)
})

test_that("pull_redcap_data works for UDS-3", {
  skip_if(is.null(getOption("redcap_adrc_uds3")))

  uds3_pull <- pull_redcap_data(
    getOption("redcap_adrc_uds3")$token,
    fields = wadrc_uds3_redcap_fields,
    uds = 3
  )[
    as.Date(paste(VISITYR, VISITMO, VISITDAY, sep = "-")) <
      as.Date("2026-07-17")
  ]

  scramble_uds3 <- simulate_data_table(
    uds3_pull,
    id_col = "NACCID",
    constant_cols = c("SEX", "EDUC", "RACE", "HANDED", "BIRTHYR", "BIRTHMO"),
    date_parts = c("VISITYR", "VISITMO", "VISITDAY"),
    seed = 1
  )

  expect_snapshot(scramble_uds3)
})

test_that("pull_redcap_data works for UDS-4", {
  skip_if(is.null(getOption("redcap_adrc_uds4")))

  uds4_pull <- pull_redcap_data(
    getOption("redcap_adrc_uds4")$token,
    fields = wadrc_uds4_redcap_fields,
    uds = 4
  )[
    as.Date(paste(VISITYR, VISITMO, VISITDAY, sep = "-")) <
      as.Date("2026-07-17")
  ]

  scramble_uds4 <- simulate_data_table(
    uds4_pull,
    id_col = "NACCID",
    constant_cols = c("SEX", "EDUC", "RACE", "HANDED", "BIRTHYR", "BIRTHMO"),
    date_parts = c("VISITYR", "VISITMO", "VISITDAY"),
    seed = 1
  )

  expect_snapshot(scramble_uds4)
})
