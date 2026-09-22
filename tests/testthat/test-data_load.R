# test-data_load.R
#
# Covers R/data_load.R:
#   prepare_combined()  [pure helper]     -> combine/fill/drop/derive
#   data_load()         [S7 method]       -> pulls -> prepare_combined -> data_nacc
#   ntrd::data_nacc()   [contract]        -> ntrdWisconsin output is a valid data_nacc
#   pull_redcap_data()  [network wrapper] -> Layer 1: REDCap call mocked (no token)
#                                            Layer 2: live, token-gated invariants
#
# STATUS: DRAFT -- not yet executed. Requires ntrd + ntrs loaded (test_check
# provides this).
#
# FIRST-RUN CHECKS:
#  * The S7 class string is assumed to be "ntrd::data_nacc"; adjust if S7 names
#    it differently.
#  * data_nacc()'s exact required columns/types are validated inside ntrd -- if
#    construction errors, the message names the offending column and the fixture
#    gains one line (this is the point of the smoke test).
#  * The npsych-tagging + calc_* chain runs on made-up values; an error there is
#    almost certainly fixture coding, not the refactor.
#  * The fixture-based pull tests skip until data-raw/redcap_fixtures.R is run.

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

# test_that("data_load() wires pulls through to a data_nacc object", {
#   testthat::local_mocked_bindings(
#     pull_redcap_data = function(token, fields, uds) {
#       readRDS(test_path("sim-dat", paste0("redcap_uds", uds, ".Rds")))
#     }
#   )
#   res <- suppressWarnings(ntrd::data_load(
#     wadrc_source(),
#     uds2_api_token = "x",
#     uds3_api_token = "y",
#     uds4_api_token = "z"
#   ))

#   expect_s3_class(res, "ntrd::data_nacc")
# })

test_that("data_load() wires pulls through to a data_nacc object", {
  raw <- c(x = "uds2_raw.rds", y = "uds3_raw.rds", z = "uds4_raw.rds")
  paths <- test_path("fixtures", raw)
  skip_if_not(all(file.exists(paths)), "Run data-raw/redcap_fixtures.R first")

  local_mocked_bindings(
    redcap_read_oneshot = function(token, ...) {
      list(success = TRUE, data = readRDS(test_path("fixtures", raw[[token]])))
    },
    .package = "REDCapR"
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
# pull_redcap_data() -- Layer 1: REDCap call mocked, runs anywhere
# ---------------------------------------------------------------------------

# Stand-in for REDCapR::redcap_read_oneshot(). pull_redcap_data() only reads
# `$success` and `$data` from the result.
fake_read <- function(success = TRUE, data = data.frame(x = 1)) {
  function(...) list(success = success, data = data)
}

# Stand-in for wadrc_data_prep() output: the five columns the UDS-2 fix touches.
fake_prepped <- function() {
  data.table::data.table(
    TRAILARR = c(88, 1, NA),
    TRAILALI = c(88, 2, NA),
    TRAILBRR = c(88, 3, NA),
    TRAILBLI = c(88, 4, NA),
    MEMTIME = c(88, 5, NA)
  )
}

test_that("pull_redcap_data() warns and returns NULL when the pull fails", {
  local_reproducible_output()
  local_mocked_bindings(
    redcap_read_oneshot = fake_read(success = FALSE),
    .package = "REDCapR"
  )
  for (uds in 2:4) {
    expect_warning(
      out <- pull_redcap_data("tok", "f", uds),
      paste0("Failed to pull UDS-", uds)
    )
    expect_null(out)
  }
})

test_that("pull_redcap_data() warns and returns NULL on an empty pull", {
  local_reproducible_output()
  local_mocked_bindings(
    redcap_read_oneshot = fake_read(data = data.frame()),
    .package = "REDCapR"
  )
  for (uds in 2:4) {
    expect_warning(
      out <- pull_redcap_data("tok", "f", uds),
      paste0("No data retrieved from REDCap UDS-", uds)
    )
    expect_null(out)
  }
})

test_that("pull_redcap_data() sends token, fields and URI to REDCap", {
  seen <- NULL
  local_mocked_bindings(
    redcap_read_oneshot = function(...) {
      seen <<- list(...)
      list(success = FALSE)
    },
    .package = "REDCapR"
  )
  suppressWarnings(pull_redcap_data("tok", c("a", "b"), uds = 3))

  expect_identical(seen$token, "tok")
  expect_identical(seen$fields, c("a", "b"))
  expect_identical(seen$redcap_uri, "https://redcap.medicine.wisc.edu/api/")
})

test_that("pull_redcap_data() hands a data.table and 'udsN' to wadrc_data_prep()", {
  seen <- NULL
  local_mocked_bindings(redcap_read_oneshot = fake_read(), .package = "REDCapR")
  local_mocked_bindings(
    wadrc_data_prep = function(adrc_data, uds) {
      seen <<- list(adrc_data = adrc_data, uds = uds)
      data.table::data.table()
    }
  )
  pull_redcap_data("tok", "f", uds = 4)

  expect_s3_class(seen$adrc_data, "data.table")
  expect_identical(seen$uds, "uds4")
})

test_that("pull_redcap_data() recodes 88 -> -4 for UDS-2 only", {
  local_mocked_bindings(redcap_read_oneshot = fake_read(), .package = "REDCapR")
  local_mocked_bindings(wadrc_data_prep = function(...) fake_prepped())

  out2 <- pull_redcap_data("tok", "f", uds = 2)
  expect_true(all(unlist(out2[1]) == -4)) # every 88 recoded
  expect_equal(out2[2:3], fake_prepped()[2:3]) # other values and NAs untouched
  expect_equal(pull_redcap_data("tok", "f", uds = "2"), out2) # string uds works

  expect_equal(pull_redcap_data("tok", "f", uds = 3), fake_prepped())
  expect_equal(pull_redcap_data("tok", "f", uds = 4), fake_prepped())
})

# End-to-end on frozen raw exports (see data-raw/redcap_fixtures.R). The input
# never changes, so a snapshot diff here means the code changed.
for (uds in 2:4) {
  test_that(
    paste0("pull_redcap_data() prepares the frozen UDS-", uds, " fixture"),
    {
      path <- test_path("fixtures", paste0("uds", uds, "_raw.rds"))
      skip_if_not(file.exists(path), "Run data-raw/redcap_fixtures.R first")
      local_mocked_bindings(
        redcap_read_oneshot = fake_read(data = readRDS(path)),
        .package = "REDCapR"
      )

      out <- pull_redcap_data(
        "tok",
        get(paste0("wadrc_uds", uds, "_redcap_fields")),
        uds
      )

      expect_s3_class(out, "data.table")
      expect_true(all(
        c("NACCID", "VISITYR", "VISITMO", "VISITDAY") %in% names(out)
      ))
      expect_snapshot(out)
    }
  )
}

# ---------------------------------------------------------------------------
# pull_redcap_data() -- Layer 2: live, local only (needs API tokens)
# ---------------------------------------------------------------------------
#
# Asserts things that stay true as the database grows: schema and a row-count
# floor. Growth passes; lost data or column-type drift fails.

# TODO: set the UDS-3/4 floors a bit below today's counts; bump occasionally.
min_rows <- c(`2` = 1600, `3` = 1, `4` = 1)

for (uds in 2:4) {
  test_that(paste0("live UDS-", uds, " pull meets its contract"), {
    skip_on_cran()
    token <- getOption(paste0("redcap_adrc_uds", uds))$token
    skip_if(is.null(token), "No REDCap token set")

    out <- pull_redcap_data(
      token,
      get(paste0("wadrc_uds", uds, "_redcap_fields")),
      uds
    )

    expect_s3_class(out, "data.table")
    expect_gte(nrow(out), min_rows[[as.character(uds)]])
    expect_snapshot(vapply(out, function(x) class(x)[1], character(1)))
  })
}
