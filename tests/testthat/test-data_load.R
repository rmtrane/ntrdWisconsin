# Covers R/data_load.R:
#   prepare_combined()    [pure helper]     -> combine/fill/drop/derive
#   data_load()           [S7 method]       -> thin wrapper: delegates to
#                                              load_wadrc_source()
#   load_wadrc_source()   [orchestration]   -> pulls -> filter NULLs ->
#                                              prepare_combined -> data_nacc,
#                                              plus Shiny notifications
#   ntrd::data_nacc()     [contract]        -> ntrdWisconsin output is a valid data_nacc
#   pull_redcap_data()    [network wrapper] -> Layer 1: REDCap call mocked (no token)
#                                              Layer 2: live, token-gated invariants
#
# Requires ntrd + ntrs loaded (test_check provides this).
#
# SKIPS:
#  * The frozen-fixture tests (end-to-end data_load() and the three
#    pull_redcap_data() snapshots) skip until data-raw/redcap_fixtures.R is run.
#  * The live Layer 2 tests skip unless REDCap tokens are set via
#    options(redcap_adrc_uds{2,3,4} = list(token = ...)).
#
# SNAPSHOTS: test names for the snapshot tests are unchanged from the previous
# version, so the existing _snaps/data_load.md entries still match.

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
# Helpers for the data_load() / load_wadrc_source() tests
# ---------------------------------------------------------------------------

# Mock pull_redcap_data(). `results` maps UDS version ("2", "3", "4") to what
# that pull returns; missing versions return NULL (a failed pull). Returns a
# function giving the recorded calls.
local_pulls <- function(results = list(), env = parent.frame()) {
  calls <- list()
  local_mocked_bindings(
    pull_redcap_data = function(token, fields, uds) {
      calls[[length(calls) + 1L]] <<- list(
        token = token,
        fields = fields,
        uds = uds
      )
      results[[as.character(uds)]]
    },
    .env = env
  )
  function() calls
}

# Mock shiny's notification functions and record every call, in order.
local_notifications <- function(env = parent.frame()) {
  log <- list()
  local_mocked_bindings(
    showNotification = function(
      ui,
      ...,
      duration = 5,
      id = NULL,
      type = "default"
    ) {
      log[[length(log) + 1L]] <<- list(
        fn = "show",
        ui = ui,
        duration = duration,
        id = id,
        type = type
      )
      invisible(id)
    },
    removeNotification = function(id, ...) {
      log[[length(log) + 1L]] <<- list(fn = "remove", id = id)
      invisible(NULL)
    },
    .package = "shiny",
    .env = env
  )
  function() log
}

# Evaluate `expr` as if inside a running Shiny session, so that
# getDefaultReactiveDomain() is non-NULL.
in_shiny <- function(expr) {
  shiny::withReactiveDomain(shiny::MockShinySession$new(), expr)
}

# ---------------------------------------------------------------------------
# data_load() method -- thin wrapper
# ---------------------------------------------------------------------------

test_that("data_load() delegates to load_wadrc_source() with each token", {
  seen <- NULL
  local_mocked_bindings(
    load_wadrc_source = function(
      uds2_api_token,
      uds3_api_token,
      uds4_api_token
    ) {
      seen <<- list(uds2_api_token, uds3_api_token, uds4_api_token)
      "sentinel"
    }
  )

  # Arguments given out of order, to check they're matched by name.
  res <- ntrd::data_load(
    wadrc_source(),
    uds4_api_token = "t4",
    uds2_api_token = "t2",
    uds3_api_token = "t3"
  )

  expect_identical(res, "sentinel")
  expect_identical(seen, list("t2", "t3", "t4"))
})

test_that("data_load() accepts and ignores the Panda key in `...`", {
  # ntrd calls do.call(data_load, c(source = source, params())), and
  # data_source_server's params() includes panda_api_token.
  local_mocked_bindings(load_wadrc_source = function(...) "sentinel")
  expect_identical(
    ntrd::data_load(
      wadrc_source(),
      uds2_api_token = "t2",
      uds3_api_token = "t3",
      uds4_api_token = "t4",
      panda_api_token = "pk"
    ),
    "sentinel"
  )
})

# ---------------------------------------------------------------------------
# load_wadrc_source() -- pulls and combining (outside Shiny)
# ---------------------------------------------------------------------------

test_that("load_wadrc_source() sends each token to its own UDS pull", {
  calls <- local_pulls()
  load_wadrc_source("t2", "t3", "t4")

  got <- calls()
  expect_length(got, 3L)
  expect_identical(vapply(got, `[[`, numeric(1), "uds"), c(2, 3, 4))
  expect_identical(
    vapply(got, `[[`, character(1), "token"),
    c("t2", "t3", "t4")
  )
  expect_identical(got[[1]]$fields, wadrc_uds2_redcap_fields)
  expect_identical(got[[2]]$fields, wadrc_uds3_redcap_fields)
  expect_identical(got[[3]]$fields, wadrc_uds4_redcap_fields)
})

test_that("load_wadrc_source() returns NULL quietly when every pull fails", {
  local_pulls()
  log <- local_notifications()

  expect_silent(res <- load_wadrc_source("t2", "t3", "t4"))
  expect_null(res)
  expect_length(log(), 0L)
})

test_that("load_wadrc_source() combines only the pulls that succeeded", {
  local_pulls(list(`3` = make_prepped_pull()))

  real_prepare <- prepare_combined
  got <- NULL
  local_mocked_bindings(
    prepare_combined = function(prepped_list) {
      got <<- prepped_list
      real_prepare(prepped_list)
    }
  )

  suppressWarnings(load_wadrc_source("t2", "t3", "t4"))
  expect_length(got, 1L)
  expect_equal(got[[1]], make_prepped_pull())
})

test_that("load_wadrc_source() skips prepare_combined() when nothing came back", {
  local_pulls()
  called <- FALSE
  local_mocked_bindings(prepare_combined = function(...) {
    called <<- TRUE
  })

  load_wadrc_source("t2", "t3", "t4")
  expect_false(called)
})

test_that("load_wadrc_source() returns a data_nacc when any one pull succeeds", {
  for (uds in c("2", "3", "4")) {
    local_pulls(setNames(list(make_prepped_pull()), uds))
    res <- suppressWarnings(load_wadrc_source("t2", "t3", "t4"))
    expect_s3_class(res, "ntrd::data_nacc")
  }
})

test_that("load_wadrc_source() returns a data_nacc when all pulls succeed", {
  local_pulls(list(
    `2` = make_prepped_pull(),
    `3` = make_prepped_pull(),
    `4` = make_prepped_pull()
  ))
  res <- suppressWarnings(load_wadrc_source("t2", "t3", "t4"))
  expect_s3_class(res, "ntrd::data_nacc")
})

test_that("load_wadrc_source() shows no notifications outside Shiny", {
  local_pulls(list(`2` = make_prepped_pull()))
  log <- local_notifications()

  suppressWarnings(load_wadrc_source("t2", "t3", "t4"))
  expect_length(log(), 0L)
})

# ---------------------------------------------------------------------------
# load_wadrc_source() -- notifications inside Shiny
# ---------------------------------------------------------------------------

test_that("inside Shiny, an empty result shows one error notification", {
  local_pulls()
  log <- local_notifications()

  res <- in_shiny(load_wadrc_source("t2", "t3", "t4"))

  expect_null(res)
  got <- log()
  expect_length(got, 1L)
  expect_identical(got[[1]]$fn, "show")
  expect_identical(got[[1]]$ui, "No data retrieved.")
  expect_identical(got[[1]]$type, "error")
})

test_that("inside Shiny, a successful load shows progress then success", {
  local_pulls(list(`2` = make_prepped_pull()))
  log <- local_notifications()

  res <- suppressWarnings(in_shiny(load_wadrc_source("t2", "t3", "t4")))
  expect_s3_class(res, "ntrd::data_nacc")

  got <- log()
  expect_identical(
    vapply(got, `[[`, character(1), "fn"),
    c("show", "remove", "show")
  )

  # 1. Persistent "combining" notice, addressable by id.
  expect_identical(got[[1]]$ui, "Combining data sets")
  expect_identical(got[[1]]$id, "combining")
  expect_null(got[[1]]$duration)

  # 2. That same notice is removed...
  expect_identical(got[[2]]$id, "combining")

  # 3. ...and replaced by a success message.
  expect_identical(got[[3]]$ui, "REDCap data ready!")
  expect_identical(got[[3]]$type, "message")
})

# ---------------------------------------------------------------------------
# End-to-end through the S7 method on frozen raw exports (integration)
# ---------------------------------------------------------------------------

test_that("data_load() turns frozen raw exports into a data_nacc", {
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
