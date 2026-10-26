# test-simulate_data_table.R
#
# Covers R/simulate_data_table.R -> simulate_data_table()
#
# Pure: no tokens, no fixtures on disk. Calls go through sim(), which fixes the
# seed AND restores the caller's RNG state afterwards, because `seed =` calls
# set.seed() and would otherwise reset the global RNG for later tests.
#
# Code vs docs (the tests assert the code):
# - @return promises the same column types as `dat`, but `date_parts` columns
#   that are neither integer nor double (e.g. character) come back as integer.
# - "Rows with a missing id_col are dropped" is about the row count: values from
#   those rows still sit in each column's resampling pool.

# Three participants with 3/2/1 visits, plus one row with a missing id.
# yr/mo/dy always agree with `visit`, so a drawn triple can be matched back to
# a real visit date.
src <- function() {
  data.table::data.table(
    id = c("a", "a", "a", "b", "b", "c", NA),
    visit = as.Date(c(
      "2020-01-01",
      "2021-06-15",
      "2019-03-01",
      "2018-01-01",
      "2022-02-02",
      "2020-05-05",
      "2017-07-07"
    )),
    yr = c(2020L, 2021L, 2019L, 2018L, 2022L, 2020L, 2017L),
    mo = c(1L, 6L, 3L, 1L, 2L, 5L, 7L),
    dy = c(1L, 15L, 1L, 1L, 2L, 5L, 7L),
    sex = factor(
      c("F", "F", "F", "M", "M", "F", "M"),
      levels = c("F", "M", "X")
    ),
    educ = c(12, 12, 12, 16, 16, 20, 8),
    age = c(70L, 71L, 69L, 60L, 64L, 80L, 50L),
    score = c(1.5, NA, 3.2, 4.1, 2.2, 5.5, 0.1),
    flag = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, FALSE)
  )
}

sim <- function(..., seed = 1) {
  withr::local_preserve_seed()
  simulate_data_table(..., seed = seed)
}

n_unique_per_id <- function(out, col) {
  lengths(lapply(split(out[[col]], out$id), unique))
}

sorted_per_id <- function(x, id) {
  !any(vapply(split(x, id), is.unsorted, logical(1)))
}

# Years since each id's first visit (rows are sorted within id).
elapsed <- function(out) {
  d <- as.numeric(out$visit)
  (d - ave(d, out$id, FUN = min)) / 365.25
}

first_per_id <- function(x, id) ave(x, id, FUN = function(a) a[1])

# ---------------------------------------------------------------------------
# Argument checks
# ---------------------------------------------------------------------------

test_that("simulate_data_table() validates its arguments", {
  local_reproducible_output()
  d <- src()
  ymd <- c("yr", "mo", "dy")

  expect_error(sim(d), "`id_col` must be a single column name")
  expect_error(sim(d, c("id", "sex")), "`id_col` must be a single column name")
  expect_error(
    sim(d, "id", date_col = "visit", date_parts = ymd),
    "only one of `date_col` or `date_parts`"
  )
  expect_error(sim(d, "id", date_parts = ymd[1:2]), "length 3")

  expect_error(sim(d, "nope"), "`id_col` not found in `dat`: \"nope\"")
  expect_error(
    sim(d, "id", constant_cols = c("sex", "x")),
    "`constant_cols` not found.*\"x\""
  )
  expect_error(sim(d, "id", date_col = "x"), "`date_col` not found")
  expect_error(
    sim(d, "id", date_parts = c("yr", "mo", "x")),
    "`date_parts` not found"
  )
  expect_error(sim(d, "id", age_col = "x"), "`age_col` not found")
})

test_that("simulate_data_table() errors when every id is missing", {
  local_reproducible_output()
  d <- data.table::data.table(id = NA_character_, x = 1)
  expect_error(sim(d, "id"), "has no non-missing values")
})

test_that("a zero-row input returns an empty copy (after the argument checks)", {
  d <- src()[0]
  expect_equal(sim(d, "id"), d)
  expect_error(sim(d, "nope"), "not found")
})

# ---------------------------------------------------------------------------
# Shape and types
# ---------------------------------------------------------------------------

test_that("output keeps the input's names, order and column types", {
  d <- src()
  for (dates in list(
    list(date_col = "visit"),
    list(date_parts = c("yr", "mo", "dy"))
  )) {
    out <- do.call(
      sim,
      c(
        list(as.data.frame(d), "id", constant_cols = "sex", age_col = "age"),
        dates
      )
    )
    expect_s3_class(out, "data.table")
    expect_named(out, names(d))
    expect_identical(lapply(out, class), lapply(d, class))
    expect_identical(levels(out$sex), levels(d$sex))
  }
})

test_that("date_parts types: double stays double, character becomes integer", {
  d <- src()
  ymd <- c("yr", "mo", "dy")

  for (p in ymd) {
    d[[p]] <- as.double(d[[p]])
  }
  out <- sim(d, "id", date_parts = ymd)
  for (p in ymd) {
    expect_type(out[[p]], "double")
  }

  for (p in ymd) {
    d[[p]] <- as.character(d[[p]])
  }
  out <- sim(d, "id", date_parts = ymd)
  for (p in ymd) {
    expect_type(out[[p]], "integer")
  } # docs say same type as input
})

test_that("every non-id value is drawn from the same column of the input", {
  d <- src()
  out <- sim(d, "id", n_ids = 25)
  for (col in setdiff(names(d), "id")) {
    expect_true(all(out[[col]] %in% d[[col]]), label = col)
  }
})

# ---------------------------------------------------------------------------
# Ids and panel structure
# ---------------------------------------------------------------------------

test_that("ids are replaced by sequential synthetic ids; no real id survives", {
  d <- src()
  out <- sim(d, "id")
  expect_identical(unique(out$id), sprintf("sim%05d", 1:3))
  expect_false(any(out$id %in% d$id))
})

test_that("by default visit counts mirror the input exactly, minus missing-id rows", {
  d <- src()
  out <- sim(d, "id")
  expect_equal(nrow(out), 6L) # 7 rows, one with a missing id
  expect_identical(as.integer(table(out$id)), as.integer(table(d$id))) # 3, 2, 1
})

test_that("n_ids sets the participant count, resampling the real visit counts", {
  out <- sim(src(), "id", n_ids = 25)
  expect_identical(unique(out$id), sprintf("sim%05d", 1:25))
  expect_true(all(table(out$id) %in% 1:3))

  expect_identical(unique(sim(src(), "id", n_ids = 1)$id), "sim00001")
})

test_that("constant_cols are constant within id; other columns are not forced to be", {
  out <- sim(src(), "id", constant_cols = c("sex", "educ"), n_ids = 30)
  expect_true(all(n_unique_per_id(out, "sex") == 1L))
  expect_true(all(n_unique_per_id(out, "educ") == 1L))
  expect_true(any(n_unique_per_id(out, "score") > 1L))
})

# ---------------------------------------------------------------------------
# Visit dates
# ---------------------------------------------------------------------------

test_that("date_col is sorted ascending within each id", {
  out <- sim(src(), "id", date_col = "visit", n_ids = 30)
  expect_true(sorted_per_id(out$visit, out$id))
})

test_that("date_parts are drawn as one date, sorted within id, and split back", {
  d <- src()
  out <- sim(d, "id", date_parts = c("yr", "mo", "dy"), n_ids = 30)
  dates <- as.Date(sprintf("%04d-%02d-%02d", out$yr, out$mo, out$dy))

  expect_true(sorted_per_id(dates, out$id))
  # Each y/m/d triple is a real visit date, so the parts were drawn together.
  expect_true(all(dates %in% d$visit))
})

# ---------------------------------------------------------------------------
# Age
# ---------------------------------------------------------------------------

test_that("age is a per-id baseline plus the years since that id's first visit", {
  d <- src()
  d$age <- d$age + 0.3 # not whole numbers, so no rounding
  out <- sim(d, "id", date_col = "visit", age_col = "age", n_ids = 30)

  base <- first_per_id(out$age, out$id)
  expect_true(all(base %in% d$age))
  expect_equal(out$age, base + elapsed(out))
  expect_true(any(elapsed(out) > 1)) # the check above isn't vacuous
})

test_that("whole-number ages are rounded; integer stays integer, double stays double", {
  d <- src()
  out <- sim(d, "id", date_col = "visit", age_col = "age", n_ids = 30)
  expect_type(out$age, "integer")
  expect_equal(out$age, first_per_id(out$age, out$id) + round(elapsed(out)))

  d$age <- as.double(d$age)
  out <- sim(d, "id", date_col = "visit", age_col = "age", n_ids = 30)
  expect_type(out$age, "double")
  expect_equal(out$age, round(out$age))
})

test_that("without a date representation, age is constant within id", {
  out <- sim(src(), "id", age_col = "age", n_ids = 30)
  expect_true(all(n_unique_per_id(out, "age") == 1L))
})

# ---------------------------------------------------------------------------
# Reproducibility
# ---------------------------------------------------------------------------

test_that("seed makes the output reproducible", {
  d <- src()
  expect_identical(
    sim(d, "id", n_ids = 20, seed = 42),
    sim(d, "id", n_ids = 20, seed = 42)
  )
  expect_false(identical(
    sim(d, "id", n_ids = 20, seed = 1),
    sim(d, "id", n_ids = 20, seed = 2)
  ))
})

test_that("without seed, the draw follows the global RNG", {
  d <- src()
  a <- withr::with_seed(7, simulate_data_table(d, "id", n_ids = 20))
  b <- withr::with_seed(7, simulate_data_table(d, "id", n_ids = 20))
  expect_identical(a, b)
})
