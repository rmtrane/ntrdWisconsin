# Tests for R/npsych_scores-CESDTOTAL.R
#
# CESDTOTAL is a custom npsych_scores subclass created via
# ntrs::new_npsych_scores() with label "CES-D Total Score", domain "Mood",
# and range c(0, 60) (no special codes). No API tokens required.

test_that("CESDTOTAL is an S7 class named 'CESDTOTAL'", {
  expect_true(inherits(CESDTOTAL, "S7_class"))
  expect_identical(CESDTOTAL@name, "CESDTOTAL")
})

test_that("a CESDTOTAL instance inherits from ntrs::npsych_scores", {
  expect_true(S7::S7_inherits(CESDTOTAL(10), ntrs::npsych_scores))
})

test_that("CESDTOTAL behaves as a numeric vector holding its scores", {
  x <- CESDTOTAL(c(5, 12, NA, 30))
  expect_equal(as.numeric(x), c(5, 12, NA, 30))
  expect_length(x, 4L)
})

test_that("CESDTOTAL carries the fixed label, domain, and range properties", {
  x <- CESDTOTAL(10)
  expect_identical(x@label, "CES-D Total Score")
  expect_identical(x@domain, "Mood")
  expect_equal(x@range, c(0, 60))
})

test_that("CESDTOTAL accepts in-range scores, the boundaries, NA, and empty input", {
  expect_no_error(CESDTOTAL(c(0, 30, 60, NA)))
  expect_no_error(CESDTOTAL())
})

test_that("CESDTOTAL rejects scores outside its 0-60 range", {
  local_reproducible_output()
  expect_error(CESDTOTAL(61), "range")
  expect_error(CESDTOTAL(-1), "range")
})
