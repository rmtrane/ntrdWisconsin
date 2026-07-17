# Tests for R/create_threshold_table.R
#
# Covers both exported functions defined in that file:
#   - count_decimal_places()
#   - create_thresholds_table()
#
# Both are pure (no API tokens required). Note that count_decimal_places()
# counts every digit remaining after stripping a leading "0." / "-0." prefix,
# so for values with a non-zero integer part it also counts the integer
# digits (e.g. 3.14 -> 3, 100 -> 3). This is intentional given its role:
# it is only ever applied to the sub-1 threshold cut-points, where the count
# equals the number of decimal places.


# ---------------------------------------------------------------------------
# count_decimal_places()
# ---------------------------------------------------------------------------

test_that("count_decimal_places() matches the documented examples", {
  expect_equal(count_decimal_places(3.14), 3)
  expect_equal(count_decimal_places(0.00025), 5)
})

test_that("count_decimal_places() counts decimals for sub-1 values", {
  expect_equal(count_decimal_places(0.05), 2)
  expect_equal(count_decimal_places(0.047), 3)
})

test_that("count_decimal_places() handles zero and integers", {
  expect_equal(count_decimal_places(0), 1)
  expect_equal(count_decimal_places(100), 3)
})

test_that("count_decimal_places() returns 0 for Inf", {
  # create_thresholds_table() relies on this: an open-ended max = Inf must
  # contribute no significant digits.
  expect_equal(count_decimal_places(Inf), 0)
})

test_that("count_decimal_places() ignores scientific notation", {
  # 2.5e-04 formats to 0.00025 before counting.
  expect_equal(count_decimal_places(2.5e-04), 5)
})


# ---------------------------------------------------------------------------
# create_thresholds_table()
# ---------------------------------------------------------------------------

# Documented example fixture.
thres <- data.frame(
  label = c("Negative", "Positive"),
  min   = c(0, 0.05),
  max   = c(0.05, Inf)
)

test_that("create_thresholds_table() returns a shiny HTML object", {
  res <- create_thresholds_table(thres)
  expect_s3_class(res, "html")
  expect_type(as.character(res), "character")
})

test_that("create_thresholds_table() produces the expected HTML for the documented example", {
  expected <- paste0(
    "<table>",
    "<tr><td>Negative</td><td>if</td><td></td>",
    "<td>value</td><td>&le; 0.05</td></tr>",
    "<tr><td>Positive</td><td>if</td><td>0.05 <</td>",
    "<td>value</td><td></td></tr>",
    "</table>"
  )
  expect_equal(as.character(create_thresholds_table(thres)), expected)
})

test_that("create_thresholds_table() renders one row per threshold with its label", {
  res <- as.character(create_thresholds_table(thres))
  expect_match(res, "Negative")
  expect_match(res, "Positive")
  # Two data rows.
  expect_equal(lengths(regmatches(res, gregexpr("<tr>", res))), 2L)
})

test_that("create_thresholds_table() leaves a zero minimum blank and an infinite maximum blank", {
  res <- as.character(create_thresholds_table(thres))
  # min == 0 -> empty lower-bound cell; max == Inf -> empty upper-bound cell.
  expect_match(res, "<td>Negative</td><td>if</td><td></td>", fixed = TRUE)
  expect_match(res, "<td>0.05 <</td><td>value</td><td></td>", fixed = TRUE)
})

test_that("create_thresholds_table() adapts the number of significant digits to the data", {
  fine <- data.frame(
    label = c("Neg", "Pos"),
    min   = c(0, 0.0005),
    max   = c(0.0005, Inf)
  )
  res <- as.character(create_thresholds_table(fine))
  # Finer cut-points must be formatted with four decimal places, not two.
  expect_match(res, "0.0005", fixed = TRUE)
})
