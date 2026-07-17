# Tests for R/biomarker_ratios.R
#
# Covers the three exported functions defined in that file:
#   - lumipulse_pTau217_ABeta42_ratio()
#   - lumipulse_ABeta42_ABeta40_ratio()
#   - categorize_ratio()
#
# All three are pure and deterministic (no API tokens required). Expected
# values follow the value-capping / NA rules taken from the FDA review
# documents referenced in the function documentation.


# ---------------------------------------------------------------------------
# lumipulse_pTau217_ABeta42_ratio()
# ---------------------------------------------------------------------------

test_that("lumipulse_pTau217_ABeta42_ratio() matches the documented example", {
  # Element 1: 1.5 / 300                              -> 0.005
  # Element 2: pTau < 0.047 & ABeta42 >= 12.8         -> floored to 0.047
  # Element 3: pTau >= 500 & ABeta42 > 1.85           -> NA
  expect_equal(
    lumipulse_pTau217_ABeta42_ratio(
      pTau217 = c(1.5, 0.03, 600),
      ABeta42 = c(300, 50, 2)
    ),
    c(0.005, 0.047, NA)
  )
})

test_that("lumipulse_pTau217_ABeta42_ratio() caps pTau217 at 10000", {
  # ABeta42 <= 1.85 keeps us out of the NA branch, so the cap is observable.
  # Uncapped this would be 15000 / 1 = 15000; capped it is 10000 / 1 = 10000.
  expect_equal(
    lumipulse_pTau217_ABeta42_ratio(pTau217 = 15000, ABeta42 = 1),
    10000
  )
})

test_that("lumipulse_pTau217_ABeta42_ratio() caps ABeta42 at 500 when pTau217 <= 1.85", {
  # ABeta42 >= 500 & pTau217 <= 1.85 -> ABeta42 replaced by 500.
  # Capped: 1.0 / 500 = 0.002 (vs uncapped 1.0 / 600 = 0.001667).
  expect_equal(
    lumipulse_pTau217_ABeta42_ratio(pTau217 = 1.0, ABeta42 = 600),
    0.002
  )
})

test_that("lumipulse_pTau217_ABeta42_ratio() returns NA when pTau217 < 0.047 & ABeta42 < 12.8", {
  expect_equal(
    lumipulse_pTau217_ABeta42_ratio(pTau217 = 0.03, ABeta42 = 10),
    NA_real_
  )
})

test_that("lumipulse_pTau217_ABeta42_ratio() returns NA when pTau217 >= 500 & ABeta42 > 1.85", {
  expect_equal(
    lumipulse_pTau217_ABeta42_ratio(pTau217 = 600, ABeta42 = 2),
    NA_real_
  )
})

test_that("lumipulse_pTau217_ABeta42_ratio() floors to 0.047 when pTau217 < 0.047 & ABeta42 >= 12.8", {
  expect_equal(
    lumipulse_pTau217_ABeta42_ratio(pTau217 = 0.03, ABeta42 = 50),
    0.047
  )
})

test_that("lumipulse_pTau217_ABeta42_ratio() rounds to 5 decimal places", {
  # 1 / 3 = 0.33333... -> rounded to 0.33333
  expect_equal(
    lumipulse_pTau217_ABeta42_ratio(pTau217 = 1, ABeta42 = 3),
    0.33333
  )
})

test_that("lumipulse_pTau217_ABeta42_ratio() is vectorized and length-preserving", {
  res <- lumipulse_pTau217_ABeta42_ratio(
    pTau217 = c(1.5, 0.03, 600, 1),
    ABeta42 = c(300, 50, 2, 3)
  )
  expect_length(res, 4L)
  expect_type(res, "double")
})


# ---------------------------------------------------------------------------
# lumipulse_ABeta42_ABeta40_ratio()
# ---------------------------------------------------------------------------

test_that("lumipulse_ABeta42_ABeta40_ratio() matches the documented example", {
  # Element 1: 800 / 5000                 -> 0.16
  # Element 2: ABeta40 < 158              -> NA
  # Element 3: ABeta40 > 28450            -> NA (ABeta42 also capped at 2200)
  expect_equal(
    lumipulse_ABeta42_ABeta40_ratio(
      ABeta40 = c(5000, 100, 30000),
      ABeta42 = c(800, 40, 2500)
    ),
    c(0.16, NA, NA)
  )
})

test_that("lumipulse_ABeta42_ABeta40_ratio() caps ABeta42 at 2200", {
  # Capped: 2200 / 10000 = 0.22 (vs uncapped 3000 / 10000 = 0.3).
  expect_equal(
    lumipulse_ABeta42_ABeta40_ratio(ABeta40 = 10000, ABeta42 = 3000),
    0.22
  )
})

test_that("lumipulse_ABeta42_ABeta40_ratio() returns NA when ABeta42 < 38", {
  expect_equal(
    lumipulse_ABeta42_ABeta40_ratio(ABeta40 = 5000, ABeta42 = 30),
    NA_real_
  )
})

test_that("lumipulse_ABeta42_ABeta40_ratio() returns NA when ABeta40 < 158", {
  expect_equal(
    lumipulse_ABeta42_ABeta40_ratio(ABeta40 = 100, ABeta42 = 40),
    NA_real_
  )
})

test_that("lumipulse_ABeta42_ABeta40_ratio() returns NA when ABeta40 > 28450", {
  expect_equal(
    lumipulse_ABeta42_ABeta40_ratio(ABeta40 = 30000, ABeta42 = 100),
    NA_real_
  )
})

test_that("lumipulse_ABeta42_ABeta40_ratio() returns the ratio for in-range values", {
  expect_equal(
    lumipulse_ABeta42_ABeta40_ratio(ABeta40 = 5000, ABeta42 = 800),
    0.16
  )
})

test_that("lumipulse_ABeta42_ABeta40_ratio() keeps the boundary ABeta42 == 38 (not NA)", {
  # The NA rule is ABeta42 < 38 (strict), so exactly 38 is retained.
  expect_equal(
    lumipulse_ABeta42_ABeta40_ratio(ABeta40 = 5000, ABeta42 = 38),
    0.0076
  )
})


# ---------------------------------------------------------------------------
# categorize_ratio()
# ---------------------------------------------------------------------------

# Shared thresholds fixture (mirrors the documented example).
thres <- data.frame(
  label = c("Negative", "Positive"),
  bin   = c(0, 1),
  min   = c(0, 0.05),
  max   = c(0.05, Inf)
)

test_that("categorize_ratio() returns the label for a value in range", {
  expect_equal(categorize_ratio(0.03, thres, out = "label"), "Negative")
})

test_that("categorize_ratio() returns the bin when out = 'bin'", {
  expect_equal(categorize_ratio(0.08, thres, out = "bin"), 1)
})

test_that("categorize_ratio() includes the lowest break and treats intervals as (min, max]", {
  # include.lowest = TRUE, right = TRUE: 0 and 0.05 both fall in the first bin.
  expect_equal(categorize_ratio(0, thres, out = "label"), "Negative")
  expect_equal(categorize_ratio(0.05, thres, out = "label"), "Negative")
})

test_that("categorize_ratio() returns NA for a value outside all thresholds", {
  expect_true(is.na(categorize_ratio(-1, thres, out = "label")))
})

test_that("categorize_ratio() errors when `out` is not length 1", {
  local_reproducible_output()
  # The default `out = c('label', 'bin')` has length 2 and must trigger the abort.
  expect_error(
    categorize_ratio(0.03, thres),
    "must be either"
  )
})
