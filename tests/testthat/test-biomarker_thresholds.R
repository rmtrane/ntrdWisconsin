# Tests for R/biomarker_thresholds.R
#
# biomarker_thresholds is a bundled (lazy-loaded) dataset, not a function, so
# these are structural / contract tests: they assert the shape documented in
# ?biomarker_thresholds so that a bad regeneration from data-raw/ is caught.
#
# Documented contract:
#   - A named list keyed by Panda table name.
#   - Each element is a named list keyed by biomarker name.
#   - Each biomarker entry is a list with:
#       $source     : list with `description` (character) and `link` (character)
#       $thresholds : data.frame with columns label, min, max, bin
#
# No API tokens required.


test_that("biomarker_thresholds is a non-empty, uniquely named list", {
  expect_type(biomarker_thresholds, "list")
  expect_gt(length(biomarker_thresholds), 0L)

  nms <- names(biomarker_thresholds)
  expect_false(is.null(nms))
  expect_true(all(nzchar(nms)))
  expect_false(anyDuplicated(nms) > 0L)
})

test_that("each Panda table maps to a non-empty, uniquely named list of biomarkers", {
  for (tbl in names(biomarker_thresholds)) {
    entry <- biomarker_thresholds[[tbl]]
    expect_type(entry, "list")
    expect_gt(length(entry), 0L)

    bio_nms <- names(entry)
    expect_false(is.null(bio_nms), info = tbl)
    expect_true(all(nzchar(bio_nms)), info = tbl)
    expect_false(anyDuplicated(bio_nms) > 0L, info = tbl)
  }
})

test_that("every biomarker entry carries a `source` and a `thresholds` element", {
  for (tbl in names(biomarker_thresholds)) {
    for (bio in names(biomarker_thresholds[[tbl]])) {
      leaf <- biomarker_thresholds[[tbl]][[bio]]
      loc  <- paste(tbl, bio, sep = " / ")

      expect_type(leaf, "list")
      expect_true("source" %in% names(leaf), info = loc)
      expect_true("thresholds" %in% names(leaf), info = loc)
    }
  }
})

test_that("every `source` describes where the categorization comes from", {
  for (tbl in names(biomarker_thresholds)) {
    for (bio in names(biomarker_thresholds[[tbl]])) {
      src <- biomarker_thresholds[[tbl]][[bio]]$source
      loc <- paste(tbl, bio, sep = " / ")

      expect_type(src, "list")
      expect_true("description" %in% names(src), info = loc)
      expect_true("link" %in% names(src), info = loc)
      expect_type(src$description, "character")
      expect_type(src$link, "character")
    }
  }
})

test_that("every `thresholds` is a data.frame with label/min/max/bin columns", {
  required_cols <- c("label", "min", "max", "bin")

  for (tbl in names(biomarker_thresholds)) {
    for (bio in names(biomarker_thresholds[[tbl]])) {
      thr <- biomarker_thresholds[[tbl]][[bio]]$thresholds
      loc <- paste(tbl, bio, sep = " / ")

      expect_s3_class(thr, "data.frame")
      expect_true(all(required_cols %in% names(thr)), info = loc)
      expect_gt(nrow(thr), 0L)
    }
  }
})

test_that("threshold bounds are numeric and ordered (min <= max within each row)", {
  for (tbl in names(biomarker_thresholds)) {
    for (bio in names(biomarker_thresholds[[tbl]])) {
      thr <- biomarker_thresholds[[tbl]][[bio]]$thresholds
      loc <- paste(tbl, bio, sep = " / ")

      expect_type(thr$min, "double")
      expect_type(thr$max, "double")
      # Inf-safe: a finite lower bound is always <= an Inf upper bound.
      expect_true(all(thr$min <= thr$max), info = loc)
    }
  }
})

test_that("threshold labels are present (no missing category names)", {
  for (tbl in names(biomarker_thresholds)) {
    for (bio in names(biomarker_thresholds[[tbl]])) {
      thr <- biomarker_thresholds[[tbl]][[bio]]$thresholds
      loc <- paste(tbl, bio, sep = " / ")

      expect_false(anyNA(thr$label), info = loc)
    }
  }
})
