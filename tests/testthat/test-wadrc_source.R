# Tests for R/wadrc_source.R
#
# wadrc_source is built with ntrd::new_data_source(), which returns an S7
# *class* (constructor). Calling wadrc_source() yields an instance whose
# id / name properties are set. No API tokens required.

test_that("wadrc_source is an S7 class (constructor), not an instance", {
  expect_true(inherits(wadrc_source, "S7_class"))
})

test_that("a wadrc_source instance inherits from ntrd::data_source", {
  src <- wadrc_source()
  expect_true(S7::S7_inherits(src, ntrd::data_source))
})

test_that("a wadrc_source instance carries the documented id and name", {
  src <- wadrc_source()
  expect_identical(src@id, "wadrc_redcap")
  expect_identical(src@name, "Wisconsin ADRC")
})
