# Tests for R/wadrc_uds2_to_nacc.R  (data object: wadrc_uds2_to_nacc)

test_that("wadrc_uds2_to_nacc is a well-formed named translation map", {
  expect_valid_translation_map(wadrc_uds2_to_nacc)
})
