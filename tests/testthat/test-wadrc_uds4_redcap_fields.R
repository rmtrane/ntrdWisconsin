# Tests for R/wadrc_uds4_redcap_fields.R  (data object: wadrc_uds4_redcap_fields)

test_that("wadrc_uds4_redcap_fields is a clean vector of field names", {
  expect_valid_field_vector(wadrc_uds4_redcap_fields)
})
