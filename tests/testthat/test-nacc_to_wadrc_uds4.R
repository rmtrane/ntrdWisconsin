# Tests for R/nacc_to_wadrc_uds4.R  (data object: nacc_to_wadrc_uds4)

test_that("nacc_to_wadrc_uds4 is a well-formed named translation map", {
  # No UDS-4-specific required keys are asserted here: for UDS-4,
  # wadrc_data_prep() adds visityr/visitmo/visitday/sex separately rather than
  # sourcing them from this map, so only the general contract is checked.
  expect_valid_translation_map(nacc_to_wadrc_uds4)
})

test_that("nacc_to_wadrc_uds4 and wadrc_uds4_to_nacc agree where they overlap", {
  expect_inverse_on_overlap(nacc_to_wadrc_uds4, wadrc_uds4_to_nacc)
})
