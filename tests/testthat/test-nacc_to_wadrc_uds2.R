# Tests for R/nacc_to_wadrc_uds2.R  (data object: nacc_to_wadrc_uds2)

test_that("nacc_to_wadrc_uds2 is a well-formed named translation map", {
  # These four keys are indexed directly by wadrc_data_prep() for UDS-2
  # (nacc_to_wadrc_uds2[c("NACCID", "VISITDAY", "VISITMO", "VISITYR")]),
  # so their presence is part of the contract.
  expect_valid_translation_map(
    nacc_to_wadrc_uds2,
    required_keys = c("NACCID", "VISITYR", "VISITMO", "VISITDAY")
  )
})

test_that("nacc_to_wadrc_uds2 and wadrc_uds2_to_nacc agree where they overlap", {
  expect_inverse_on_overlap(nacc_to_wadrc_uds2, wadrc_uds2_to_nacc)
})
