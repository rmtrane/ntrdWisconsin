# Tests for R/nacc_to_wadrc_uds3.R  (data object: nacc_to_wadrc_uds3)

test_that("nacc_to_wadrc_uds3 is a well-formed named translation map", {
  # wadrc_data_prep() treats EDUC and RACE specially for UDS-3
  # (it references names(nacc_to_wadrc_uds3) %in% c("EDUC", "RACE")),
  # so both keys must be present.
  expect_valid_translation_map(
    nacc_to_wadrc_uds3,
    required_keys = c("EDUC", "RACE")
  )
})

test_that("nacc_to_wadrc_uds3 and wadrc_uds3_to_nacc agree where they overlap", {
  expect_inverse_on_overlap(nacc_to_wadrc_uds3, wadrc_uds3_to_nacc)
})
