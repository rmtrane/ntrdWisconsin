test_that("Test summarize_braak", {
  not_elevateds <- data.frame(
    braak_1 = c(0, 1, 0),
    braak_2 = c(0, 0, 1),
    braak_3 = 0,
    braak_4 = 0,
    braak_5 = 0,
    braak_6 = 0
  )

  expect_all_true(
    do.call(summarize_braak, not_elevateds) == "Not Elevated"
  )

  borderline_mtl <- data.frame(
    braak_1 = 1,
    braak_2 = 1,
    braak_3 = 0,
    braak_4 = 0,
    braak_5 = 0,
    braak_6 = 0
  )

  expect_all_true(
    do.call(summarize_braak, borderline_mtl) == "Borderline (MTL only)"
  )

  borderline_neocorticals <- expand.grid(
    braak_1 = c(0, 1),
    braak_2 = c(0, 1),
    braak_3 = c(0, 1),
    braak_4 = c(0, 1),
    braak_5 = c(0, 1),
    braak_6 = c(0, 1)
  ) |>
    subset(
      braak_1 + braak_2 < 2 & braak_3 + braak_4 + braak_5 + braak_6 > 0
    )

  expect_all_true(
    do.call(summarize_braak, borderline_neocorticals) ==
      "Borderline (Neocortical only)"
  )

  elevateds <- expand.grid(
    braak_1 = 1,
    braak_2 = 1,
    braak_3 = c(0, 1),
    braak_4 = c(0, 1),
    braak_5 = c(0, 1),
    braak_6 = c(0, 1)
  ) |>
    subset(braak_3 + braak_4 + braak_5 + braak_6 > 0)

  expect_all_true(
    do.call(summarize_braak, elevateds) == "Elevated (MTL + Neocortical)"
  )

  ## Check that we have reached all possible combinations
  all <- rbind(
    not_elevateds,
    borderline_mtl,
    borderline_neocorticals,
    elevateds
  )

  all <- all[
    with(all, order(braak_1, braak_2, braak_3, braak_4, braak_5, braak_6)),
  ]

  all_exp <- expand.grid(
    braak_1 = c(0, 1),
    braak_2 = c(0, 1),
    braak_3 = c(0, 1),
    braak_4 = c(0, 1),
    braak_5 = c(0, 1),
    braak_6 = c(0, 1)
  )

  all_exp <- all_exp[
    with(all_exp, order(braak_1, braak_2, braak_3, braak_4, braak_5, braak_6)),
  ]

  row.names(all) <- 1:nrow(all)
  row.names(all_exp) <- 1:nrow(all_exp)

  attr(all_exp, "out.attrs") <- NULL

  expect_equal(all, all_exp)
})
