# test-respval_behavioral_data.R  (plan item 9)
#
# Covers R/respval_behavioral_data.R -> respval_behavioral_data()
#
# STATUS: DRAFT — not yet executed. Response-validity assertions traced from
# source; the behavioral-observation melt output is covered by a snapshot to be
# reviewed on first run.
#
# NOTE (doc drift): the roxygen @returns documents `value` as a LIST column
# (list(value=, extra_info=)). The function actually returns `value` as a
# CHARACTER column — the list-packing happens later in behavioral_server()
# (R/behavioralModule.R). Tests below assert the real (character) behavior.

# ---------------------------------------------------------------------------
# Input contract
# ---------------------------------------------------------------------------

test_that("respval_behavioral_data() requires the checklist_complete column", {
  dt <- data.table::data.table(NACCID = "x", VISITDATE = "2021-01-01")
  expect_error(respval_behavioral_data(dt))
})

test_that("respval_behavioral_data() drops rows with NA checklist_complete", {
  out <- respval_behavioral_data(make_respval_fixture())
  # adrc002's only row had NA checklist_complete -> excluded entirely.
  expect_false("adrc002" %in% out$NACCID)
  expect_true("adrc001" %in% out$NACCID)
})

test_that("respval_behavioral_data() returns a data.table with key columns", {
  out <- respval_behavioral_data(make_respval_fixture())
  expect_s3_class(out, "data.table")
  expect_true(all(
    c("NACCID", "VISITDATE", "label", "sublabel", "extra_info", "value") %in%
      names(out)
  ))
})

# ---------------------------------------------------------------------------
# Structure of derived columns
# ---------------------------------------------------------------------------

test_that("VISITDATE is a factor with reverse-chronological levels", {
  out <- respval_behavioral_data(make_respval_fixture())
  expect_s3_class(out$VISITDATE, "factor")
  # Later dates first.
  expect_identical(levels(out$VISITDATE), c("2021-05-01", "2020-03-01"))
})

test_that("value is a character column (not the documented list column)", {
  out <- respval_behavioral_data(make_respval_fixture())
  expect_type(out$value, "character")
})

test_that("label maps behavioral domains to human-readable names", {
  out <- respval_behavioral_data(make_respval_fixture())
  labs <- as.character(unique(out$label))
  expect_true("Mood" %in% labs)
  expect_true("Response Validity" %in% labs)
  expect_true(
    "What makes this participant's responses less valid?" %in% labs
  )
})

# ---------------------------------------------------------------------------
# Response-validity recodes (fully specified in source)
# ---------------------------------------------------------------------------

test_that("respval codes recode to validity labels", {
  out <- respval_behavioral_data(make_respval_fixture())
  rv <- out[label == "Response Validity"]
  # visit 1 respval = 2 -> Questionably valid; visit 2 respval = 1 -> Very valid.
  v1 <- rv[VISITDATE == "2021-05-01"]$value
  v2 <- rv[VISITDATE == "2020-03-01"]$value
  expect_identical(v1, "Questionably valid")
  expect_identical(v2, "Very valid")
})

test_that("loc_res___8 carries respothx text into extra_info", {
  out <- respval_behavioral_data(make_respval_fixture())
  other <- out[
    label == "What makes this participant's responses less valid?" &
      sublabel == "Other" &
      VISITDATE == "2021-05-01"
  ]
  expect_equal(nrow(other), 1L)
  expect_identical(other$extra_info, "Needed a break")
})

test_that("unchecked loc_res / mood boxes (value 0) are dropped", {
  out <- respval_behavioral_data(make_respval_fixture())
  # loc_res___2 was the only 'reasons' box checked in visit 1 besides Other;
  # unchecked reason boxes must not appear.
  reasons <- out[
    label == "What makes this participant's responses less valid?" &
      VISITDATE == "2021-05-01"
  ]
  expect_setequal(as.character(reasons$sublabel), c("Distractions", "Other"))
})

# ---------------------------------------------------------------------------
# Regression snapshot for the intricate behavioral-observation melt
# ---------------------------------------------------------------------------

test_that("full output is stable (review snapshot on first run)", {
  out <- respval_behavioral_data(make_respval_fixture())
  # Normalize to a plain, deterministic form for snapshotting.
  snap <- as.data.frame(out[order(NACCID, VISITDATE, label, sublabel)])
  snap$VISITDATE <- as.character(snap$VISITDATE)
  snap$label <- as.character(snap$label)
  snap$sublabel <- as.character(snap$sublabel)
  expect_snapshot(print(snap))
})
