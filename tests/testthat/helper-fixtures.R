# helper-fixtures.R
#
# Shared, hand-constructed data.table fixtures for the `data.table`-transform
# tests (plan items 8-10). Auto-loaded by testthat before test files.
#
# STATUS: DRAFT — not yet executed against the real package. Expected values
# were derived by tracing the source logic, not by running R. Verify on first
# real run.
#
# SCOPE NOTE: these fixtures target `fill_data_downup()`, which is callable in
# isolation with a small, known set of columns. A fixture for the full
# `wadrc_data_prep()` pipeline is deliberately NOT provided here — see the note
# at the bottom.

# --- fill_data_downup() fixture ------------------------------------------
#
# fill_data_downup() renames c(ptid, visityr, visitmo, visitday, educ) (defaults
# to themselves) and fills c(sex, race, birthyr, birthmo, handed) by ptid, plus
# a within-visit numeric fill. This fixture must therefore include `educ` and
# the id/date columns literally.
#
# One participant (p1), three visit-rows across two distinct visits (rows 1 & 2
# share a visit; row 3 is later). Missingness exercises every fill path:
#   - educ: one non-missing value  -> copied to all rows
#   - sex : one non-missing value  -> filled by ptid
#   - somescore: within-visit fill -> row1/2 share a value; row3 stays NA
#   - constant cols (race/birthyr/birthmo/handed): filled by ptid
# After filling, rows 1 & 2 become identical and `unique()` collapses them,
# so the expected output has 2 rows.
make_fill_fixture <- function() {
  data.table::data.table(
    ptid = c("p1", "p1", "p1"),
    visityr = c(2020, 2020, 2021),
    visitmo = c(1, 1, 6),
    visitday = c(15, 15, 20),
    educ = c(16, NA, NA),
    sex = c(1, NA, 1),
    race = c(1, 1, NA),
    birthyr = c(1950, NA, 1950),
    birthmo = c(3, 3, NA),
    handed = c(2, NA, NA),
    somescore = c(10, NA, NA)
  )
}

# Variant to exercise the SEX 8/9 scrub inside the constant-fill block:
# 8 is scrubbed to NA, then return_single() over {1, NA, NA} yields 1.
make_sex_scrub_fixture <- function() {
  data.table::data.table(
    ptid = c("p1", "p1", "p1"),
    visityr = c(2019, 2020, 2021),
    visitmo = c(1, 1, 1),
    visitday = c(1, 1, 1),
    educ = c(12, 12, 12),
    sex = c(1, 8, NA), # 8 -> NA -> return_single -> 1
    race = c(1, 1, 1),
    birthyr = c(1955, 1955, 1955),
    birthmo = c(6, 6, 6),
    handed = c(1, 1, 1)
  )
}

# --- wadrc_data_prep() full-pipeline fixture: DEFERRED --------------------
#
# A valid UDS-2-like input can't be hand-built blind: after fill_data_downup(),
# the uds2 path selects `out[, cols_wanted, with = FALSE]` over the FULL
# nacc_to_wadrc_uds2 field set, so the fixture must carry every mapped field (and
# an `educ` column), and the map values aren't visible in project knowledge.
#
# The faithful fixture is a small captured or synthetic WADRC export, frozen as
# e.g. tests/testthat/fixtures/uds2_min.rds and loaded with readRDS(). Build it
# where REDCap data is accessible (strip/replace PHI), then assert on the output
# of wadrc_data_prep(readRDS(test_path("fixtures/uds2_min.rds")), uds = "uds2").

# --- respval_behavioral_data() fixture (plan item 9) ---------------------
#
# respval_behavioral_data() grabs columns by pattern (wadrc_c2_boc*, respval,
# loc_res, respothx) and melts them; no map objects or full-field-set selection,
# so a small wide fixture works. It REQUIRES the row-filter column
# `wadrc_c2_behavioral_observations_checklist_complete` (rows where it is NA are
# dropped).
#
# adrc001 has two visits; adrc002 has a NA checklist value and must be dropped
# entirely. VISITDATE levels sort reverse-chronologically.
make_respval_fixture <- function() {
  data.table::data.table(
    NACCID = c("adrc001", "adrc001", "adrc002"),
    VISITDATE = c("2021-05-01", "2020-03-01", "2021-06-15"),
    wadrc_c2_behavioral_observations_checklist_complete = c(2, 2, NA),
    wadrc_c2_boc_mood___1 = c(1, 0, 1), # checked -> "Happy/positive"
    wadrc_c2_boc_mood___2 = c(0, 1, 0), # checked (visit 2) -> "Irritable/angry"
    wadrc_c2_boc_battery = c(1, 2, 1), # 1 -> "checked", 2 -> "unchecked"
    wadrc_c2_boc_notes = c("Cooperative", NA, "Tired"),
    respval = c(2, 1, 3), # -> Questionably valid / Very valid / Invalid
    loc_res___2 = c(1, 0, 0), # Distractions checked (visit 1)
    loc_res___8 = c(1, 0, 0), # Other checked (visit 1) -> extra_info = respothx
    respothx = c("Needed a break", NA, NA)
  )
}


# --- behavioral_table() fixtures (plan item 12) --------------------------
#
# behavioral_table() expects `label`, `variable`, and one or more date columns
# (names matching "20[0-9]{2}-[0-9]{2}-[0-9]{2}"). NOTE: the roxygen describes
# list cells with $value/$extra_info and a `sublabel` column, but the code joins
# on `variable` and its real caller (behavioral_server) passes already-formatted
# HTML *strings* in the date columns. These fixtures follow the real caller.

# Only Response-Validity rows -> excluded from the value count -> function
# returns the "No values found" placeholder via the ncol == 0 guard.
make_behavioral_table_invalid_only <- function() {
  dt <- data.table::data.table(
    label = factor("Response Validity"),
    variable = "respval"
  )
  dt[["2021-05-01"]] <- "Questionably valid"
  dt[]
}

# A minimal valid table: one real domain row + one battery row, string cells.
make_behavioral_table_fixture <- function() {
  dt <- data.table::data.table(
    label = factor(c("Mood", "Test Battery Completed")),
    variable = c("mood", "battery")
  )
  dt[["2021-05-01"]] <- c("Happy/positive", "checked")
  dt[["2020-03-01"]] <- c(NA_character_, "unchecked")
  dt[]
}


# --- clean_biomarker_data() fixtures (plan item 10) ----------------------
#
# clean_biomarker_data(as_df, table_name, query) has inline level-maps and
# per-table renaming (all with skip_absent = TRUE), so small hand-built
# data.tables flow through — no captured JSON payload needed. `query` is unused
# by the body; pass NULL. Column names here are POST prefix-strip (no "cg_*"
# prefixes), so the internal prefix gsub is a no-op on them.

# 0-row input -> "visits could not be matched to dates." error-message.
make_biomarker_empty <- function() {
  data.table::data.table(
    date_csf = character(0),
    enumber = character(0),
    status_csf_lumi_ratio_fda = character(0)
  )
}

# 1-row, everything NA -> "no non-missing biomarker data found." error-message.
make_biomarker_all_na <- function() {
  data.table::data.table(
    date_csf = NA_character_,
    age_at_appointment = NA_character_,
    enumber = NA_character_,
    status_csf_lumi_ratio_fda = NA_character_,
    csf_ratio_lumi_ab42_ab40 = NA_character_
  )
}

# 1-row csf happy path with real-ish values (character, mimicking fromJSON).
make_biomarker_csf <- function() {
  data.table::data.table(
    date_csf = "2021-05-01",
    age_at_appointment = "70",
    enumber = "adrc00006",
    status_csf_lumi_ratio_fda = "Positive", # -> factor, renamed *_cat
    csf_ratio_lumi_ab42_ab40 = "0.052" # -> numeric, renamed *_raw
  )
}


# --- data_load() / prepare_combined() fixture ----------------------------
#
# A prepped UDS pull in NACC scheme (what pull_redcap_data() returns). Carries
# the fill keys (NACCID/VISITYR/VISITMO/VISITDAY/EDUC), the constant fields
# (BIRTHYR/BIRTHMO/SEX/RACE/HANDED), AND every raw input the derived-score
# calcs consume -- if any derivation input were missing, prepare_combined()'s
# `with()` calls would error, so this fixture also documents that contract.
#
# p1: two visits, SEX present on visit 1 / NA on visit 2  -> filled, kept.
# p2: one visit, SEX NA                                   -> dropped (+warning).
make_prepped_pull <- function() {
  data.table::data.table(
    NACCID = c("p1", "p1", "p2"),
    VISITYR = c(2018, 2020, 2019),
    VISITMO = c(3, 3, 6),
    VISITDAY = c(15, 15, 1),
    EDUC = c(16, NA, 12),
    BIRTHYR = c(1950, 1950, 1945),
    BIRTHMO = c(4, 4, 8),
    SEX = c(1, NA, NA),
    RACE = c(1, 1, 2),
    HANDED = c(1, NA, 2),
    # REYAREC inputs
    REYTCOR = c(10, 12, 8),
    REYFPOS = c(1, 0, 2),
    # FAS inputs
    BILLS = c(0, 1, 0),
    TAXES = c(0, 0, 1),
    SHOPPING = c(1, 0, 0),
    GAMES = c(0, 1, 1),
    STOVE = c(0, 0, 0),
    MEALPREP = c(1, 1, 0),
    EVENTS = c(0, 0, 1),
    PAYATTN = c(0, 1, 0),
    REMDATES = c(1, 0, 0),
    TRAVEL = c(0, 0, 1),
    # MOCACLOCK inputs
    MOCACLOC = c(1, 1, 0),
    MOCACLON = c(1, 0, 1),
    MOCACLOH = c(1, 1, 1),
    # REYTOTAL inputs
    REY1REC = c(2, 3, 1),
    REY2REC = c(4, 5, 2),
    REY3REC = c(6, 6, 3),
    REY4REC = c(7, 8, 4),
    REY5REC = c(9, 9, 5)
  )
}
