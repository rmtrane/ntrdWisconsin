# test-query_panda.R  (plan item 15)
#
# Covers R/query_panda.R:
#   html_cat()            [pure]    -> status/rating -> icon + text cell content
#   bio_tab_for_gt()      [pure]    -> cleaned biomarker table -> wide list-cell
#                                      table for bio_tab_to_html_table()
#   get_all_densities()   [pure]    -> density() per *_raw column
#   get_all_cuts()        [pure]    -> known thresholds, or inferred from bins
#   check_connection()    [network] -> Layer 1 mocked, Layer 2 live
#   get_biomarker_data()  [network] -> Layer 1 mocked, Layer 2 live
#
#
# STATUS: DRAFT — not yet executed. Traced from source.
#
# KNOWN BUGS: the "Known bugs" section at the end asserts intended behaviour
# and is expected to FAIL until the package is fixed (see the notes there).
#
# DOC DRIFT (tests assert real behaviour):
#   - bio_tab_for_gt() @returns says NULL for NULL input; it returns the
#     "No values found" table.
#   - get_all_densities() @param says a named list of data.tables; it takes a
#     single data.table.
#   - get_biomarker_data() @returns says a named list of data.tables; it returns
#     one cleaned data.table (or a try-error, an HTTP status, an
#     "error-message", or a one-column table when Panda has no rows).
#
# LAYER 2 (live) needs options(panda_api_key = "...") and campus/VPN access.

# ---------------------------------------------------------------------------
# html_cat()
# ---------------------------------------------------------------------------

hc <- function(x, name) html_cat(x, rep(name, length(x)))

is_negative <- function(cell) {
  identical(cell$text, "Negative") &&
    grepl("minus-sign", cell$icon, fixed = TRUE)
}
is_positive <- function(cell) {
  identical(cell$text, "Positive") &&
    grepl("plus-sign", cell$icon, fixed = TRUE)
}

test_that("html_cat() returns one cell per input value", {
  expect_length(hc(c("Positive", "Negative", NA), "foo"), 3L)
})

test_that("html_cat() maps generic statuses to icon + text", {
  out <- hc(
    c("Positive", "SAA+", "Negative", "SAA-", "Likely Positive"),
    "csf_foo"
  )
  expect_true(is_positive(out[[1]]))
  expect_true(is_positive(out[[2]]))
  expect_true(is_negative(out[[3]]))
  expect_true(is_negative(out[[4]]))
  expect_identical(out[[5]]$text, "Likely Positive")
  expect_true(grepl("#ffa9a9", out[[5]]$icon, fixed = TRUE))
})

test_that("html_cat() gives text-only cells for Indeterminate/Unavailable", {
  out <- hc(c("Indeterminate", "Unavailable"), "plasma_foo")
  expect_identical(out[[1]], list(text = "Indeterminate"))
  expect_identical(out[[2]], list(text = "Unavailable"))
})

test_that("html_cat() returns an empty cell for unknown or missing values", {
  out <- hc(c("something else", NA), "csf_foo")
  expect_identical(out[[1]], list())
  expect_identical(out[[2]], list())
})

test_that("html_cat() maps Braak stage 0/1 to negative/positive", {
  out <- hc(c("0", "1"), "braak_3")
  expect_true(is_negative(out[[1]]))
  expect_true(is_positive(out[[2]]))

  # Numeric input works the same way.
  num <- hc(c(0, 1), "braak_3")
  expect_true(is_negative(num[[1]]))
  expect_true(is_positive(num[[2]]))
})

test_that("html_cat() passes braak_positive text through, even when NA", {
  out <- hc(c("Borderline (MTL only)", NA), "braak_positive")
  expect_identical(out[[1]], list(text = "Borderline (MTL only)"))
  expect_identical(out[[2]], list(text = NA_character_))
})

test_that("html_cat() passes non-empty Braak comments through as text", {
  out <- hc(c("Tau in MTL", "", NA), "braak_comment")
  expect_identical(out[[1]], list(text = "Tau in MTL"))
  expect_identical(out[[2]], list())
  expect_identical(out[[3]], list())
})

test_that("html_cat() maps NAV4694 ratings 0-2 to negative and 3 to positive", {
  out <- hc(c("0", "1", "2", "3"), "nav4694_visual_ratings")
  expect_true(all(vapply(out[1:3], is_negative, logical(1))))
  expect_true(is_positive(out[[4]]))
})

test_that("html_cat() maps PiB ratings 0-3 to their labels", {
  out <- hc(c("0", "1", "2", "3"), "pib_visual_ratings_20180126")

  expect_identical(out[[1]]$text, "Clearly PiB negative (0)")
  expect_true(grepl("minus-sign", out[[1]]$icon, fixed = TRUE))
  expect_identical(out[[2]]$text, "Clearly PiB negative (1)")
  expect_true(grepl("minus-sign", out[[2]]$icon, fixed = TRUE))

  expect_identical(out[[3]], list(text = "Ambiguous/Indeterminate"))

  expect_identical(out[[4]]$text, "PiB+")
  expect_true(grepl("plus-sign", out[[4]]$icon, fixed = TRUE))
})

test_that("html_cat() handles mixed biomarkers in one call", {
  out <- html_cat(
    c("1", "Positive", "3"),
    c("braak_2", "csf_foo", "nav4694_visual_ratings")
  )
  expect_true(is_positive(out[[1]]))
  expect_true(is_positive(out[[2]]))
  expect_true(is_positive(out[[3]]))
})

# ---------------------------------------------------------------------------
# bio_tab_for_gt()
# ---------------------------------------------------------------------------

# A table in the shape clean_biomarker_data() returns: date, age, enumber, and
# <name>_raw / <name>_cat columns.
make_cleaned_tab <- function() {
  data.table::data.table(
    date = as.Date(c("2021-05-01", "2022-06-15")),
    age = c(70, 71),
    enumber = "adrc00001",
    foo_raw = c(1.5, 2.5),
    foo_cat = c("Positive", "Negative")
  )
}

cell <- function(out, nm, date) out[out$name == nm][[date]][[1]]

no_values_tab <- data.table::data.table(
  name = "No values found",
  name_label = "No values found"
)

test_that("bio_tab_for_gt() returns a try-error unchanged", {
  err <- structure("Error in x : boom\n", class = "try-error")
  expect_identical(bio_tab_for_gt(err), err)
})

test_that("bio_tab_for_gt() returns the 'No values found' table for NULL", {
  expect_equal(bio_tab_for_gt(NULL), no_values_tab, ignore_attr = TRUE)
})

test_that("bio_tab_for_gt() returns 'No values found' when every date is missing", {
  tab <- make_cleaned_tab()[, date := as.Date(NA)]
  expect_equal(bio_tab_for_gt(tab), no_values_tab, ignore_attr = TRUE)
})

test_that("bio_tab_for_gt() aborts for a non-data.table", {
  local_reproducible_output()
  df <- as.data.frame(make_cleaned_tab())
  expect_error(suppressWarnings(bio_tab_for_gt(df)), regexp = "data.table")
})

test_that("bio_tab_for_gt() widens to one row per biomarker, one column per date", {
  out <- bio_tab_for_gt(make_cleaned_tab())

  expect_s3_class(out, "data.table")
  expect_setequal(
    names(out),
    c("name", "name_label", "2021-05-01", "2022-06-15")
  )
  expect_type(out$name, "character")
  expect_setequal(out$name, c("Age", "foo"))
})

test_that("bio_tab_for_gt() puts raw value and category in each cell", {
  out <- bio_tab_for_gt(make_cleaned_tab())

  c1 <- cell(out, "foo", "2021-05-01")
  expect_equal(c1$raw, 1.5)
  expect_true(is_positive(c1$cat))

  c2 <- cell(out, "foo", "2022-06-15")
  expect_equal(c2$raw, 2.5)
  expect_true(is_negative(c2$cat))
})

test_that("bio_tab_for_gt() turns age into an 'Age' row with an empty category", {
  out <- bio_tab_for_gt(make_cleaned_tab())
  age <- cell(out, "Age", "2021-05-01")
  expect_equal(age$raw, 70)
  expect_length(age$cat, 0L)
  expect_identical(out[out$name == "Age"]$name_label, "Age")
})

test_that("bio_tab_for_gt() leaves `raw` out of a cell when the value is NA", {
  tab <- make_cleaned_tab()[2, foo_raw := NA]
  out <- bio_tab_for_gt(tab)
  expect_named(cell(out, "foo", "2022-06-15"), "cat")
  expect_named(cell(out, "foo", "2021-05-01"), c("raw", "cat"))
})

test_that("bio_tab_for_gt() drops rows with a missing date", {
  tab <- make_cleaned_tab()[2, date := as.Date(NA)]
  out <- bio_tab_for_gt(tab)
  expect_false("2022-06-15" %in% names(out))
  expect_true("2021-05-01" %in% names(out))
})

test_that("bio_tab_for_gt() drops visits with no biomarker values at all", {
  tab <- rbind(
    make_cleaned_tab(),
    data.table::data.table(
      date = as.Date("2023-01-01"),
      age = 72,
      enumber = "adrc00001",
      foo_raw = NA_real_,
      foo_cat = NA_character_
    )
  )
  out <- bio_tab_for_gt(tab)
  expect_false("2023-01-01" %in% names(out))
})

test_that("bio_tab_for_gt() converts character dates", {
  tab <- make_cleaned_tab()[, date := as.character(date)]
  out <- bio_tab_for_gt(tab)
  expect_true(all(c("2021-05-01", "2022-06-15") %in% names(out)))
})

test_that("bio_tab_for_gt() coerces numeric categories (Braak 0/1)", {
  tab <- data.table::data.table(
    date = as.Date(c("2021-05-01", "2022-06-15")),
    age = c(70, 71),
    enumber = "adrc00001",
    braak_1_cat = c(1, 0)
  )
  out <- bio_tab_for_gt(tab)

  expect_true(is_positive(cell(out, "braak_1", "2021-05-01")$cat))
  expect_true(is_negative(cell(out, "braak_1", "2022-06-15")$cat))
  # No raw column for this biomarker, so no `raw` in its cells.
  expect_named(cell(out, "braak_1", "2021-05-01"), "cat")
})

test_that("bio_tab_for_gt() applies human-readable labels", {
  tab <- data.table::data.table(
    date = as.Date("2021-05-01"),
    age = 70,
    enumber = "adrc00001",
    hdx_ptau217_ashton_raw = 0.6,
    hdx_ptau217_ashton_cat = "Positive",
    braak_2_cat = "1",
    pib_visual_ratings_20180126_cat = "3",
    mystery_raw = 1
  )
  out <- bio_tab_for_gt(tab)
  label <- \(nm) out[out$name == nm]$name_label

  expect_identical(
    label("hdx_ptau217_ashton"),
    "Quanterix HDX pTau217 (Ashton et al.)"
  )
  expect_identical(label("braak_2"), "Stage II")
  expect_identical(label("pib_visual_ratings_20180126"), "PiB Visual Rating")
  expect_identical(label("mystery"), "mystery") # default: the name itself
})

test_that("bio_tab_for_gt() works on real clean_biomarker_data() output", {
  # Same chain item 13 relies on; guards the hand-off between the two files.
  out <- bio_tab_for_gt(clean_biomarker_data(make_biomarker_csf(), "csf", NULL))
  c1 <- cell(out, "csf_ratio_lumi_ab42_ab40_fda", "2021-05-01")
  expect_equal(c1$raw, 0.052)
  expect_true(is_positive(c1$cat))
})

# ---------------------------------------------------------------------------
# get_all_densities()
# ---------------------------------------------------------------------------

make_density_input <- function(n = 50) {
  set.seed(1)
  data.table::data.table(
    date = as.Date("2021-05-01") + seq_len(n + 1),
    a_raw = c(abs(rnorm(n, 5)), NA),
    b_raw = c(abs(rnorm(n, 2)), NA),
    a_cat = "Positive"
  )
}

test_that("get_all_densities() returns one density per *_raw column", {
  out <- get_all_densities(make_density_input())
  expect_named(out, c("a_raw", "b_raw"), ignore.order = TRUE)
  expect_s3_class(out$a_raw, "density")
  expect_s3_class(out$b_raw, "density")
})

test_that("get_all_densities() drops NAs and starts each grid at 0", {
  out <- get_all_densities(make_density_input(50))
  expect_equal(out$a_raw$n, 50L)
  expect_equal(min(out$a_raw$x), 0)
})

test_that("get_all_densities() widens the SJ bandwidth by 1.5 for small samples", {
  x <- make_density_input(50)
  y <- stats::na.omit(x$a_raw)
  out <- get_all_densities(x)
  expect_equal(out$a_raw$bw, 1.5 * stats::bw.SJ(y, method = "ste"))
})

test_that("get_all_densities() uses the plain SJ bandwidth above 500 values", {
  x <- make_density_input(600)
  y <- stats::na.omit(x$a_raw)
  out <- get_all_densities(x)
  expect_equal(out$a_raw$bw, stats::bw.SJ(y, method = "ste"))
})

test_that("get_all_densities() returns NULL for 0 rows or no *_raw columns", {
  expect_null(get_all_densities(make_density_input()[0]))
  expect_null(get_all_densities(make_density_input()[, list(date, a_cat)]))
})

# ---------------------------------------------------------------------------
# get_all_cuts()
# ---------------------------------------------------------------------------

cut_cols <- c("name", "bin", "color", "min_obs", "max_obs")
green <- "rgba(0, 100, 0, alpha)"
red <- "rgba(139, 0, 0, alpha)"
grey <- "rgba(216, 216, 216, 0.5)"

test_that("get_all_cuts() keeps input names and skips the Amprion table", {
  out <- get_all_cuts(list(
    "Amprion - CSF a-Synuclein" = data.table::data.table(x_raw = 1, x_bin = 0),
    "No raw table" = data.table::data.table(x_cat = "Positive")
  ))
  expect_named(out, c("Amprion - CSF a-Synuclein", "No raw table"))
  expect_null(out[["Amprion - CSF a-Synuclein"]])
  expect_null(out[["No raw table"]]) # nothing to infer from
})

test_that("get_all_cuts() uses biomarker_thresholds for known tables", {
  known <- setdiff(names(biomarker_thresholds), "Amprion - CSF a-Synuclein")
  skip_if(length(known) == 0, "No known tables in biomarker_thresholds")

  out <- get_all_cuts(
    setNames(rep(list(data.table::data.table()), length(known)), known)
  )

  for (tbl in known) {
    cuts <- out[[tbl]]
    expected <- data.table::rbindlist(
      lapply(biomarker_thresholds[[tbl]], `[[`, "thresholds"),
      use.names = TRUE,
      idcol = "name"
    )

    expect_named(cuts, cut_cols, label = tbl)
    expect_identical(cuts$name, expected$name, label = tbl)
    expect_equal(cuts$bin, expected$bin, label = tbl)
    expect_equal(cuts$min_obs, expected$min, label = tbl)
    expect_equal(cuts$max_obs, expected$max, label = tbl)

    expect_true(all(cuts[bin == 0]$color == green), label = tbl)
    expect_true(all(cuts[bin == 1]$color == red), label = tbl)
    expect_true(all(cuts[bin == 0.5]$color == grey), label = tbl)
  }
})

test_that("get_all_cuts() infers two-bin cuts halfway between groups", {
  # bin 0 spans 1-3, bin 1 spans 10-12 -> boundary at (3 + 10) / 2 = 6.5.
  # The NA-bin row is ignored.
  out <- get_all_cuts(list(
    "Some table" = data.table::data.table(
      foo_raw = c(1, 2, 3, 10, 11, 12, 5),
      foo_bin = c(0, 0, 0, 1, 1, 1, NA)
    )
  ))
  expect_equal(
    out[["Some table"]],
    data.table::data.table(
      name = c("foo", "foo"),
      bin = c(0, 1),
      color = c(green, red),
      min_obs = c(0, 6.5),
      max_obs = c(6.5, Inf)
    ),
    ignore_attr = TRUE
  )
})

test_that("get_all_cuts() rescales three bins to 0 / 0.5 / 1", {
  out <- get_all_cuts(list(
    "Some table" = data.table::data.table(
      foo_raw = c(1, 2, 4, 5, 8, 9),
      foo_bin = c(0, 0, 1, 1, 2, 2)
    )
  ))
  cuts <- out[["Some table"]]
  expect_equal(cuts$bin, c(0, 0.5, 1))
  expect_identical(cuts$color, c(green, grey, red))
  expect_equal(cuts$min_obs, c(0, 3, 6.5))
  expect_equal(cuts$max_obs, c(3, 6.5, Inf))
})

test_that("get_all_cuts() reads *_cat columns as bins", {
  out <- get_all_cuts(list(
    "Some table" = data.table::data.table(
      foo_raw = c(1, 3, 10, 12),
      foo_cat = c(0, 0, 1, 1)
    )
  ))
  expect_equal(out[["Some table"]]$min_obs, c(0, 6.5))
})

test_that("get_all_cuts() infers cuts per biomarker, sorted by name", {
  out <- get_all_cuts(list(
    "Some table" = data.table::data.table(
      zed_raw = c(1, 3, 10, 12),
      zed_bin = c(0, 0, 1, 1),
      abc_raw = c(0.1, 0.2, 0.5, 0.6),
      abc_bin = c(0, 0, 1, 1)
    )
  ))
  cuts <- out[["Some table"]]
  expect_identical(cuts$name, c("abc", "abc", "zed", "zed"))
  expect_equal(cuts[name == "abc"]$max_obs, c(0.35, Inf))
  expect_equal(cuts[name == "zed"]$max_obs, c(6.5, Inf))
})

# ---------------------------------------------------------------------------
# Network helpers (Layer 1)
# ---------------------------------------------------------------------------

# A Panda-style response: {"data": "<json string>"}.
fake_panda_response <- function(data_json = "[]", status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(as.character(
      jsonlite::toJSON(list(data = data_json), auto_unbox = TRUE)
    ))
  )
}

# Replace httr2::req_perform() for the calling test. `result` is returned, or,
# if it's a condition message string wrapped in I(), thrown as an error.
# Returns a function giving the last request seen.
local_perform <- function(result, env = parent.frame()) {
  seen <- NULL
  local_mocked_bindings(
    req_perform = function(req, ...) {
      seen <<- req
      if (inherits(result, "AsIs")) {
        stop(unclass(result))
      }
      result
    },
    .package = "httr2",
    .env = env
  )
  function() seen
}

csf_json <- function() system.file("json/csf.json", package = "ntrdWisconsin")

# ---------------------------------------------------------------------------
# check_connection() -- Layer 1
# ---------------------------------------------------------------------------

test_that("check_connection() reports success with the HTTP status", {
  local_perform(fake_panda_response())
  expect_identical(
    check_connection(api_key = "k"),
    list(connected = TRUE, status = 200L, message = "Connection successful")
  )
})

test_that("check_connection() reports failure instead of erroring", {
  local_perform(I("timed out"))
  expect_identical(
    check_connection(api_key = "k"),
    list(
      connected = FALSE,
      status = NA,
      message = "Connection failed: timed out"
    )
  )
})

test_that("check_connection() sends an authorised POST with the timeout", {
  last <- local_perform(fake_panda_response())
  check_connection(api_key = "secret-key", timeout = 2)
  req <- last()

  expect_identical(req$url, "https://panda.medicine.wisc.edu/api/search/search")
  expect_identical(req$method, "POST")
  expect_equal(req$options$timeout_ms, 2000)

  skip_if(
    utils::packageVersion("httr2") < "1.1.0",
    "req_get_headers() needs httr2 >= 1.1.0"
  )
  headers <- httr2::req_get_headers(req, redacted = "reveal")
  expect_identical(headers$Authorization, "Bearer secret-key")
})

# ---------------------------------------------------------------------------
# get_biomarker_data() -- Layer 1
# ---------------------------------------------------------------------------

test_that("get_biomarker_data() returns the try-error when the request fails", {
  local_perform(I("offline"))
  out <- get_biomarker_data("k", base_query_file = csf_json())
  expect_s3_class(out, "try-error")
})

test_that("get_biomarker_data() returns the status for a non-200 success code", {
  # httr2 turns 4xx/5xx into errors itself (previous test), so only codes like
  # 204 reach this branch.
  local_perform(fake_panda_response(status = 204L))
  expect_identical(get_biomarker_data("k", base_query_file = csf_json()), 204L)
})

test_that("get_biomarker_data() returns just the IDs when Panda has no rows", {
  local_perform(fake_panda_response("[]"))
  out <- get_biomarker_data(
    "k",
    base_query_file = csf_json(),
    adrc_ptids = "adrc00006"
  )
  expect_equal(
    out,
    data.table::data.table(enumber = "adrc00006"),
    ignore_attr = TRUE
  )
})

test_that("get_biomarker_data() cleans the rows, keeping only ADRC IDs", {
  rows <- rbind(
    make_biomarker_csf(),
    data.table::data.table(
      date_csf = "2021-06-01",
      age_at_appointment = "65",
      enumber = "wrap0001",
      status_csf_lumi_ratio_fda = "Negative",
      csf_ratio_lumi_ab42_ab40 = "0.09"
    )
  )
  local_perform(fake_panda_response(as.character(jsonlite::toJSON(rows))))

  out <- get_biomarker_data("k", base_query_file = csf_json())

  expect_false("wrap0001" %in% out$enumber)
  # The csf template routes to clean_biomarker_data(..., "csf").
  expect_equal(
    out,
    clean_biomarker_data(make_biomarker_csf(), "csf", NULL),
    ignore_attr = TRUE
  )
})

test_that("get_biomarker_data() sends an authorised POST with the template", {
  last <- local_perform(fake_panda_response())
  get_biomarker_data("secret-key", base_query_file = csf_json())
  req <- last()

  expect_identical(req$url, "https://panda.medicine.wisc.edu/api/search/search")
  expect_identical(req$method, "POST")
  expect_identical(
    req$body$data,
    jsonlite::fromJSON(readLines(csf_json()))
  )

  skip_if(
    utils::packageVersion("httr2") < "1.1.0",
    "req_get_headers() needs httr2 >= 1.1.0"
  )
  headers <- httr2::req_get_headers(req, redacted = "reveal")
  expect_identical(headers$Authorization, "Bearer secret-key")
})

enrollment_constraint <- function(req) {
  tabs <- req$body$data$query$tables
  enr <- tabs[tabs$name == "Enrollments", ]
  list(join = enr$join, constraint = enr$columns[[1]]$constraints[[1]])
}

test_that("get_biomarker_data() filters to one participant with '='", {
  last <- local_perform(fake_panda_response())
  get_biomarker_data(
    "k",
    base_query_file = csf_json(),
    adrc_ptids = "adrc00006"
  )

  got <- enrollment_constraint(last())
  expect_identical(got$join, "inner")
  expect_identical(got$constraint$operator, "=")
  expect_identical(unlist(got$constraint$values), "'adrc00006'")
})

test_that("get_biomarker_data() filters to several participants with 'in'", {
  last <- local_perform(fake_panda_response())
  get_biomarker_data(
    "k",
    base_query_file = csf_json(),
    adrc_ptids = c("adrc00006", "adrc00010")
  )

  got <- enrollment_constraint(last())
  expect_identical(got$join, "inner")
  expect_identical(got$constraint$operator, "in")
  expect_identical(unlist(got$constraint$values), c("adrc00006", "adrc00010"))
})

# ---------------------------------------------------------------------------
# Layer 2: live Panda (local only; needs options(panda_api_key) + VPN/campus)
# ---------------------------------------------------------------------------

skip_if_no_panda <- function() {
  skip_on_cran()
  key <- getOption("panda_api_key")
  skip_if(is.null(key), "No Panda API key set")
  skip_if_not(check_connection(key)$connected, "Panda not reachable")
  key
}

test_that("live: check_connection() succeeds with the configured key", {
  key <- skip_if_no_panda()
  res <- check_connection(key)
  expect_true(res$connected)
  expect_identical(res$status, 200L)
})

for (tmpl in c("csf", "plasma", "visual_ratings")) {
  test_that(paste0("live: ", tmpl, " pull meets its contract"), {
    key <- skip_if_no_panda()
    out <- get_biomarker_data(
      api_key = key,
      adrc_ptids = "adrc00449",
      base_query_file = system.file(
        paste0("json/", tmpl, ".json"),
        package = "ntrdWisconsin"
      )
    )

    expect_s3_class(out, "data.table")
    expect_true(all(c("date", "age", "enumber") %in% names(out)))
    expect_true(all(grepl("^adrc", out$enumber, ignore.case = TRUE)))
    expect_snapshot(
      vapply(out, function(x) class(x)[1], character(1))[sort(names(out))]
    )
  })
}

test_that("bio_tab_for_gt() turns an error-message into a one-row table", {
  msg <- structure(
    "Error: visits could not be matched to dates.",
    class = "error-message"
  )

  out <- bio_tab_for_gt(msg)

  expect_equal(
    out,
    data.table::data.table(
      name = "Error: visits could not be matched to dates."
    ),
    ignore_attr = TRUE
  )
})

test_that("get_all_densities() returns NULL for NULL or a try-error", {
  expect_null(get_all_densities(NULL))
  expect_null(get_all_densities(structure("boom", class = "try-error")))
})
