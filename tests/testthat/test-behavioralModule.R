# test-behavioralModule.R  (Shiny modules, M2)
#
# Covers R/behavioralModule.R:
#   behavioral_ui()      -> uiOutput placeholder
#   behavioral_server()  -> respval_behavioral_data() -> per-participant wide
#                           table of formatted cells -> behavioral_table()
#   behavioralApp()      -> dev app wrapper (smoke test with mocked loading)
#
# STATUS: DRAFT — not yet executed. Expected values come from the reviewed
# snapshot of respval_behavioral_data(make_respval_fixture()) in
# _snaps/respval_behavioral_data.md. For adrc001 that is:
#
#   2021-05-01: battery "checked" (extra_info "All tests were completed"),
#               notes "Cooperative", respval "Questionably valid",
#               loc_res "Other" (extra_info "Needed a break") + "Distractions",
#               mood "Happy/positive"
#   2020-03-01: battery "unchecked", respval "Very valid",
#               mood "Irritable/angry"
#
# APPROACH: behavioral_table() is replaced with a recorder, so tests can
# inspect the table the module builds (`for_tab`) before it is rendered. One
# test at the end renders for real.
#
# The respval/loc_res pipeline is exercised end to end here: this is where
# respval_behavioral_data()'s character `value` column gets packed into
# list(value, extra_info) and formatted by format_date_cell() — the hand-off
# behind item 9's doc drift.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Replace behavioral_table() for the calling test; returns a function giving
# the recorded calls (for_tab + table_id).
local_table_capture <- function(env = parent.frame()) {
  calls <- list()
  local_mocked_bindings(
    behavioral_table = function(for_tab, table_id = "status-tracker") {
      calls[[length(calls) + 1L]] <<- list(
        for_tab = for_tab,
        table_id = table_id
      )
      shiny::div("stub")
    },
    .env = env
  )
  function() calls
}

last_tab <- function(calls) {
  got <- calls()
  if (length(got) == 0) NULL else got[[length(got)]]$for_tab
}

cell <- function(tab, var, date) tab[tab$variable == var][[date]]

has <- function(x, pattern) grepl(pattern, x, fixed = TRUE)

# Fixture variant where adrc002's visit is kept (checklist complete).
make_two_ptid_fixture <- function() {
  fx <- make_respval_fixture()
  fx[3, wadrc_c2_behavioral_observations_checklist_complete := 2]
  fx[]
}

# ---------------------------------------------------------------------------
# behavioral_ui()
# ---------------------------------------------------------------------------

test_that("behavioral_ui() is a namespaced uiOutput for the table", {
  html <- as.character(behavioral_ui())
  expect_true(has(html, 'id="behavior-table"'))
  expect_true(has(html, "shiny-html-output"))

  expect_true(has(as.character(behavioral_ui("x")), 'id="x-table"'))
})

# ---------------------------------------------------------------------------
# behavioral_server() -- when the table is (not) built
# ---------------------------------------------------------------------------

test_that("nothing is rendered until data is available", {
  calls <- local_table_capture()
  dat <- shiny::reactiveVal(NULL)

  shiny::testServer(
    behavioral_server,
    args = list(dat = dat, ptid = shiny::reactive("adrc001")),
    {
      session$flushReact()
      try(output$table, silent = TRUE)
      expect_length(calls(), 0L)

      dat(make_respval_fixture())
      session$flushReact()
      output$table
      expect_gte(length(calls()), 1L)
      expect_gt(nrow(last_tab(calls)), 0L)
    }
  )
})

test_that("a participant without behavioral data gets an empty table", {
  calls <- local_table_capture()
  shiny::testServer(
    behavioral_server,
    # default ptid ("adrc00006") isn't in the fixture
    args = list(dat = shiny::reactive(make_respval_fixture())),
    {
      session$flushReact()
      output$table
      tab <- last_tab(calls)
      expect_s3_class(tab, "data.table")
      expect_equal(nrow(tab), 0L)
    }
  )
})

test_that("the table is passed to behavioral_table() with id 'behaviorTable'", {
  calls <- local_table_capture()
  shiny::testServer(
    behavioral_server,
    args = list(
      dat = shiny::reactive(make_respval_fixture()),
      ptid = shiny::reactive("adrc001")
    ),
    {
      session$flushReact()
      output$table
      expect_identical(calls()[[1]]$table_id, "behaviorTable")
    }
  )
})

# ---------------------------------------------------------------------------
# behavioral_server() -- shape of the table
# ---------------------------------------------------------------------------

build_tab <- function(fixture = make_respval_fixture(), id = "adrc001") {
  calls <- local_table_capture()
  tab <- NULL
  shiny::testServer(
    behavioral_server,
    args = list(dat = shiny::reactive(fixture), ptid = shiny::reactive(id)),
    {
      session$flushReact()
      output$table
      tab <<- last_tab(calls)
    }
  )
  tab
}

test_that("one row per domain, one column per visit date", {
  tab <- build_tab()
  expect_setequal(
    names(tab),
    c("label", "variable", "2021-05-01", "2020-03-01")
  )
  expect_setequal(
    tab$variable,
    c("battery", "notes", "respval", "loc_res", "mood")
  )
  expect_equal(nrow(tab), 5L)
})

test_that("only the selected participant's visits appear", {
  tab1 <- build_tab(make_two_ptid_fixture(), "adrc001")
  expect_false("2021-06-15" %in% names(tab1))

  tab2 <- build_tab(make_two_ptid_fixture(), "adrc002")
  expect_setequal(names(tab2), c("label", "variable", "2021-06-15"))
})

test_that("visits without a value for a domain are NA", {
  tab <- build_tab()
  expect_true(is.na(cell(tab, "notes", "2020-03-01")))
  expect_true(is.na(cell(tab, "loc_res", "2020-03-01")))
})

# ---------------------------------------------------------------------------
# behavioral_server() -- cell contents
# ---------------------------------------------------------------------------

test_that("battery cells show checked/unchecked icons with the detail tooltip", {
  tab <- build_tab()

  done <- cell(tab, "battery", "2021-05-01")
  expect_true(has(done, "bi-check-lg"))
  expect_true(has(done, "All tests were completed"))

  partial <- cell(tab, "battery", "2020-03-01")
  expect_true(has(partial, "bi-x-circle"))
})

test_that("single-value domains are plain formatted cells, not lists", {
  tab <- build_tab()

  rv1 <- cell(tab, "respval", "2021-05-01")
  expect_true(has(rv1, "Questionably valid"))
  expect_false(has(rv1, "<ul"))

  expect_true(has(cell(tab, "respval", "2020-03-01"), "Very valid"))
  expect_true(has(cell(tab, "notes", "2021-05-01"), "Cooperative"))
})

test_that("checkbox domains become a bulleted list per visit", {
  tab <- build_tab()

  reasons <- cell(tab, "loc_res", "2021-05-01")
  expect_true(startsWith(
    reasons,
    '<ul style="margin-top: 8px; margin-bottom: 8px;">'
  ))
  expect_true(endsWith(reasons, "</ul>"))
  expect_equal(lengths(regmatches(reasons, gregexpr("<li>", reasons))), 2L)
  expect_true(has(reasons, "Distractions"))
  expect_true(has(reasons, "Other"))

  mood <- cell(tab, "mood", "2021-05-01")
  expect_equal(lengths(regmatches(mood, gregexpr("<li>", mood))), 1L)
  expect_true(has(mood, "Happy/positive"))
  expect_true(has(cell(tab, "mood", "2020-03-01"), "Irritable/angry"))
})

test_that("the 'Other' reason carries the free-text explanation as a tooltip", {
  # respothx -> extra_info -> list(value, extra_info) -> format_date_cell()
  tab <- build_tab()
  reasons <- cell(tab, "loc_res", "2021-05-01")
  expect_true(has(reasons, 'data-bs-title="Needed a break"'))
})

test_that("each cell matches format_date_cell() on the packed value", {
  # Pins the hand-off: value + extra_info are packed into a list and passed to
  # format_date_cell(), with extra_info omitted when NA.
  tab <- build_tab()
  expect_identical(
    cell(tab, "respval", "2021-05-01"),
    as.character(format_date_cell(list(value = "Questionably valid")))
  )
  expect_identical(
    cell(tab, "battery", "2021-05-01"),
    as.character(format_date_cell(list(
      value = "checked",
      extra_info = "All tests were completed"
    )))
  )
})

# ---------------------------------------------------------------------------
# behavioral_server() -- reactivity
# ---------------------------------------------------------------------------

test_that("the table follows the selected participant", {
  calls <- local_table_capture()
  ptid <- shiny::reactiveVal("adrc001")

  shiny::testServer(
    behavioral_server,
    args = list(dat = shiny::reactive(make_two_ptid_fixture()), ptid = ptid),
    {
      session$flushReact()
      output$table
      expect_true("2021-05-01" %in% names(last_tab(calls)))

      ptid("adrc002")
      session$flushReact()
      output$table
      expect_setequal(
        names(last_tab(calls)),
        c("label", "variable", "2021-06-15")
      )
    }
  )
})

test_that("the table updates when the data changes", {
  calls <- local_table_capture()
  dat <- shiny::reactiveVal(make_respval_fixture())

  shiny::testServer(
    behavioral_server,
    args = list(dat = dat, ptid = shiny::reactive("adrc002")),
    {
      session$flushReact()
      output$table
      expect_equal(nrow(last_tab(calls)), 0L) # adrc002 dropped (NA checklist)

      dat(make_two_ptid_fixture())
      session$flushReact()
      output$table
      expect_gt(nrow(last_tab(calls)), 0L)
    }
  )
})

# ---------------------------------------------------------------------------
# behavioral_server() -- real render (integration with behavioral_table())
# ---------------------------------------------------------------------------

test_that("the real render produces the scoped behavior table", {
  shiny::testServer(
    behavioral_server,
    args = list(
      dat = shiny::reactive(make_respval_fixture()),
      ptid = shiny::reactive("adrc001")
    ),
    {
      session$flushReact()
      html <- as.character(output$table$html)
      expect_true(has(html, "behaviorTable-container"))
      expect_true(has(html, "Happy/positive"))
      expect_true(has(html, "Questionably valid"))
    }
  )
})

# ---------------------------------------------------------------------------
# behavioralApp()
# ---------------------------------------------------------------------------

test_that("behavioralApp() builds a shiny app from loaded data", {
  loaded <- NULL
  local_mocked_bindings(
    load_wadrc_source = function(...) {
      loaded <<- list(...)
      "loaded-data"
    }
  )
  local_mocked_bindings(prepare_data = function(x) x, .package = "ntrd")

  app <- behavioralApp()
  expect_s3_class(app, "shiny.appobj")
  expect_length(loaded, 3L) # the three UDS tokens, from options()
})
