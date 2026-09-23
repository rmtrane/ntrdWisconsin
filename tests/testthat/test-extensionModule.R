# test-extensionModule.R  (Shiny modules, #3)
#
# Covers R/extensionModule.R -> extension_ui(), extension_server() (testServer),
# extension_app().
#
# The child modules (biomarker_ui/_server from biomarkerModule.R,
# behavioral_ui/_server from behavioralModule.R) have their own tests. Here they
# are replaced by recorders, so these tests check only the wiring: ids, JSON
# templates, batch loading, and what each child receives.
#
# How ntrd uses this module (appServer.R): it calls extras$extension_ui(), which
# wadrc_source_server() builds with id = "wisconsin-extension", and calls
# extras$extension_server(ptid =, dat =, extras =) by name, WITHOUT an id. So
# the UI and server only pair up because extension_server()'s default id is
# also "wisconsin-extension".
#
# BUG (the last test fails until fixed): extension_app() accepts `dat` but never
# passes it to extension_server(). The real behavioral_server() then errors in
# its observer ('argument "dat" is missing'), which ends the app's session.
# Fix: add `dat = dat,` to the extension_server() call in extension_app().

ext_id <- "wisconsin-extension"
bio_ids <- c("Plasma", "CSF", "Visual Ratings") # display order in the UI

panel_title <- function(p) p$attribs$title

# Replace the four child modules with recorders for the calling test. Server
# recorders store the fully namespaced id, so they can be compared with the UI.
local_child_recorders <- function(env = parent.frame()) {
  rec <- new.env()
  rec$ui <- character()
  rec$bio <- list()
  rec$beh <- NULL
  ns_id <- function(id) shiny::getDefaultReactiveDomain()$ns(id)

  local_mocked_bindings(
    biomarker_ui = function(id, title) {
      rec$ui <- c(rec$ui, id)
      shiny::div(id = id)
    },
    behavioral_ui = function(id) {
      rec$ui <- c(rec$ui, id)
      shiny::div(id = id)
    },
    biomarker_server = function(
      id,
      ptid,
      batch_loading,
      api_token,
      base_query_file
    ) {
      rec$bio[[id]] <- list(
        ns_id = ns_id(id),
        ptid = ptid,
        batch_loading = batch_loading,
        api_token = api_token,
        base_query_file = base_query_file
      )
    },
    behavioral_server = function(id, dat, ptid) {
      rec$beh <- list(ns_id = ns_id(id), dat = dat, ptid = ptid)
    },
    .env = env
  )
  rec
}

# Run extension_server() the way ntrd does (named ptid/dat/extras) under
# testServer(), with the default id made explicit. Returns the recorders and
# the reactives passed in.
run_server <- function(env = parent.frame()) {
  rec <- local_child_recorders(env = env)
  ptid <- shiny::reactive("adrc00006")
  dat <- shiny::reactive("the data")
  extras <- shiny::reactive(list(panda_api_token = "pk"))
  shiny::testServer(
    extension_server,
    args = list(id = ext_id, ptid = ptid, dat = dat, extras = extras),
    {}
  )
  list(rec = rec, ptid = ptid, dat = dat)
}

# ---------------------------------------------------------------------------
# extension_ui()
# ---------------------------------------------------------------------------

test_that("extension_ui() gives a Biomarkers and a Behavioral Checklist panel", {
  local_child_recorders()
  ui <- extension_ui()
  expect_type(ui, "list")
  expect_identical(
    vapply(ui, panel_title, character(1)),
    c("Biomarkers", "Behavioral Checklist")
  )
})

test_that("extension_ui() namespaces every section under its id", {
  rec <- local_child_recorders()
  extension_ui()
  expect_identical(rec$ui, shiny::NS(ext_id, c(bio_ids, "behavior")))

  rec$ui <- character()
  extension_ui(id = "x")
  expect_identical(rec$ui, shiny::NS("x", c(bio_ids, "behavior")))
})

test_that("extension_ui(pa = FALSE) drops the Biomarkers panel", {
  rec <- local_child_recorders()
  ui <- extension_ui(pa = FALSE)
  expect_identical(
    vapply(ui, panel_title, character(1)),
    "Behavioral Checklist"
  )
  expect_identical(rec$ui, shiny::NS(ext_id, "behavior"))
})

# ---------------------------------------------------------------------------
# extension_server()
# ---------------------------------------------------------------------------

test_that("extension_server()'s default id is the one the UI hook uses", {
  # ntrd calls extension_server() without an id (see header).
  expect_identical(formals(extension_server)$id, ext_id)
  expect_identical(formals(extension_ui)$id, ext_id)
})

test_that("every UI section has a server under the same namespace", {
  rec <- local_child_recorders()
  extension_ui()
  ui_ids <- rec$ui

  srv <- run_server()$rec
  server_ids <- c(vapply(srv$bio, `[[`, "", "ns_id"), srv$beh$ns_id)
  expect_setequal(server_ids, ui_ids)
})

test_that("each biomarker server gets its own JSON template and batch setting", {
  bio <- run_server()$rec$bio
  expect_setequal(names(bio), bio_ids)

  expected <- list(
    CSF = list(json = "csf.json", batch = TRUE),
    Plasma = list(json = "plasma.json", batch = TRUE),
    `Visual Ratings` = list(json = "visual_ratings.json", batch = FALSE)
  )
  for (id in bio_ids) {
    file <- bio[[id]]$base_query_file
    expect_identical(
      file,
      system.file("json", expected[[id]]$json, package = "ntrdWisconsin"),
      label = id
    )
    # system.file() returns "" for a missing file, which would pass silently.
    expect_true(file.exists(file), label = id)
    expect_identical(bio[[id]]$batch_loading, expected[[id]]$batch, label = id)
  }
})

test_that("ptid and dat are passed through unchanged", {
  run <- run_server()
  for (id in bio_ids) {
    expect_identical(run$rec$bio[[id]]$ptid, run$ptid, label = id)
  }
  expect_identical(run$rec$beh$ptid, run$ptid)
  expect_identical(run$rec$beh$dat, run$dat)
})

test_that("the biomarker servers read the Panda key from extras(), live", {
  rec <- local_child_recorders()
  extras <- shiny::reactiveVal(list(panda_api_token = "pk"))
  shiny::testServer(
    extension_server,
    args = list(
      ptid = shiny::reactive("adrc00006"),
      dat = shiny::reactive(NULL),
      extras = extras
    ),
    {
      tokens <- lapply(rec$bio, `[[`, "api_token")
      for (id in bio_ids) {
        expect_identical(tokens[[id]](), "pk", label = id)
      }

      extras(list(panda_api_token = "pk2"))
      session$flushReact()
      for (id in bio_ids) {
        expect_identical(tokens[[id]](), "pk2", label = id)
      }
    }
  )
})

# ---------------------------------------------------------------------------
# extension_app()  (dev wrapper)
# ---------------------------------------------------------------------------

# Replace everything extension_app() touches outside the app. Returns a
# recorder environment: daemons (mirai::daemons() calls), on_stop (the
# registered callback), inserted (nav_insert() calls), server (the
# extension_server() call), ui_id.
local_app_mocks <- function(
  ext_ui = list(bslib::nav_panel("A"), bslib::nav_panel("B")),
  env = parent.frame()
) {
  rec <- new.env()
  rec$daemons <- list()
  rec$inserted <- list()

  local_mocked_bindings(
    shinyAddResources = function() invisible(),
    .package = "ntrd",
    .env = env
  )
  local_mocked_bindings(
    daemons = function(n, ...) rec$daemons[[length(rec$daemons) + 1L]] <- n,
    .package = "mirai",
    .env = env
  )
  local_mocked_bindings(
    onStop = function(fun, session = NULL) {
      rec$on_stop <- fun
      function() invisible()
    },
    .package = "shiny",
    .env = env
  )
  local_mocked_bindings(
    nav_insert = function(id, nav, ...) {
      rec$inserted[[length(rec$inserted) + 1L]] <- list(id = id, nav = nav)
    },
    .package = "bslib",
    .env = env
  )
  local_mocked_bindings(
    extension_ui = function(id, ...) {
      rec$ui_id <- id
      ext_ui
    },
    extension_server = function(id, ptid, dat, extras) {
      rec$server <- list(
        id = id,
        ptid = ptid,
        extras = extras,
        dat = if (!missing(dat)) dat
      )
    },
    .env = env
  )
  rec
}

test_that("extension_app() requires biomarker_api to be reactive", {
  local_reproducible_output()
  expect_error(
    extension_app(biomarker_api = "pk"),
    "`biomarker_api` argument must be a reactive"
  )
})

test_that("extension_app() builds an app with one mirai daemon, stopped on exit", {
  rec <- local_app_mocks()
  app <- extension_app(
    dat = shiny::reactive(NULL),
    biomarker_api = shiny::reactive("pk")
  )

  expect_s3_class(app, "shiny.appobj")
  expect_identical(rec$daemons, list(1))
  rec$on_stop()
  expect_identical(rec$daemons, list(1, 0))
})

test_that("extension_app() inserts each extension panel and starts the server", {
  rec <- local_app_mocks()
  app <- extension_app(
    dat = shiny::reactive(NULL),
    biomarker_api = shiny::reactive("pk")
  )

  shiny::testServer(app, {
    session$setInputs(current_studyid = "adrc00010")

    expect_identical(rec$ui_id, "ext-module")
    expect_identical(
      vapply(rec$inserted, `[[`, "", "id"),
      c("long-trends", "long-trends")
    )
    expect_identical(
      vapply(rec$inserted, function(x) panel_title(x$nav), ""),
      c("A", "B")
    )

    expect_identical(rec$server$id, "ext-module")
    expect_identical(rec$server$ptid(), "adrc00010")
    expect_identical(rec$server$extras()$panda_api_token, "pk")
  })
})

test_that("extension_app() inserts a single (unlisted) panel as one panel", {
  # Without the shiny.tag check, the loop would iterate over the tag's parts.
  rec <- local_app_mocks(ext_ui = bslib::nav_panel("Only"))
  app <- extension_app(
    dat = shiny::reactive(NULL),
    biomarker_api = shiny::reactive("pk")
  )
  shiny::testServer(app, {
    expect_length(rec$inserted, 1L)
    expect_identical(panel_title(rec$inserted[[1]]$nav), "Only")
  })
})

test_that("extension_app() passes `dat` on to extension_server()", {
  # Regression test for the bug in the header: fails until it is fixed.
  rec <- local_app_mocks()
  dat <- shiny::reactive("the data")
  app <- extension_app(dat = dat, biomarker_api = shiny::reactive("pk"))
  shiny::testServer(app, {
    expect_identical(rec$server$dat, dat)
  })
})
