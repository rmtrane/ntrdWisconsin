# test-biomarkerModule.R  (Shiny modules, #4)
#
# Covers R/biomarkerModule.R -> biomarker_ui(), biomarker_server()
# (testServer), biomarker_app().
#
# ASYNC: the module runs get_biomarker_data() in an ExtendedTask backed by
# mirai. Here mirai::mirai() is replaced by a stand-in that evaluates the real
# task expression synchronously (so the real call to get_biomarker_data() runs,
# against a mock) and returns a promise. By default the promise is already
# resolved, so one session$flushReact() takes the task to "success". With
# hold = TRUE it stays pending until rec$release(), which is how "running"
# states and cancellation are tested. mirai::stop_mirai() rejects the pending
# promise with the error real mirai gives ("20 | Operation canceled").
#
# The collaborators (bio_tab_for_gt(), bio_tab_to_html_table(),
# get_all_densities(), get_all_cuts(), long_bio_plot(), add_row_titles(),
# on_render()) have their own tests. Here they are recorders, and these tests
# check what the module passes them.
#
# FAILED PULLS: get_biomarker_data() returns failures as values (a try-error,
# e.g. for a wrong or expired key, or an HTTP status code), and the task can
# also throw. Either way the module records a message (pull_failure_message())
# and both outputs show it in a one-row table (message_table()) for any
# participant without cached data. The next successful pull clears it.
# (Regression: returned failures used to error in an observer and end the
# session; see "a failed pull shows its error ...".)

bio_id <- "CSF"

# Rows for three participants. adrc003 has no values for the plot variables;
# other_raw is not a plot variable.
make_bio_result <- function() {
  data.table::data.table(
    enumber = c("adrc001", "adrc001", "adrc002", "adrc003"),
    date = as.Date(c("2020-01-01", "2021-01-01", "2020-06-01", "2022-01-01")),
    csf_ratio_lumi_ab42_ab40_fda_raw = c(0.05, 0.06, 0.07, NA),
    csf_ratio_roche_ptau181_ab42_local_raw = c(0.02, NA, 0.03, NA),
    other_raw = c(1, 2, 3, 4)
  )
}

plot_vars <- c(
  "csf_ratio_lumi_ab42_ab40_fda_raw",
  "csf_ratio_roche_ptau181_ab42_local_raw"
)

# What the get_all_densities() / get_all_cuts() stand-ins return.
dens_stub <- stats::setNames(as.list(paste0("dens:", plot_vars)), plot_vars)
cuts_stub <- list(data.table::data.table(
  name = c(
    "csf_ratio_lumi_ab42_ab40_fda",
    "csf_ratio_roche_ptau181_ab42_local"
  ),
  cut = c("cut-lumi", "cut-roche")
))

# Replace the pull (mirai + get_biomarker_data) and every collaborator with
# recorders for the calling test. `result` is what get_biomarker_data()
# returns: a data.table is filtered to the requested participants, a condition
# is thrown, and a function is called with `adrc_ptids` first. Returns the
# recorder environment.
local_module_stubs <- function(
  result = make_bio_result(),
  hold = FALSE,
  env = parent.frame()
) {
  rec <- new.env()
  rec$result <- result
  rec$fetches <- list()
  rec$handles <- list()
  rec$pending <- list()
  rec$stopped <- list()
  rec$tables <- list()
  rec$for_gt <- list()
  rec$plots <- list()
  rec$release <- function() {
    for (p in rec$pending) {
      p$resolve()
    }
    rec$pending <- list()
  }

  local_mocked_bindings(
    mirai = function(.expr, ..., .args = list()) {
      value <- tryCatch(
        eval(substitute(.expr), list2env(.args, parent = baseenv())),
        error = identity
      )
      handle <- if (inherits(value, "error")) {
        promises::promise_reject(value)
      } else if (!hold) {
        promises::promise_resolve(value)
      } else {
        promises::promise(function(resolve, reject) {
          rec$pending[[length(rec$pending) + 1L]] <- list(
            resolve = function() resolve(value),
            reject = function() reject(simpleError("20 | Operation canceled"))
          )
        })
      }
      rec$handles[[length(rec$handles) + 1L]] <- handle
      handle
    },
    stop_mirai = function(x) {
      rec$stopped[[length(rec$stopped) + 1L]] <- x
      i <- which(vapply(rec$handles, identical, logical(1), x))
      if (length(i) && length(rec$pending) >= 1L) {
        rec$pending[[1]]$reject()
        rec$pending[[1]] <- NULL
      }
    },
    .package = "mirai",
    .env = env
  )

  local_mocked_bindings(
    get_biomarker_data = function(api_key, base_query_file, adrc_ptids = NULL) {
      rec$fetches[[length(rec$fetches) + 1L]] <- list(
        api_key = api_key,
        base_query_file = base_query_file,
        adrc_ptids = adrc_ptids
      )
      res <- if (is.function(rec$result)) rec$result(adrc_ptids) else rec$result
      if (inherits(res, "error")) {
        stop(res)
      }
      if (data.table::is.data.table(res) && !is.null(adrc_ptids)) {
        res <- res[res$enumber %in% adrc_ptids]
      }
      res
    },
    get_all_densities = function(x) {
      rec$dens_in <- x
      dens_stub
    },
    get_all_cuts = function(x) {
      rec$cuts_in <- x
      cuts_stub
    },
    bio_tab_for_gt = function(tab) {
      rec$for_gt[[length(rec$for_gt) + 1L]] <- tab
      "for-gt"
    },
    bio_tab_to_html_table = function(
      tab_for_gt,
      densities = NULL,
      cuts = NULL,
      print_x = TRUE,
      tab_header = NULL,
      tab_id = NULL
    ) {
      rec$tables[[length(rec$tables) + 1L]] <- list(
        tab_for_gt = tab_for_gt,
        densities = densities,
        cuts = cuts,
        print_x = print_x,
        tab_header = tab_header,
        tab_id = tab_id
      )
      label <- if (data.table::is.data.table(tab_for_gt)) {
        tab_for_gt$name_label
      } else {
        tab_for_gt
      }
      shiny::div("html-table", label)
    },
    long_bio_plot = function(
      dat,
      y_val,
      dens,
      cuts,
      height = 100,
      width = 400,
      new_id = NULL
    ) {
      rec$plots[[length(rec$plots) + 1L]] <- list(
        dat = dat,
        y_val = y_val,
        dens = dens,
        cuts = cuts,
        new_id = new_id
      )
      p <- plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers")
      list(scatter = p, density = p)
    },
    add_row_titles = function(p, titles) {
      rec$titles <- titles
      p
    },
    on_render = function(p, js) {
      rec$js <- js
      p
    },
    .env = env
  )
  rec
}

module_args <- function(
  batch = TRUE,
  file = "csf.json",
  ptid = shiny::reactive("adrc001"),
  api = shiny::reactive("pk")
) {
  list(
    id = bio_id,
    base_query_file = file,
    batch_loading = batch,
    ptid = ptid,
    api_token = api
  )
}

# Flush until the event loop is idle. Each session$flushReact() runs one
# reactive flush and one pass of the later loop, i.e. one step of a promise
# chain; how many steps a finished pull takes depends on the shiny version.
settle <- function(session, max = 50) {
  idle <- 0
  for (i in seq_len(max)) {
    session$flushReact()
    idle <- if (later::loop_empty()) idle + 1 else 0
    if (idle == 2) return(invisible())
  }
  stop("the session did not settle")
}

# settle() after stopping a running pull. A stopped pull ends the task in
# "error", and shiny >= 1.11 warns on every task error ("An error occurred
# when invoking the ExtendedTask"). That warning is expected here, so only it
# is muffled; a calling handler (unlike expect_warning()) doesn't unwind the
# promise callback that raised it.
settle_after_cancel <- function(session) {
  withCallingHandlers(
    settle(session),
    warning = function(w) {
      if (is_cancellation(w$parent)) invokeRestart("muffleWarning")
    }
  )
}

html_of <- function(x) paste(as.character(x$html), collapse = "")
has <- function(x, pattern) grepl(pattern, x, fixed = TRUE)
fetched_ptids <- function(rec) lapply(rec$fetches, `[[`, "adrc_ptids")

# ---------------------------------------------------------------------------
# biomarker_ui()
# ---------------------------------------------------------------------------

test_that("biomarker_ui() has a Table and a Plot tab with namespaced outputs", {
  html <- as.character(biomarker_ui("CSF"))
  expect_true(has(html, 'id="CSF-table"'))
  expect_true(has(html, 'id="CSF-long_plot"'))
  expect_true(has(html, 'data-value="Table"'))
  expect_true(has(html, 'data-value="Plot"'))
})

test_that("biomarker_ui() shows only the table for Visual Ratings", {
  html <- as.character(biomarker_ui("x-Visual Ratings"))
  expect_true(has(html, 'id="x-Visual Ratings-table"'))
  expect_false(has(html, "long_plot"))
})

# ---------------------------------------------------------------------------
# biomarker_server() -- when and what it fetches
# ---------------------------------------------------------------------------

test_that("batch modules wait for the key, then fetch everyone once", {
  rec <- local_module_stubs()
  api <- shiny::reactiveVal(NULL)
  ptid <- shiny::reactiveVal("adrc001")

  shiny::testServer(
    biomarker_server,
    args = module_args(ptid = ptid, api = api),
    {
      settle(session)
      expect_length(rec$fetches, 0L)

      api("pk")
      settle(session)
      expect_identical(
        rec$fetches,
        list(list(
          api_key = "pk",
          base_query_file = "csf.json",
          adrc_ptids = NULL
        ))
      )

      ptid("adrc002") # every participant is already loaded
      settle(session)
      expect_length(rec$fetches, 1L)

      api("pk2") # a new key fetches again
      settle(session)
      expect_length(rec$fetches, 2L)
      expect_identical(rec$fetches[[2]]$api_key, "pk2")
    }
  )
})

test_that("non-batch modules fetch per participant and reuse cached results", {
  rec <- local_module_stubs()
  ptid <- shiny::reactiveVal("adrc001")

  shiny::testServer(
    biomarker_server,
    args = module_args(batch = FALSE, ptid = ptid),
    {
      settle(session)
      ptid("adrc002")
      settle(session)
      ptid("adrc001")
      settle(session)
      expect_identical(fetched_ptids(rec), list("adrc001", "adrc002"))
    }
  )
})

test_that("batch_loading defaults to FALSE for visual ratings, TRUE otherwise", {
  rec <- local_module_stubs()
  shiny::testServer(
    biomarker_server,
    args = module_args(batch = NULL, file = "visual_ratings.json"),
    {
      settle(session)
    }
  )
  shiny::testServer(
    biomarker_server,
    args = module_args(batch = NULL, file = "csf.json"),
    {
      settle(session)
    }
  )
  expect_identical(fetched_ptids(rec), list("adrc001", NULL))
})

test_that("a new request stops the pull that is still running", {
  rec <- local_module_stubs(hold = TRUE)
  ptid <- shiny::reactiveVal("adrc001")

  shiny::testServer(
    biomarker_server,
    args = module_args(batch = FALSE, ptid = ptid),
    {
      settle(session)
      expect_identical(bio_dat$status(), "running")

      ptid("adrc002")
      settle_after_cancel(session)
      expect_identical(rec$stopped, rec$handles[1])
      expect_identical(fetched_ptids(rec), list("adrc001", "adrc002"))

      rec$release()
      settle(session)
      expect_identical(bio_dat$status(), "success")
    }
  )
})

test_that("stopping a pull is not an error, even when no new pull replaces it", {
  # Stopping a pull ends the task in "error". Here the user switches back to a
  # cached participant, so no new pull replaces it and the "error" stays. It
  # must not be reported as a failure, and nothing may re-throw it (the plot
  # used to). Both outputs must show the cached participant.
  rec <- local_module_stubs(hold = TRUE, result = function(ptids) {
    if (identical(ptids, "adrc003")) simpleError("boom") else make_bio_result()
  })
  ptid <- shiny::reactiveVal("adrc001")

  shiny::testServer(
    biomarker_server,
    args = module_args(batch = FALSE, ptid = ptid),
    {
      settle(session)
      rec$release() # adrc001 is now cached
      settle(session)

      ptid("adrc002") # starts a pull that is still running ...
      settle(session)
      ptid("adrc001") # ... and is stopped by going back to a cached participant
      settle_after_cancel(session)

      expect_identical(bio_dat$status(), "error")
      expect_true(is_cancellation(tryCatch(bio_dat$result(), error = identity)))
      expect_length(rec$stopped, 1L)
      expect_no_warning(expect_no_error(table_html <- html_of(output$table)))
      expect_no_warning(expect_no_error(plot_html <- html_of(output$long_plot)))
      expect_true(has(table_html, "for-gt"))
      expect_false(has(table_html, "Could not retrieve"))
      expect_equal(
        rec$plots[[length(rec$plots)]]$dat,
        make_bio_result()[enumber == "adrc001"]
      )
      expect_null(pull_error()) # the stopped pull is not recorded as a failure

      ptid("adrc003") # a real error after a cancellation is still reported
      suppressWarnings(settle(session)) # shiny >= 1.11 warns about real errors
      expect_match(pull_error(), "boom")
    }
  )
})

# ---------------------------------------------------------------------------
# biomarker_server() -- the table
# ---------------------------------------------------------------------------

test_that("the table is built from the participant's rows, densities and cuts", {
  rec <- local_module_stubs()
  shiny::testServer(
    biomarker_server,
    args = module_args(ptid = shiny::reactive("adrc002")),
    {
      settle(session)
      html <- html_of(output$table)

      expect_true(has(html, "html-table"))
      expect_equal(
        rec$for_gt[[length(rec$for_gt)]],
        make_bio_result()[enumber == "adrc002"]
      )
      call <- rec$tables[[length(rec$tables)]]
      expect_identical(call$tab_for_gt, "for-gt")
      expect_identical(call$densities, dens_stub)
      expect_identical(call$cuts, cuts_stub[[1]])
      expect_false(call$print_x)
      expect_identical(call$tab_header, "")
      expect_identical(call$tab_id, bio_id)
    }
  )
})

test_that("batch modules compute densities and cuts from the whole result", {
  rec <- local_module_stubs()
  shiny::testServer(biomarker_server, args = module_args(), {
    settle(session)
    # ignore_attr: subsetting by enumber gives the result a data.table index.
    expect_equal(rec$dens_in, make_bio_result(), ignore_attr = TRUE)
    expect_named(rec$cuts_in, "csf") # named after the template
    expect_equal(rec$cuts_in$csf, make_bio_result(), ignore_attr = TRUE)
  })
})

test_that("non-batch modules compute no densities or cuts", {
  # Densities need the whole population; a non-batch pull has one participant.
  for (file in c("csf.json", "visual_ratings.json")) {
    rec <- local_module_stubs()
    shiny::testServer(
      biomarker_server,
      args = module_args(batch = FALSE, file = file),
      {
        settle(session)
        expect_null(rec$dens_in, label = file)
        expect_null(rec$cuts_in, label = file)
      }
    )
  }
})

test_that("the table shows a loading message while the pull runs", {
  rec <- local_module_stubs(hold = TRUE)
  shiny::testServer(biomarker_server, args = module_args(), {
    settle(session)
    html <- html_of(output$table)
    expect_true(has(html, "Loading data"))
    expect_length(rec$tables, 0L)
  })
})

test_that("a non-batch participant without data gets an empty table", {
  rec <- local_module_stubs()
  shiny::testServer(
    biomarker_server,
    args = module_args(batch = FALSE, ptid = shiny::reactive("adrc009")),
    {
      settle(session)
      expect_identical(suppressMessages(html_of(output$table)), "")
      expect_length(rec$tables, 0L)
    }
  )
})

# ---------------------------------------------------------------------------
# biomarker_server() -- the plot
# ---------------------------------------------------------------------------

test_that("the plot shows a loading message while the pull runs", {
  rec <- local_module_stubs(hold = TRUE)
  shiny::testServer(biomarker_server, args = module_args(), {
    settle(session)
    expect_true(has(html_of(output$long_plot), "Loading data"))
  })
})

test_that("each plot variable in the result gets a row for the participant", {
  rec <- local_module_stubs()
  shiny::testServer(biomarker_server, args = module_args(), {
    settle(session)
    html <- html_of(output$long_plot)

    expect_identical(vapply(rec$plots, `[[`, "", "y_val"), plot_vars) # other_raw skipped
    for (i in seq_along(plot_vars)) {
      call <- rec$plots[[i]]
      expect_equal(call$dat, make_bio_result()[enumber == "adrc001"])
      expect_identical(call$dens, dens_stub[[i]])
      expect_equal(call$cuts, cuts_stub[[1]][i])
      expect_identical(call$new_id, plot_vars[i])
    }
    expect_length(rec$titles, 2L)
    expect_match(rec$titles[1], "Lumipulse")
    expect_identical(rec$js, long_bio_plot_js)
    expect_true(has(html, paste0('id="', bio_id, '"'))) # elementId = module id
  })
})

test_that("a non-batch module plots the selected participant, not the last pull", {
  rec <- local_module_stubs()
  ptid <- shiny::reactiveVal("adrc001")
  shiny::testServer(
    biomarker_server,
    args = module_args(batch = FALSE, ptid = ptid),
    {
      settle(session)
      ptid("adrc002")
      settle(session)
      ptid("adrc001") # cached; the latest pull was for adrc002
      settle(session)
      output$long_plot
      expect_equal(
        rec$plots[[length(rec$plots)]]$dat,
        make_bio_result()[enumber == "adrc001"]
      )
    }
  )
})

test_that("the plot says 'No values found' when the result has no plot variables", {
  res <- make_bio_result()[, c("enumber", "date", "other_raw")]
  rec <- local_module_stubs(result = res)
  shiny::testServer(biomarker_server, args = module_args(), {
    settle(session)
    expect_true(has(html_of(output$long_plot), "No values found"))
    expect_length(rec$plots, 0L)
  })
})

test_that("the plot says 'No values found' when the participant has no plot values", {
  rec <- local_module_stubs()
  shiny::testServer(
    biomarker_server,
    args = module_args(ptid = shiny::reactive("adrc003")),
    {
      settle(session)
      expect_true(has(html_of(output$long_plot), "No values found"))
      expect_length(rec$plots, 0L)
    }
  )
})

test_that("the plot's fallback (non-batch participant without data) doesn't error", {
  # Regression: the cli message used to miss its closing "}".
  rec <- local_module_stubs()
  shiny::testServer(
    biomarker_server,
    args = module_args(batch = FALSE, ptid = shiny::reactive("adrc009")),
    {
      settle(session)
      expect_no_error(suppressMessages(output$long_plot))
    }
  )
})

# ---------------------------------------------------------------------------
# biomarker_server() -- failed pulls
# ---------------------------------------------------------------------------

failures <- list(
  try_error = try(stop("HTTP 401 Unauthorized."), silent = TRUE),
  status = 204L,
  thrown = simpleError("cannot open file 'csf.json'")
)

test_that("a failed pull shows its error in the table and the plot", {
  for (nm in names(failures)) {
    rec <- local_module_stubs(result = failures[[nm]])
    shiny::testServer(biomarker_server, args = module_args(), {
      suppressWarnings(settle(session))
      expect_false(session$isClosed(), label = nm) # used to end the session

      msg <- pull_failure_message(failures[[nm]])
      expect_true(has(html_of(output$table), msg), label = nm)
      expect_true(has(html_of(output$long_plot), msg), label = nm)
      expect_length(rec$for_gt, 0L)
    })
  }
})

test_that("the error is only shown for participants without data", {
  rec <- local_module_stubs(result = function(ptids) {
    if (identical(ptids, "adrc002")) failures$try_error else make_bio_result()
  })
  ptid <- shiny::reactiveVal("adrc001")

  shiny::testServer(
    biomarker_server,
    args = module_args(batch = FALSE, ptid = ptid),
    {
      settle(session)
      ptid("adrc002") # fails
      settle(session)
      expect_match(html_of(output$table), "401")

      ptid("adrc001") # cached from before, so its table is shown
      settle(session)
      html <- html_of(output$table)
      expect_true(has(html, "for-gt"))
      expect_false(has(html, "401"))
    }
  )
})

test_that("a successful pull clears the error", {
  rec <- local_module_stubs(result = failures$try_error)
  api <- shiny::reactiveVal("expired-key")
  # adrc009 has no rows even after the good pull, so only clearing the error
  # (not the cache) keeps the stale message away.
  ptid <- shiny::reactive("adrc009")

  shiny::testServer(
    biomarker_server,
    args = module_args(api = api, ptid = ptid),
    {
      settle(session)
      expect_match(html_of(output$table), "401")

      rec$result <- make_bio_result()
      api("new-key")
      settle(session)
      html <- html_of(output$table)
      expect_true(has(html, "for-gt"))
      expect_false(has(html, "401"))
    }
  )
})

test_that("is_cancellation() recognises only a stopped mirai", {
  expect_true(is_cancellation(simpleError("20 | Operation canceled")))
  expect_false(is_cancellation(simpleError("HTTP 401 Unauthorized.")))
  expect_false(is_cancellation(simpleError("5 | Timed out"))) # a real failure
  expect_false(is_cancellation("20 | Operation canceled")) # not a condition
})

test_that("pull_failure_message() explains each kind of failure", {
  expect_identical(
    pull_failure_message(failures$try_error),
    "Could not retrieve data from Panda: HTTP 401 Unauthorized."
  )
  expect_identical(
    pull_failure_message(204L),
    "Could not retrieve data from Panda: HTTP status 204"
  )
  expect_identical(
    pull_failure_message(failures$thrown),
    "Could not retrieve data from Panda: cannot open file 'csf.json'"
  )
  # A try-error without its condition attribute falls back to its text.
  bare <- structure("Error : timed out\n", class = "try-error")
  expect_identical(
    pull_failure_message(bare),
    "Could not retrieve data from Panda: Error : timed out"
  )
})

# ---------------------------------------------------------------------------
# biomarker_app()  (dev wrapper)
# ---------------------------------------------------------------------------

local_app_mocks <- function(daemons_set = FALSE, env = parent.frame()) {
  rec <- new.env()
  rec$daemons <- list()
  rec$servers <- list()
  local_mocked_bindings(
    daemons_set = function() daemons_set,
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
    biomarker_server = function(
      id,
      base_query_file,
      batch_loading,
      ptid,
      api_token
    ) {
      rec$servers[[id]] <- list(
        file = base_query_file,
        batch = batch_loading,
        ptid = ptid,
        api = api_token
      )
    },
    .env = env
  )
  rec
}

test_that("biomarker_app() requires biomarker_api to be reactive", {
  local_reproducible_output()
  expect_error(biomarker_app(biomarker_api = "pk"), "must be a reactive")
})

test_that("biomarker_app() resets any daemons, starts one, and stops it on exit", {
  rec <- local_app_mocks(daemons_set = TRUE)
  app <- biomarker_app(biomarker_api = shiny::reactive("pk"))
  expect_s3_class(app, "shiny.appobj")
  expect_identical(rec$daemons, list(0, 1))
  rec$on_stop()
  expect_identical(rec$daemons, list(0, 1, 0))

  rec <- local_app_mocks(daemons_set = FALSE)
  biomarker_app(biomarker_api = shiny::reactive("pk"))
  expect_identical(rec$daemons, list(1))
})

test_that("biomarker_app() runs a CSF, a Plasma and a Visual Ratings module", {
  rec <- local_app_mocks()
  key <- shiny::reactive("pk")
  app <- biomarker_app(biomarker_api = key)
  shiny::testServer(app, {
    session$setInputs(current_studyid = "adrc00010")
    s <- rec$servers
    expect_named(s, c("CSF", "Plasma", "Visual Ratings"))
    expect_identical(
      vapply(s, `[[`, logical(1), "batch"),
      c(CSF = TRUE, Plasma = TRUE, `Visual Ratings` = FALSE)
    )
    expect_identical(
      vapply(s, function(x) basename(x$file), ""),
      c(
        CSF = "csf.json",
        Plasma = "plasma.json",
        `Visual Ratings` = "visual_ratings.json"
      )
    )
    for (x in s) {
      expect_identical(x$api, key)
      expect_identical(x$ptid(), "adrc00010")
    }
  })
})
