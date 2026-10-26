# test-data_source_server.R  (Shiny modules, #1)
#
# Covers R/data_source_server.R -> data_source_server method on wadrc_source
# (generic from ntrd), via shiny::testServer().
#
# STATUS: DRAFT — not yet executed. Traced from source.
#
# NETWORK: the module checks Panda reachability once, at start-up, with
# httr2::req_perform(). Every test mocks that call (local_panda()), so the
# suite never touches the network and can exercise both the reachable and
# unreachable branches.
#
# UPDATES: shiny::updateTextInput() is mocked to record its calls, because the
# mock session doesn't reliably reflect input updates. Tests assert on what the
# module *asked* to update.
#
# DOC DRIFT: the roxygen says `extras` holds `all_values` and
# `panda_api_token`. The code creates `panda_api_token`, `extension_ui` and
# `extension_server` — no `all_values`. Tests assert the real names. The
# @description is also still the "A short description..." placeholder.
#
# NOT ASSERTED: the "toggleInput" custom message sent when Panda is
# unreachable (MockShinySession doesn't expose sent messages). The placeholder
# update and the pandaNA warning from the same observer are asserted instead.

# Mock Panda reachability for the calling test.
local_panda <- function(reachable, env = parent.frame()) {
  local_mocked_bindings(
    req_perform = function(...) {
      if (reachable) {
        structure(list(), class = "httr2_response")
      } else {
        stop("offline")
      }
    },
    .package = "httr2",
    .env = env
  )
}

# Record shiny::updateTextInput() calls for the calling test. Returns a
# function that gives the calls made so far.
local_update_recorder <- function(env = parent.frame()) {
  calls <- list()
  local_mocked_bindings(
    updateTextInput = function(
      session,
      inputId,
      label = NULL,
      value = NULL,
      placeholder = NULL
    ) {
      calls[[length(calls) + 1L]] <<- list(
        inputId = inputId,
        value = value,
        placeholder = placeholder
      )
      invisible(NULL)
    },
    .package = "shiny",
    .env = env
  )
  function() calls
}

module <- wadrc_source_server

# ---------------------------------------------------------------------------
# Return value contract (what ntrd relies on)
# ---------------------------------------------------------------------------

test_that("data_source_server() returns params, restore, extras and session", {
  local_panda(TRUE)
  shiny::testServer(module, {
    ret <- session$returned
    expect_named(
      ret,
      c("params", "restore", "extras", "session"),
      ignore.order = TRUE
    )
    expect_s3_class(ret$params, "reactive")
    expect_type(ret$restore, "closure")
    expect_s3_class(ret$extras, "reactivevalues")
  })
})

# ---------------------------------------------------------------------------
# params
# ---------------------------------------------------------------------------

test_that("params() waits until all three UDS tokens are set", {
  local_panda(TRUE)
  shiny::testServer(module, {
    session$setInputs(uds2_api_token = "t2", uds3_api_token = "t3")
    expect_error(session$returned$params(), class = "shiny.silent.error")

    session$setInputs(uds4_api_token = "t4")
    expect_no_error(session$returned$params())
  })
})

test_that("an empty token also holds params() back", {
  # req() treats "" as not ready, which matters for a cleared password field.
  local_panda(TRUE)
  shiny::testServer(module, {
    session$setInputs(
      uds2_api_token = "t2",
      uds3_api_token = "",
      uds4_api_token = "t4"
    )
    expect_error(session$returned$params(), class = "shiny.silent.error")
  })
})

test_that("params() returns all four tokens by name", {
  local_panda(TRUE)
  shiny::testServer(module, {
    session$setInputs(
      uds2_api_token = "t2",
      uds3_api_token = "t3",
      uds4_api_token = "t4",
      panda_api_token = "pk"
    )
    expect_identical(
      session$returned$params(),
      list(
        uds2_api_token = "t2",
        uds3_api_token = "t3",
        uds4_api_token = "t4",
        panda_api_token = "pk"
      )
    )
  })
})

test_that("params() doesn't require the Panda key", {
  local_panda(TRUE)
  shiny::testServer(module, {
    session$setInputs(
      uds2_api_token = "t2",
      uds3_api_token = "t3",
      uds4_api_token = "t4"
    )
    p <- session$returned$params()
    expect_identical(p$uds2_api_token, "t2")
    expect_null(p$panda_api_token)
  })
})

# ---------------------------------------------------------------------------
# Panda reachability
# ---------------------------------------------------------------------------

test_that("the Panda reachability check is mocked (guards the other tests)", {
  local_panda(TRUE)
  shiny::testServer(module, expect_true(panda_access))

  local_panda(FALSE)
  shiny::testServer(module, expect_false(panda_access))
})

test_that("unreachable Panda: key input gets a notice and a warning is shown", {
  local_panda(FALSE)
  updates <- local_update_recorder()

  shiny::testServer(module, {
    # The observer only fires once the key input exists.
    session$setInputs(panda_api_token = "")

    panda_updates <- Filter(\(x) x$inputId == "panda_api_token", updates())
    expect_length(panda_updates, 1L)
    expect_identical(
      panda_updates[[1]]$placeholder,
      "Panda server not accessible."
    )

    html <- as.character(output$pandaNA$html)
    expect_true(grepl("Panda server could not be reached", html, fixed = TRUE))
    expect_true(grepl("SMPH VPN", html, fixed = TRUE))
  })
})

test_that("reachable Panda: no notice and no warning output", {
  local_panda(TRUE)
  updates <- local_update_recorder()

  shiny::testServer(module, {
    session$setInputs(panda_api_token = "")
    expect_length(updates(), 0L)
    expect_error(output$pandaNA)
  })
})

# ---------------------------------------------------------------------------
# restore()
# ---------------------------------------------------------------------------

saved <- list(
  uds2_api_token = "s2",
  uds3_api_token = "s3",
  uds4_api_token = "s4",
  panda_api_token = "spk"
)

test_that("restore() refills all four inputs when Panda is reachable", {
  local_panda(TRUE)
  updates <- local_update_recorder()

  shiny::testServer(module, {
    session$returned$restore(saved)

    got <- updates()
    ids <- vapply(got, `[[`, character(1), "inputId")
    vals <- vapply(got, `[[`, character(1), "value")
    expect_identical(
      ids,
      c("uds2_api_token", "uds3_api_token", "uds4_api_token", "panda_api_token")
    )
    expect_identical(vals, c("s2", "s3", "s4", "spk"))
  })
})

test_that("restore() leaves the Panda key alone when Panda is unreachable", {
  local_panda(FALSE)
  updates <- local_update_recorder()

  shiny::testServer(module, {
    session$returned$restore(saved)

    ids <- vapply(updates(), `[[`, character(1), "inputId")
    expect_identical(
      ids,
      c("uds2_api_token", "uds3_api_token", "uds4_api_token")
    )
  })
})

test_that("restore()'s pa argument overrides detected reachability", {
  local_panda(TRUE)
  updates <- local_update_recorder()

  shiny::testServer(module, {
    session$returned$restore(saved, pa = FALSE)
    ids <- vapply(updates(), `[[`, character(1), "inputId")
    expect_false("panda_api_token" %in% ids)
  })
})

# ---------------------------------------------------------------------------
# extras
# ---------------------------------------------------------------------------

test_that("extras carries the Panda key when Panda is reachable", {
  local_panda(TRUE)
  shiny::testServer(module, {
    session$setInputs(panda_api_token = "pk")
    expect_identical(session$returned$extras$panda_api_token, "pk")

    session$setInputs(panda_api_token = "pk2")
    expect_identical(session$returned$extras$panda_api_token, "pk2")
  })
})

test_that("extras never carries the Panda key when Panda is unreachable", {
  local_panda(FALSE)
  local_update_recorder() # silence the placeholder update
  shiny::testServer(module, {
    session$setInputs(panda_api_token = "pk")
    expect_null(session$returned$extras$panda_api_token)
  })
})

test_that("extras provides the extension UI and server hooks", {
  local_panda(TRUE)
  shiny::testServer(module, {
    session$setInputs(panda_api_token = "")
    ex <- session$returned$extras
    expect_type(ex$extension_ui, "closure")
    expect_identical(ex$extension_server, extension_server)
    expect_setequal(
      names(ex),
      c("panda_api_token", "extension_ui", "extension_server")
    )
  })
})


test_that("data_source_server() method delegates to wadrc_source_server()", {
  local_mocked_bindings(wadrc_source_server = function(id) paste("server", id))
  expect_identical(ntrd::data_source_server(wadrc_source(), "x"), "server x")
})
