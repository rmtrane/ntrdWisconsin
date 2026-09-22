# test-data_source_ui.R  (plan item 14)
#
# Covers R/data_source_ui.R:
#   data_source_ui  [S7 method on wadrc_source, generic from ntrd]
#   api_token_inputs()  [internal]
#
# STATUS: DRAFT — not yet executed. Traced from source.
#
# The method is dispatched through the ntrd generic, the same way
# test-data_load.R calls ntrd::data_load(wadrc_source(), ...).
#
# LIKELY PACKAGE BUG — first test to look at if this file errors:
# the method body calls bare `HTML(...)` (in `shiny::tags$script(HTML(...))`),
# but ntrdWisconsin's NAMESPACE imports neither shiny nor htmltools. The call
# only resolves when shiny happens to be attached (as it is in a running app).
# Under devtools::test() it will error with 'could not find function "HTML"'
# unless something else attaches shiny. Fix: use shiny::HTML(). If these tests
# pass anyway, check whether a helper or setup file attaches shiny.
#
# HTML MATCHING: rendered attribute order isn't guaranteed, so inputs are
# located by id and then checked attribute-by-attribute.

render_ui <- function(ns = shiny::NS("ds")) {
  as.character(wadrc_source_ui(ns = ns))
}

# Return the full <input ...> tag that carries the given id (or "" if absent).
input_tag <- function(html, id) {
  m <- regmatches(
    html,
    regexpr(paste0('<input[^>]*id="', id, '"[^>]*>'), html)
  )
  if (length(m) == 0) "" else m
}

has <- function(html, pattern) grepl(pattern, html, fixed = TRUE)

count <- function(html, pattern) {
  m <- gregexpr(pattern, html, fixed = TRUE)[[1]]
  if (m[1] == -1) 0L else length(m)
}

# ---------------------------------------------------------------------------
# data_source_ui() method
# ---------------------------------------------------------------------------

test_that("data_source_ui() returns a tagList for a wadrc_source", {
  ui <- wadrc_source_ui(shiny::NS("ds"))
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("data_source_ui() renders four namespaced password inputs", {
  html <- render_ui()
  ids <- paste0(
    "ds-",
    c(
      "uds2_api_token",
      "uds3_api_token",
      "uds4_api_token",
      "panda_api_token"
    )
  )

  expect_equal(count(html, 'type="password"'), 4L)

  for (id in ids) {
    tag <- input_tag(html, id)
    expect_true(nzchar(tag), label = id)
    expect_true(has(tag, 'type="password"'), label = id)
    expect_true(has(tag, 'autocomplete="current-password"'), label = id)
  }
})

test_that("each token input has the expected visible label and placeholder", {
  html <- render_ui()
  expected <- list(
    "ds-uds2_api_token" = c("UDS-2:", "Enter REDCap API Token for UDS-2"),
    "ds-uds3_api_token" = c("UDS-3:", "Enter REDCap API Token for UDS-3"),
    "ds-uds4_api_token" = c("UDS-4:", "Enter REDCap API Token for UDS-4"),
    "ds-panda_api_token" = c("Key:", "Enter Panda API Key (optional)")
  )

  for (id in names(expected)) {
    lab <- expected[[id]][1]
    ph <- expected[[id]][2]
    expect_match(
      html,
      paste0('<label[^>]*for="', id, '"[^>]*>', lab, "</label>"),
      label = id
    )
    expect_true(
      has(input_tag(html, id), paste0('placeholder="', ph, '"')),
      label = id
    )
  }
})

test_that("data_source_ui() includes the pandaNA output placeholder", {
  html <- render_ui()
  expect_match(html, '<div[^>]*id="ds-pandaNA"[^>]*>')
  expect_true(has(html, "shiny-html-output"))
})

test_that("data_source_ui() includes the toggleInput message handler", {
  html <- render_ui()
  expect_true(has(html, "Shiny.addCustomMessageHandler('toggleInput'"))
  expect_true(has(html, "setAttribute('disabled', 'disabled')"))
})

test_that("data_source_ui() groups inputs into REDCap and Panda cards", {
  html <- render_ui()
  expect_true(has(html, "REDCap API Tokens"))
  expect_true(has(html, "Panda API Key"))
  # REDCap card precedes the Panda card, and the UDS inputs sit in the first.
  redcap_pos <- regexpr("REDCap API Tokens", html, fixed = TRUE)
  panda_pos <- regexpr("Panda API Key", html, fixed = TRUE)
  uds4_pos <- regexpr('id="ds-uds4_api_token"', html, fixed = TRUE)
  expect_lt(redcap_pos, uds4_pos)
  expect_lt(uds4_pos, panda_pos)
})

test_that("data_source_ui() uses the supplied namespace function", {
  other <- render_ui(ns = shiny::NS("other"))
  expect_true(nzchar(input_tag(other, "other-uds2_api_token")))
  expect_false(has(other, 'id="ds-uds2_api_token"'))

  # NS(NULL) is the identity: ids come through un-prefixed.
  bare <- render_ui(ns = shiny::NS(NULL))
  expect_true(nzchar(input_tag(bare, "uds2_api_token")))
  expect_true(has(bare, 'id="pandaNA"'))
})

# ---------------------------------------------------------------------------
# api_token_inputs()
# ---------------------------------------------------------------------------

test_that("api_token_inputs() builds a labelled password row with defaults", {
  html <- as.character(api_token_inputs())

  expect_match(html, '^<div class="row">')
  expect_true(has(html, 'class="col-sm-2"'))
  expect_true(has(html, 'class="col-sm-6"'))

  tag <- input_tag(html, "uds_api_token")
  expect_true(nzchar(tag))
  expect_true(has(tag, 'type="password"'))
  expect_true(has(tag, 'autocomplete="current-password"'))
  expect_true(has(tag, 'placeholder="Enter REDCap API Token for UDS-2"'))
  expect_match(html, '<label[^>]*for="uds_api_token"[^>]*>UDS-2:</label>')
})

test_that("api_token_inputs() passes label, placeholder and id through", {
  html <- as.character(api_token_inputs(
    label = "Tok:",
    placeholder = "Paste token",
    inputId = "my_tok"
  ))
  expect_true(has(input_tag(html, "my_tok"), 'placeholder="Paste token"'))
  expect_match(html, '<label[^>]*for="my_tok"[^>]*>Tok:</label>')
})

test_that("autocomplete is added to the <input>, not the wrapper div", {
  html <- as.character(api_token_inputs())
  expect_equal(count(html, 'autocomplete="current-password"'), 1L)
  expect_true(has(input_tag(html, "uds_api_token"), "autocomplete="))
})

test_that("data_source_ui() method delegates to wadrc_source_ui()", {
  local_mocked_bindings(wadrc_source_ui = function(ns) ns("probe"))
  expect_identical(
    ntrd::data_source_ui(wadrc_source(), ns = shiny::NS("ds")),
    "ds-probe"
  )
})
