# test-zzz.R
#
# Covers R/zzz.R -> .set_defaults(), .onAttach(), .onLoad()
#
# - .set_defaults() is tested with ntrs::set_std_defaults() replaced by a
#   recorder: exactly which scores it sets, and with what arguments. That
#   touches no global state.
# - One test runs the real ntrs. It does not restore state because it only
#   reasserts what .onAttach() already set when the package was attached
#   (library() in testthat.R, or devtools::load_all()).
# - .onLoad()/.onAttach() run before covr instruments the package, so they are
#   called directly here with their single callee mocked.

# The 11 scores that ntrdWisconsin standardizes with T-scores (ntrsTscores).
tscore_classes <- c(
  "WAIS",
  "REY1REC",
  "REY2REC",
  "REY3REC",
  "REY4REC",
  "REY5REC",
  "REY6REC",
  "REYDREC",
  "REYTOTAL",
  "REYAREC",
  "REYDLIST"
)

new_scores <- function(nm) getExportedValue("ntrs", nm)()

test_that(".set_defaults() sets 'tscores' for exactly the 11 T-score tests", {
  calls <- list()
  local_mocked_bindings(
    set_std_defaults = function(
      scores,
      method,
      version = NULL,
      overwrite = FALSE
    ) {
      calls[[length(calls) + 1L]] <<- list(
        class = S7::S7_class(scores)@name,
        method = method,
        version = version,
        overwrite = overwrite
      )
    },
    .package = "ntrs"
  )

  .set_defaults()

  field <- function(f) lapply(calls, `[[`, f)
  expect_length(calls, length(tscore_classes))
  expect_setequal(unlist(field("class")), tscore_classes)
  expect_true(all(field("method") == "tscores"))
  expect_true(all(vapply(field("version"), is.null, logical(1))))
  # ntrs sets its own defaults for these on load, so without overwrite = TRUE
  # set_std_defaults() would abort.
  expect_true(all(unlist(field("overwrite"))))
})

test_that(".set_defaults() can be called again, and ntrs then reports 'tscores'", {
  # ntrd calls .set_defaults() when the user clicks "Go", after .onAttach()
  # has already run it, so the repeat call must be harmless.
  expect_no_error(suppressMessages(.set_defaults()))

  for (nm in tscore_classes) {
    expect_identical(
      ntrs::get_std_defaults(new_scores(nm)),
      list(method = "tscores"),
      label = nm
    )
  }
})

test_that(".onAttach() sets the defaults", {
  called <- 0L
  local_mocked_bindings(.set_defaults = function() called <<- called + 1L)
  .onAttach()
  expect_identical(called, 1L)
})

test_that(".onLoad() registers the package's S7 methods", {
  called <- 0L
  local_mocked_bindings(
    methods_register = function(...) called <<- called + 1L,
    .package = "S7"
  )
  .onLoad()
  expect_identical(called, 1L)
})
