# test-behavioral_table.R  (plan item 12)
#
# Covers R/behavioral_table.R:
#   with_tooltip(), format_date_cell(), format_sublabel_cell(),
#   wrap_sticky_rowgroup(), table_css(), tooltip_script(), behavioral_table()
#
# STATUS: DRAFT — not yet executed. Pure string/HTML builders traced from
# source; the full behavioral_table() render is structural + flagged.
#
# NOTE (with_tooltip default): the signature is `title = NULL`, but the body
# does `if (is.na(title))`, and is.na(NULL) is logical(0) -> calling
# with_tooltip("x") with the default errors. Real callers always pass NA or a
# string (`ei %||% NA`), so the default is intentionally NOT tested.
#
# NOTE (behavioral_table contract): the roxygen mentions `sublabel` + list cells,
# but the code joins on `variable` and the real caller passes HTML strings in the
# date columns. Fixtures/tests follow the real caller (see helper-fixtures.R).

html_str <- function(h) as.character(h)

# ---------------------------------------------------------------------------
# table_css() — the item's named focus
# ---------------------------------------------------------------------------

test_that("table_css() scopes all rules to the given id and leaves no placeholder", {
  css <- table_css("myTbl")
  expect_length(css, 1L)
  expect_type(css, "character")
  expect_false(grepl("__ID__", css, fixed = TRUE))
  # Scoped selectors present.
  expect_true(grepl(".myTbl-container", css, fixed = TRUE))
  expect_true(grepl("#myTbl tbody td:first-child", css, fixed = TRUE))
  expect_true(grepl("#myTbl thead th:first-child", css, fixed = TRUE))
  expect_true(grepl("#myTbl .sticky-rowgroup", css, fixed = TRUE))
  expect_true(grepl("#myTbl td span[data-bs-title]", css, fixed = TRUE))
  expect_true(grepl("#myTbl .cell-missing", css, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# tooltip_script()
# ---------------------------------------------------------------------------

test_that("tooltip_script() injects the id and leaves no placeholder", {
  js <- tooltip_script("myTbl")
  expect_length(js, 1L)
  expect_type(js, "character")
  expect_false(grepl("__ID__", js, fixed = TRUE))
  expect_true(grepl("const TABLE_ID = 'myTbl';", js, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# with_tooltip()
# ---------------------------------------------------------------------------

test_that("with_tooltip() emits a toggle-only span when title is NA", {
  out <- with_tooltip("hello", NA)
  expect_true(grepl('data-bs-toggle="tooltip"', out, fixed = TRUE))
  expect_false(grepl("data-bs-title", out, fixed = TRUE))
  expect_true(grepl("hello", out, fixed = TRUE))
})

test_that("with_tooltip() adds title attributes when a title is supplied", {
  out <- with_tooltip("hello", "more info")
  expect_true(grepl('data-bs-title="more info"', out, fixed = TRUE))
  expect_true(grepl('data-bs-placement="top"', out, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# format_date_cell()
# ---------------------------------------------------------------------------

test_that("format_date_cell() renders NA as a muted em-dash", {
  out <- html_str(format_date_cell(list(value = NA, extra_info = NA)))
  expect_true(grepl("cell-missing", out, fixed = TRUE))
  expect_true(grepl("mdash", out, fixed = TRUE))
})

test_that("format_date_cell() maps checked/unchecked to icons", {
  chk <- html_str(format_date_cell(list(value = "checked", extra_info = NA)))
  unchk <- html_str(format_date_cell(list(
    value = "unchecked",
    extra_info = NA
  )))
  expect_true(grepl("bi-check-lg", chk, fixed = TRUE))
  expect_true(grepl("bi-x-circle", unchk, fixed = TRUE))
})

test_that("format_date_cell() renders plain values with tooltip when extra_info present", {
  out <- html_str(format_date_cell(list(
    value = "Very valid",
    extra_info = "tip"
  )))
  expect_true(grepl("Very valid", out, fixed = TRUE))
  expect_true(grepl('data-bs-title="tip"', out, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# format_sublabel_cell()
# ---------------------------------------------------------------------------

test_that("format_sublabel_cell() wraps value and adds tooltip when present", {
  plain <- html_str(format_sublabel_cell(list(value = "Mood", extra_info = NA)))
  tip <- html_str(format_sublabel_cell(list(
    value = "Mood",
    extra_info = "detail"
  )))
  expect_true(grepl("Mood", plain, fixed = TRUE))
  expect_false(grepl("data-bs-title", plain, fixed = TRUE))
  expect_true(grepl('data-bs-title="detail"', tip, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# wrap_sticky_rowgroup()
# ---------------------------------------------------------------------------

test_that("wrap_sticky_rowgroup() wraps non-empty labels and preserves length", {
  out <- wrap_sticky_rowgroup(c("Mood", NA, ""))
  expect_length(out, 3L)
  expect_true(grepl("sticky-rowgroup", html_str(out[[1]]), fixed = TRUE))
  # NA / empty entries are passed through without the wrapper span.
  expect_false(grepl("sticky-rowgroup", html_str(out[[3]]), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# behavioral_table() — early-return branches (solid)
# ---------------------------------------------------------------------------

test_that("behavioral_table() returns a placeholder gt table for empty input", {
  out <- behavioral_table(data.table::data.table())
  expect_s3_class(out, "gt_tbl")
})

test_that("behavioral_table() returns the placeholder when no valid responses", {
  out <- behavioral_table(make_behavioral_table_invalid_only())
  expect_s3_class(out, "gt_tbl")
})

# ---------------------------------------------------------------------------
# behavioral_table() — full render (structural; verify on first run)
# ---------------------------------------------------------------------------

test_that("behavioral_table() renders a scoped table + tooltip script", {
  out <- behavioral_table(make_behavioral_table_fixture(), table_id = "tbl123")
  # Real render returns a bslib page / shiny tag; assert it builds and carries
  # the scoped id through to the container and the init script.
  rendered <- as.character(out)
  expect_true(grepl("tbl123-container", rendered, fixed = TRUE))
  expect_true(grepl("const TABLE_ID = 'tbl123';", rendered, fixed = TRUE))
})
