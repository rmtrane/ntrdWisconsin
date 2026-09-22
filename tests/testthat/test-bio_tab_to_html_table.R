# Covers R/bio_tab_to_html_table.R:
#   bio_tab_to_html_table()  [exported]  -> validation, both input paths,
#                                           headers, sections, cell rendering
#   create_td(), cell_content()          -> exercised through the full render
#
# STATUS: DRAFT — not yet executed. Traced from source.
#
# FIXTURES: the list-valued date cells are produced by bio_tab_for_gt()
# (R/query_panda.R), so fixtures are built with the real pipeline instead of by
# hand:  make_biomarker_*() -> clean_biomarker_data() -> bio_tab_for_gt().
# That reuses the item 10 fixtures, and the density/cut fixtures from item 11.
# A failure *inside* bio_tab_for_gt() therefore points at the fixture chain,
# not at this function.
#
# HTML MATCHING: the rendered tagList inlines biomarkerTable.css and
# biomarkerTable.js from inst/www, and those files mention the same class
# names (.error-message, .subtable-header, hoverBox_, ...). Assertions therefore
# match the attribute form (class="..." / id="...") to avoid false positives.
#
# INPUT PATHS:
#   - single data.table: what biomarkerModule.R passes today. `table` is set to
#     "table", `method` comes from `tab_header`.
#   - named list of data.tables: names become `table`; "csf"/"plasma"/
#     "visual_ratings" map to method "CSF"/"Plasma"/"Visual Ratings".

render <- function(...) as.character(bio_tab_to_html_table(...))

has <- function(html, pattern) grepl(pattern, html, fixed = TRUE)

count <- function(html, pattern) {
  m <- gregexpr(pattern, html, fixed = TRUE)[[1]]
  if (m[1] == -1) 0L else length(m)
}

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("bio_tab_to_html_table() aborts when input is neither list nor data.table", {
  local_reproducible_output()
  expect_error(
    bio_tab_to_html_table("x", densities = list(), cuts = make_bio_cuts()),
    regexp = "must be a list"
  )
})

test_that("bio_tab_to_html_table() aborts on list elements that aren't data.tables", {
  local_reproducible_output()
  expect_error(
    bio_tab_to_html_table(
      list(csf = make_bio_tab_csf(), plasma = "oops"),
      densities = make_bio_densities(),
      cuts = make_bio_cuts()
    ),
    regexp = "plasma"
  )
})

# ---------------------------------------------------------------------------
# Single data.table path (current caller: biomarkerModule.R)
# ---------------------------------------------------------------------------

test_that("bio_tab_to_html_table() returns a tagList with the table scaffold", {
  out <- bio_tab_to_html_table(
    make_bio_tab_csf(),
    densities = make_bio_densities(),
    cuts = make_bio_cuts(),
    tab_header = "",
    tab_id = "tbl"
  )
  expect_s3_class(out, "shiny.tag.list")

  html <- as.character(out)
  expect_true(has(html, 'class="table-container"'))
  expect_true(has(html, 'class="biomarkerTable"'))
  expect_true(has(html, "<style")) # CSS inlined from www_path
  expect_true(has(html, "<script")) # JS inlined from www_path
})

test_that("the visit date appears as a column header", {
  html <- render(
    make_bio_tab_csf(),
    densities = make_bio_densities(),
    cuts = make_bio_cuts(),
    tab_header = ""
  )
  expect_true(has(html, "<th>2021-05-01</th>"))
})

test_that("biomarker rows show the human-readable label and the age row shows age", {
  html <- render(
    make_bio_tab_csf(),
    densities = make_bio_densities(),
    cuts = make_bio_cuts(),
    tab_header = ""
  )
  expect_true(has(html, "Fujirebio Lumipulse A&beta;42/A&beta;40 (FDA)"))
  expect_true(has(html, "<td>70</td>"))
})

test_that("the age row is rendered above the biomarker rows", {
  # Verify on first run: row order comes from setorder(method, table, name_i).
  html <- render(
    make_bio_tab_csf(),
    densities = make_bio_densities(),
    cuts = make_bio_cuts(),
    tab_header = ""
  )
  age_pos <- regexpr("<td>70</td>", html, fixed = TRUE)
  bio_pos <- regexpr("Fujirebio Lumipulse", html, fixed = TRUE)
  expect_gt(age_pos, 0)
  expect_gt(bio_pos, 0)
  expect_lt(age_pos, bio_pos)
})

test_that("cells with a raw value get a density-plot hover box", {
  html <- render(
    make_bio_tab_csf(),
    densities = make_bio_densities(),
    cuts = make_bio_cuts(),
    tab_header = "",
    tab_id = "tbl"
  )
  # cell_id = paste(tab_id, row, column, sep = "_")
  expect_match(html, 'id="hoverBox_tbl_[0-9]+_2021-05-01"')
  # htmltools escapes ' in attribute values as &#39;; accept either form.
  expect_match(
    html,
    "toggleHoverBox\\(event, (&#39;|')tbl_[0-9]+_2021-05-01(&#39;|')\\)"
  )
  expect_true(has(html, "Raw value: 0.05 (Click icon for graph.)"))
  expect_true(has(html, 'class="flex-cell"'))
  # density_plot() gets new_id = "density-plotly-<cell_id>"
  expect_match(html, "density-plotly-tbl_[0-9]+_2021-05-01")
})

test_that("tab_header controls the subtable header in the single-table path", {
  no_header <- render(
    make_bio_tab_csf(),
    densities = make_bio_densities(),
    cuts = make_bio_cuts(),
    tab_header = ""
  )
  expect_false(has(no_header, 'class="subtable-header"'))

  with_header <- render(
    make_bio_tab_csf(),
    densities = make_bio_densities(),
    cuts = make_bio_cuts(),
    tab_header = "My results"
  )
  expect_equal(count(with_header, 'class="subtable-header"'), 1L)
  expect_true(has(with_header, "&nbsp;&nbsp; My results"))
})

test_that("print_x = TRUE prints each row index", {
  expect_output(
    bio_tab_to_html_table(
      make_bio_tab_csf(),
      densities = make_bio_densities(),
      cuts = make_bio_cuts(),
      print_x = TRUE
    ),
    "\\[1\\] 1"
  )
})

# ---------------------------------------------------------------------------
# Named-list path
# ---------------------------------------------------------------------------

test_that("list names map to method subtable headers", {
  html <- render(
    list(csf = make_bio_tab_csf()),
    densities = make_bio_densities(),
    cuts = make_bio_cuts()
  )
  expect_equal(count(html, 'class="subtable-header"'), 1L)
  expect_true(has(html, "&nbsp;&nbsp; CSF"))
})

test_that("NULL list entries are dropped", {
  html <- render(
    list(csf = make_bio_tab_csf(), plasma = NULL),
    densities = make_bio_densities(),
    cuts = make_bio_cuts()
  )
  expect_false(has(html, "&nbsp;&nbsp; Plasma"))
  expect_true(has(html, "&nbsp;&nbsp; CSF"))
})

test_that("a 'No values found' table renders as a no-values row", {
  html <- render(
    list(csf = make_bio_tab_csf(), plasma = bio_tab_for_gt(NULL)),
    densities = make_bio_densities(),
    cuts = make_bio_cuts()
  )
  expect_true(has(html, "&nbsp;&nbsp; Plasma"))
  expect_true(has(html, 'class="no-values"'))
  expect_true(has(html, "No values found"))
})

test_that("a try-error entry renders as an error-message row", {
  err <- structure("Error in query : timed out\n", class = "try-error")
  html <- render(
    list(csf = make_bio_tab_csf(), plasma = err),
    densities = make_bio_densities(),
    cuts = make_bio_cuts()
  )
  expect_true(has(html, "&nbsp;&nbsp; Plasma"))
  expect_true(has(html, 'class="error-message"'))
})

# ---------------------------------------------------------------------------
# Sections (Braak rows)
# ---------------------------------------------------------------------------

test_that("consecutive Braak rows get one section header and indented names", {
  html <- render(
    list(visual_ratings = make_bio_tab_visual()),
    densities = list(),
    cuts = make_bio_cuts()
  )
  expect_equal(count(html, 'class="section-header"'), 1L)
  expect_true(has(html, "Tau PET NFT Rating"))
  expect_true(has(html, 'class="indented-cell"'))
  expect_true(has(html, "&nbsp;&nbsp; Visual Ratings"))
})

test_that("section_label and section_pattern are configurable", {
  html <- render(
    list(visual_ratings = make_bio_tab_visual()),
    densities = list(),
    cuts = make_bio_cuts(),
    section_label = "Braak stages"
  )
  expect_true(has(html, "Braak stages"))
  expect_false(has(html, "Tau PET NFT Rating"))

  no_match <- render(
    list(visual_ratings = make_bio_tab_visual()),
    densities = list(),
    cuts = make_bio_cuts(),
    section_pattern = "^no_such_row"
  )
  expect_false(has(no_match, 'class="section-header"'))
  expect_false(has(no_match, 'class="indented-cell"'))
})

test_that("visual-ratings cells without raw values get no density plot", {
  html <- render(
    list(visual_ratings = make_bio_tab_visual()),
    densities = list(),
    cuts = make_bio_cuts()
  )
  expect_false(has(html, 'id="hoverBox_'))
  # One header column per visit date.
  for (d in c("2021-05-01", "2022-06-15", "2023-07-20")) {
    expect_true(has(html, paste0("<th>", d, "</th>")), label = d)
  }
})
