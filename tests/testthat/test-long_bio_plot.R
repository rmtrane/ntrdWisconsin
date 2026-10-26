# test-long_bio_plot.R  (plan item 16)
#
# Covers R/long_bio_plot.R:
#   long_bio_plot()     -> scatter panel (bands, obs_outer, obs) + density panel
#   add_row_titles()    -> one title annotation per stacked row
#   long_bio_plot_js    -> contract: the trace names/fills the hover JS relies on
#   on_render()         -> appends render hooks to a widget
#   `%||%`              -> NULL default
#
# STATUS: DRAFT — not yet executed. Expected values traced from source.
#
# FIXTURES: reuses make_density_fixture() (y = x on x = 0, 0.1, ..., 1;
# n = 100, bw = 0.05) and make_density_cuts() (cuts at 0.45 / 0.75, off the
# grid) from helper-fixtures.R (item 11). Local helpers below add the dat
# table and variants.
#
# WHERE VALUES ARE READ FROM: band and density traces hold plain vectors, so
# they're read from p$x$attrs. The obs / obs_outer traces use formulas
# (~date, ~obs), so they're read from plotly_build(p)$x$data.
#
# LIKELY ISSUES (see notes on the individual tests):
#   - add_row_titles(): the title markup appears to contain a zero-width space
#     (U+200B) between "<" and "span", probably copied from the roxygen, where
#     it stops the tag being parsed. In the code it stops plotly parsing it too.
#   - long_bio_plot(): the x axis is given `label = ""`, which isn't a plotly
#     axis attribute (`title` is). If "scatter panel builds without warnings"
#     fails on that, it's this.

# ---------------------------------------------------------------------------
# Local helpers
# ---------------------------------------------------------------------------

# Three visits; the middle one has no value (dropped from the traces, kept on
# the x axis).
make_long_dat <- function(vals = c(0.2, NA, 0.6)) {
  data.table::data.table(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    age = c(70, 71, 72),
    val_raw = vals
  )
}

lbp <- function(
  dat = make_long_dat(),
  dens = make_density_fixture(),
  cuts = make_density_cuts()
) {
  long_bio_plot(dat, y_val = "val_raw", dens = dens, cuts = cuts)
}

attr_by_name <- function(p, nm) {
  hits <- Filter(\(a) identical(a$name, nm), p$x$attrs)
  if (length(hits) == 0) NULL else hits[[1]]
}

built_by_name <- function(p, nm) {
  b <- suppressWarnings(plotly::plotly_build(p))
  hits <- Filter(\(t) identical(t$name, nm), b$x$data)
  if (length(hits) == 0) NULL else hits[[1]]
}

# ---------------------------------------------------------------------------
# long_bio_plot() -- basics
# ---------------------------------------------------------------------------

test_that("long_bio_plot() returns NULL for NULL or empty data", {
  expect_null(long_bio_plot(
    NULL,
    dens = make_density_fixture(),
    cuts = make_density_cuts()
  ))
  expect_null(lbp(dat = make_long_dat()[0]))
})

test_that("long_bio_plot() returns scatter and density plotly panels", {
  out <- lbp()
  expect_named(out, c("scatter", "density"))
  expect_s3_class(out$scatter, "plotly")
  expect_s3_class(out$density, "plotly")
})

test_that("the scatter panel builds without warnings", {
  # If this warns about `label`, see the header note.
  expect_no_warning(plotly::plotly_build(lbp()$scatter))
})

test_that("the density panel builds without warnings", {
  expect_no_warning(plotly::plotly_build(lbp()$density))
})

# ---------------------------------------------------------------------------
# long_bio_plot() -- scatter panel: bands
# ---------------------------------------------------------------------------

test_that("one shaded band per cut region, named band1..bandN", {
  p <- lbp()$scatter
  for (i in 1:3) {
    expect_false(is.null(attr_by_name(p, paste0("band", i))), label = i)
  }
  expect_null(attr_by_name(p, "band4"))
})

test_that("bands span the cut values and the padded date range", {
  p <- lbp()$scatter
  x_rng <- as.Date(c("2019-12-25", "2019-12-25", "2022-01-08", "2022-01-08"))

  b1 <- attr_by_name(p, "band1")
  expect_equal(b1$y, c(0, 0.45, 0.45, 0))
  expect_equal(b1$x, x_rng)

  expect_equal(attr_by_name(p, "band2")$y, c(0.45, 0.75, 0.75, 0.45))
  expect_equal(attr_by_name(p, "band3")$y, c(0.75, 1, 1, 0.75))
})

test_that("bands are filled shapes with 0.2 opacity and no hover", {
  p <- lbp()$scatter
  b1 <- attr_by_name(p, "band1")
  expect_identical(b1$fill, "toself")
  expect_identical(b1$mode, "none")
  expect_identical(b1$hoverinfo, "skip")
  expect_identical(b1$fillcolor, "rgba(0,128,0,0.2)")
})

test_that("an infinite upper cut is clamped to the plot's upper limit", {
  cuts <- make_density_cuts()[3, max_obs := Inf]
  p <- lbp(cuts = cuts)$scatter
  # y_max here is max(grid x = 1, finite cuts, 1.12 * max obs = 0.672) = 1.
  expect_equal(attr_by_name(p, "band3")$y, c(0.75, 1, 1, 0.75))
})

test_that("the upper limit stops where the density reaches ~95%", {
  # Flat density (y = 1 on 0..1): cumulative area passes 0.95 after x = 0.9,
  # so the plot stops at 0.9 and the Inf band is clamped there.
  dens <- list(x = seq(0, 1, by = 0.1), y = rep(1, 11), n = 100, bw = 0.05)
  cuts <- data.table::data.table(
    color = c("rgba(0,128,0,alpha)", "rgba(255,0,0,alpha)"),
    min_obs = c(0, 0.5),
    max_obs = c(0.5, Inf)
  )
  p <- lbp(dens = dens, cuts = cuts)$scatter
  expect_equal(max(attr_by_name(p, "band2")$y), 0.9)
})

test_that("the upper limit extends to fit the largest observation", {
  # Same flat density, but an observation at 0.95 -> limit 0.95 * 1.12.
  dens <- list(x = seq(0, 1, by = 0.1), y = rep(1, 11), n = 100, bw = 0.05)
  cuts <- data.table::data.table(
    color = c("rgba(0,128,0,alpha)", "rgba(255,0,0,alpha)"),
    min_obs = c(0, 0.5),
    max_obs = c(0.5, Inf)
  )
  p <- lbp(
    dat = make_long_dat(c(0.2, NA, 0.95)),
    dens = dens,
    cuts = cuts
  )$scatter
  expect_equal(max(attr_by_name(p, "band2")$y), 0.95 * 1.12)
})

# ---------------------------------------------------------------------------
# long_bio_plot() -- scatter panel: observations and axis
# ---------------------------------------------------------------------------

test_that("the obs trace plots non-missing values with hover text", {
  obs <- built_by_name(lbp()$scatter, "obs")

  y_vals <- as.numeric(obs$y)

  expect_equal(y_vals, c(0.2, 0.6))

  x_vals <- as.Date(obs$x)
  attr(x_vals, "apiSrc") <- NULL
  expect_equal(x_vals, as.Date(c("2020-01-01", "2022-01-01")))

  expect_identical(obs$mode, "markers+lines")

  hover_template <- obs$hovertemplate
  attr(hover_template, "apiSrc") <- NULL

  expect_identical(
    hover_template,
    c(
      "2020-01-01<br>Age: 70<br>Value: 0.2<extra></extra>",
      "2022-01-01<br>Age: 72<br>Value: 0.6<extra></extra>"
    )
  )
})

test_that("obs carries 5% error bars; obs_outer carries invisible 10% bars", {
  s <- lbp()$scatter

  obs <- built_by_name(s, "obs")
  expect_equal(obs$error_y$array, c(0.01, 0.03), ignore_attr = T)
  expect_equal(obs$error_y$arrayminus, c(0.01, 0.03), ignore_attr = T)

  outer <- built_by_name(s, "obs_outer")
  expect_equal(outer$y, c(0.2, 0.6), ignore_attr = T)
  expect_equal(outer$error_y$array, c(0.02, 0.06), ignore_attr = T)
  expect_equal(outer$marker$opacity, 0, ignore_attr = T)
  expect_identical(outer$hoverinfo, c("skip", "skip"), ignore_attr = T)
})

test_that("the x axis keeps every visit date, including ones with no value", {
  lay <- lbp()$scatter$x$layoutAttrs[[1]]
  dates <- as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))
  expect_equal(lay$xaxis$tickvals, dates)
  expect_identical(lay$xaxis$ticktext, as.character(dates))
  expect_equal(lay$xaxis$range, as.Date(c("2019-12-25", "2022-01-08")))
})

# ---------------------------------------------------------------------------
# long_bio_plot() -- density panel
# ---------------------------------------------------------------------------

test_that("the density panel draws one sideways band per region", {
  d <- lbp()$density
  # attrs: [[1]] base, [[2]]-[[4]] bands, [[5]] info icon.
  expect_length(d$x$attrs, 5L)
  for (i in 2:4) {
    expect_identical(d$x$attrs[[i]]$fill, "tozerox", label = i)
  }
  # Band 1 covers biomarker values 0 .. 0.45 (0.45 inserted off-grid).
  b1 <- d$x$attrs[[2]]$y
  expect_equal(b1[1], 0)
  expect_equal(b1[length(b1)], 0.45)
  # Full opacity in the density panel.
  expect_identical(d$x$attrs[[2]]$fillcolor, "rgba(0,128,0,1)")
})

test_that("the density curve is scaled so its peak is 1", {
  d <- lbp()$density
  peaks <- vapply(2:4, \(i) max(d$x$attrs[[i]]$x), numeric(1))
  expect_equal(max(peaks), 1)
})

test_that("the density panel marks interior cuts with lines and labels", {
  lay <- lbp()$density$x$layoutAttrs[[1]]

  expect_length(lay$shapes, 2L)
  expect_equal(vapply(lay$shapes, `[[`, numeric(1), "y0"), c(0.45, 0.75))
  expect_true(all(vapply(lay$shapes, \(s) s$line$dash, character(1)) == "dot"))

  expect_identical(
    vapply(lay$annotations, `[[`, character(1), "text"),
    c("0.45", "0.75")
  )
})

test_that("the info icon reports sample size and bandwidth", {
  info <- lbp()$density$x$attrs[[5]]
  expect_identical(info$mode, "text")
  expect_true(grepl("Sample size: 100", info$hovertemplate, fixed = TRUE))
  expect_true(grepl("Bandwidth: 0.05", info$hovertemplate, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Contract with long_bio_plot_js
# ---------------------------------------------------------------------------

test_that("the hover JS looks for the trace names and fills the R side makes", {
  # JS: bands are traces whose name contains 'band' and fill === 'toself';
  # observation traces are those whose mode contains 'markers'.
  expect_type(long_bio_plot_js, "character")
  expect_length(long_bio_plot_js, 1L)
  expect_true(grepl("indexOf('band')", long_bio_plot_js, fixed = TRUE))
  expect_true(grepl("tr.fill === 'toself'", long_bio_plot_js, fixed = TRUE))
  expect_true(grepl("indexOf('markers')", long_bio_plot_js, fixed = TRUE))

  s <- lbp()$scatter
  b1 <- attr_by_name(s, "band1")
  expect_true(grepl("band", b1$name, fixed = TRUE))
  expect_identical(b1$fill, "toself")
  expect_true(grepl("markers", built_by_name(s, "obs")$mode, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# add_row_titles()
# ---------------------------------------------------------------------------

two_row_fig <- function() {
  a <- lbp()
  b <- lbp(dat = make_long_dat(c(0.3, 0.4, NA)))
  plotly::subplot(a$scatter, a$density, b$scatter, b$density, nrows = 2)
}

anns_of <- function(fig) {
  suppressWarnings(plotly::plotly_build(fig))$x$layout$annotations
}

test_that("add_row_titles() appends one annotation per row, keeping existing ones", {
  fig <- two_row_fig()
  before <- length(anns_of(fig))

  after <- anns_of(add_row_titles(fig, c("Row A", "Row B")))
  expect_length(after, before + 2L)
})

test_that("add_row_titles() places titles at the top of each row", {
  anns <- anns_of(add_row_titles(two_row_fig(), c("Row A", "Row B")))
  n <- length(anns)
  a <- anns[[n - 1]]
  b <- anns[[n]]

  expect_identical(a$yref, "paper")
  expect_identical(a$yanchor, "bottom")
  expect_equal(a$y, c("y" = 1)) # top row reaches the top of the figure
  expect_lt(b$y, a$y) # second row sits lower
  expect_true(grepl("Row A", a$text, fixed = TRUE))
  expect_true(grepl("Row B", b$text, fixed = TRUE))
})

test_that("add_row_titles() passes the font size through", {
  anns <- anns_of(add_row_titles(two_row_fig(), c("A", "B"), font_size = 20))
  expect_equal(anns[[length(anns)]]$font$size, 20)

  anns <- anns_of(add_row_titles(two_row_fig(), c("A", "B")))
  expect_equal(anns[[length(anns)]]$font$size, 13)
})

test_that("add_row_titles() wraps titles in a real <span> tag", {
  # Fails if the source string has a zero-width space (U+200B) after "<",
  # which stops plotly treating it as a tag (header note).
  anns <- anns_of(add_row_titles(two_row_fig(), c("A", "B")))
  txt <- anns[[length(anns)]]$text
  expect_false(grepl("\u200b", txt, fixed = TRUE))
  expect_true(startsWith(txt, "<span style="))
  expect_true(endsWith(txt, "</span>"))
})

# ---------------------------------------------------------------------------
# on_render() and %||%
# ---------------------------------------------------------------------------

test_that("on_render() appends a render hook, collapsing multi-line code", {
  p <- plotly::plot_ly()

  p1 <- on_render(p, c("function(el) {", "}"), data = list(a = 1))
  hooks <- p1$jsHooks$render
  expect_length(hooks, 1L)
  expect_identical(hooks[[1]]$code, "function(el) {\n}")
  expect_identical(hooks[[1]]$data, list(a = 1))

  p2 <- on_render(p1, "function(el) {}")
  expect_length(p2$jsHooks$render, 2L)
  expect_identical(p2$jsHooks$render[[1]], hooks[[1]])
  expect_null(p2$jsHooks$render[[2]]$data)
})

test_that("%||% returns the fallback only for NULL", {
  expect_identical(NULL %||% "y", "y")
  expect_identical("x" %||% "y", "x")
  expect_identical(NA %||% "y", NA)
})
