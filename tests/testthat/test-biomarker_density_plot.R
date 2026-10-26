# Covers R/biomarker_density_plot.R -> density_plot()
#
# STATUS: DRAFT — not yet executed. Expected values traced from source.
#
# NOTE (default args): the defaults for `dens` and `cuts` reference
# `all_densities` / `all_cuts`, which the body sets to NULL before they are
# forced, so calling density_plot() with its defaults errors. Tests always pass
# `dens` and `cuts` explicitly; the defaults are intentionally NOT tested.
#
# NOTE (doc drift): @returns says NULL is returned *invisibly* for obs = NA, but
# the body uses a bare `return()`, which is visible. Tests only assert NULL.
#
# NOTE (plotly internals): trace-level assertions read `p$x$attrs`, which holds
# one entry per plot_ly()/add_trace() call, in call order. For the 3-region
# fixture that is:
#   [[1]] plot_ly() base   [[2]] region 1 fill   [[3]] region 2 fill
#   [[4]] region 3 fill    [[5]] observed-value marker   [[6]] info-icon text
# If a plotly upgrade changes this layout, fix the indices, not the function.
#
# Fixtures (helper-fixtures.R): make_density_fixture() is a y = x "density" on
# x = 0, 0.1, ..., 1; make_density_cuts() has cuts at 0.45 and 0.75, which are
# deliberately OFF the grid so the cut-insertion branch runs.

attr_x <- function(p, i) p$x$attrs[[i]]

# ---------------------------------------------------------------------------
# Early return
# ---------------------------------------------------------------------------

test_that("density_plot() returns NULL when obs is NA", {
  expect_null(density_plot(
    obs = NA,
    dens = make_density_fixture(),
    cuts = make_density_cuts()
  ))
})

# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------

test_that("density_plot() returns a plotly htmlwidget", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
})

test_that("density_plot() adds one fill trace per region plus marker and info", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  # base + 3 region fills + marker + info text
  expect_length(p$x$attrs, 6L)
  for (i in 2:4) {
    expect_identical(attr_x(p, i)$fill, "tozeroy", label = paste("trace", i))
  }
  expect_identical(attr_x(p, 5)$mode, "markers")
  expect_identical(attr_x(p, 6)$mode, "text")
})

test_that("density_plot() renders to JSON without error", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  expect_no_error(plotly::plotly_json(p, jsonedit = FALSE))
})

# ---------------------------------------------------------------------------
# new_id
# ---------------------------------------------------------------------------

test_that("density_plot() uses new_id for the widget and trace ids", {
  p <- density_plot(
    0.5,
    make_density_fixture(),
    make_density_cuts(),
    new_id = "density-plotly-abc"
  )
  expect_identical(p$elementId, "density-plotly-abc")
  expect_identical(p$x$cur_data, "density-plotly-abc")
  expect_identical(names(p$x$visdat), "density-plotly-abc")
})

test_that("density_plot() leaves elementId unset without new_id", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  expect_null(p$elementId)
})

# ---------------------------------------------------------------------------
# Region colouring
# ---------------------------------------------------------------------------

test_that("the region containing obs is highlighted (alpha 0.9), others 0.4", {
  # obs = 0.5 falls in region 2: [0.45, 0.75).
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  expect_identical(attr_x(p, 2)$fillcolor, "rgba(0,128,0,0.4)")
  expect_identical(attr_x(p, 3)$fillcolor, "rgba(255,165,0,0.9)")
  expect_identical(attr_x(p, 4)$fillcolor, "rgba(255,0,0,0.4)")
  # Line colour follows fill colour.
  expect_identical(attr_x(p, 3)$line$color, "rgba(255,165,0,0.9)")
})

test_that("no region is highlighted when obs is beyond the last cut", {
  # findInterval() returns 4 (past the last cut) -> indexes a missing colour.
  p <- density_plot(1.5, make_density_fixture(), make_density_cuts())
  fills <- vapply(2:4, \(i) attr_x(p, i)$fillcolor, character(1))
  expect_false(any(grepl("0.9", fills, fixed = TRUE)))
  expect_true(all(grepl("0.4)", fills, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Cut insertion
# ---------------------------------------------------------------------------

test_that("off-grid cut values are inserted so regions meet exactly", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())

  # Region 1 runs 0 .. 0.45 and ends ON the inserted cut.
  x1 <- attr_x(p, 2)$x
  expect_equal(x1[1], 0)
  expect_equal(x1[length(x1)], 0.45)

  # Region 2 starts on 0.45 and ends on 0.75.
  x2 <- attr_x(p, 3)$x
  expect_equal(x2, c(0.45, 0.5, 0.6, 0.7, 0.75))

  # Region 3 starts on 0.75 and runs to the end of the grid.
  x3 <- attr_x(p, 4)$x
  expect_equal(x3[1], 0.75)
  expect_equal(x3[length(x3)], 1)
})

test_that("inserted cut points take the mean of the neighbouring y values", {
  # y = x on the grid, so the inserted y at 0.45 is mean(0.4, 0.5) = 0.45.
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  x2 <- attr_x(p, 3)$x
  y2 <- attr_x(p, 3)$y
  expect_equal(y2[x2 == 0.45], 0.45)
  expect_equal(y2[x2 == 0.75], 0.75)
})

# ---------------------------------------------------------------------------
# Hover text
# ---------------------------------------------------------------------------

test_that("the marker hover shows the observed value and its percentile", {
  # Left Riemann sum of y = x over 0..0.5 in steps of 0.1:
  #   0.1 * (0 + 0.1 + 0.2 + 0.3 + 0.4) = 0.1 -> "10%".
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  marker <- attr_x(p, 5)
  expect_equal(marker$x, 0.5)
  expect_true(grepl("Observed value: 0.5", marker$hovertemplate, fixed = TRUE))
  expect_true(grepl("Percentile: 10%", marker$hovertemplate, fixed = TRUE))
})

test_that("the info-icon hover reports sample size and bandwidth", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  info <- attr_x(p, 6)$hovertemplate
  expect_true(grepl("Sample size: 100", info, fixed = TRUE))
  expect_true(grepl("Bandwidth: 0.05", info, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Layout / config
# ---------------------------------------------------------------------------

test_that("density_plot() hides the mode bar and scales the x range to the data", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  expect_false(p$x$config$displayModeBar)
  lay <- p$x$layoutAttrs[[1]]
  expect_equal(lay$xaxis$range, c(-0.01, 1.01))
  expect_false(lay$showlegend)
})

test_that("density_plot() draws one dotted line per interior cut", {
  p <- density_plot(0.5, make_density_fixture(), make_density_cuts())
  shapes <- p$x$layoutAttrs[[1]]$shapes
  expect_length(shapes, 2L)
  expect_equal(vapply(shapes, `[[`, numeric(1), "x0"), c(0.45, 0.75))
  expect_equal(vapply(shapes, `[[`, numeric(1), "x1"), c(0.45, 0.75))
  expect_true(all(vapply(shapes, \(s) s$line$dash, character(1)) == "dot"))
})
