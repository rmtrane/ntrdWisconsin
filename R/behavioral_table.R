#' Build a behavioral tracking gt table
#'
#' Renders a scrollable, sticky-column \pkg{gt} table with formatted date
#' columns and sublabel columns, row-group labels that stick to the left edge,
#' and Bootstrap tooltips that activate on overflow or when extra info is
#' available.
#'
#' @param for_tab A data frame to pass to [gt::gt()]. Must contain a `label`
#'   column (used as the row-group column), a `sublabel` column, and one or
#'   more date columns whose names match `"20[0-9]{2}-[0-9]{2}-[0-9]{2}"`.
#'   Date-column cells should be lists with `$value` and `$extra_info`
#'   elements; sublabel cells should be lists with `$value` and `$extra_info`
#'   elements.
#' @param table_id A single string used as the HTML `id` attribute of the
#'   rendered table and as a CSS scope prefix. Defaults to
#'   `"status-tracker"`.
#'
#' @return A [shiny::tagList()] containing the rendered gt table wrapped in a
#'   container `<div>` and an inline `<script>` that initialises Bootstrap
#'   tooltips.
#'
#' @seealso [format_date_cell()], [format_sublabel_cell()],
#'   [wrap_sticky_rowgroup()], [table_css()], [tooltip_script()]
behavioral_table <- function(
  for_tab,
  table_id = "status-tracker"
) {
  label <- sublabel <- variable <- NULL

  no_val_dt <- data.table::data.table(
    label = "No values found"
  ) |>
    gt::gt() |>
    gt::cols_label(label ~ "") |>
    gt::tab_style(
      style = gt::cell_borders(sides = "all", style = "hidden"),
      locations = list(
        gt::cells_column_labels(columns = gt::everything()),
        gt::cells_body(columns = gt::everything())
      )
    )

  if (nrow(for_tab) == 0 | ncol(for_tab) == 0) {
    return(no_val_dt)
  }

  date_cols_match <- "20[0-9]{2}-[0-9]{2}-[0-9]{2}"

  with_valid_responses <- for_tab[
    !label %in%
      c(
        "Response Validity",
        "What makes this participant's responses less valid?"
      ),
    lapply(
      .SD,
      \(x) {
        # if (is.list(x)) {
        sum(!is.na(x))
        # }
      }
    ),
    .SDcols = data.table::patterns(date_cols_match)
  ]

  if (nrow(with_valid_responses) == 0) {
    return(no_val_dt)
  }

  with_valid_responses <- with_valid_responses[,
    lapply(.SD, \(x) if (x > 0) x)
  ]

  if (ncol(with_valid_responses) == 0) {
    return(no_val_dt)
  }

  for_tab <- for_tab[,
    .SD,
    .SDcols = c("label", "variable", names(with_valid_responses))
  ]

  complete_tab <- data.table::data.table(
    label = factor(c(
      "Test Battery Completed",
      "Additional Notes",
      "Response Validity",
      "Mood",
      "Affect",
      "Attitude Toward Testing",
      "Language",
      "Sensory Function",
      "Comprehension"
    )),
    variable = c(
      "battery",
      "notes",
      "respval",
      "mood",
      "affect",
      "attitude",
      "language",
      "snsry_fncn",
      "comprhnsn"
    )
  )

  tbl <- gt::gt(
    complete_tab[for_tab, on = c("label", "variable")][,
      variable := NULL
    ][order(label)],
    # groupname_col = "label",
    id = table_id
  ) |>
    # Structure
    gt::cols_label(label ~ "") |>
    # Content transforms
    gt::fmt(
      columns = gt::matches(date_cols_match),
      fn = \(x) purrr::map(x, gt::html)
    ) |>
    # gt::fmt(
    #   columns = sublabel,
    #   fn = \(x) purrr::map(x, format_sublabel_cell)
    # ) |>

    # Styling
    gt::tab_style(
      style = gt::cell_text(whitespace = "nowrap", weight = "bold"),
      locations = gt::cells_column_labels(
        columns = gt::matches(date_cols_match)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "top", style = "double"),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = label == "Mood"
      )
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(
          align = "left" #,
          # indent = gt::px(12)
        ),
        gt::css(
          "max-width" = "clamp(100px, 30cqw, 500px)",
          "overflow" = "hidden",
          "white-space" = "nowrap",
          "text-overflow" = "ellipsis",
          "vertical-align" = "top"
        )
      ),
      locations = gt::cells_body(columns = label)
    ) |>
    # gt::tab_style(
    #   style = gt::cell_borders(sides = "left", style = "hidden"),
    #   locations = gt::cells_body(columns = gt::everything())
    # ) |>
    gt::opt_css(css = table_css(table_id))

  # ---- Assemble page -----------------------------------------------------------

  shiny::tagList(
    shiny::tags$div(class = paste0(table_id, '-container'), tbl),
    shiny::tags$script(shiny::HTML(tooltip_script(table_id)))
  ) |>
    bslib::page()
}

# ---- Helpers -----------------------------------------------------------------

#' Wrap content in a Bootstrap tooltip span
#'
#' Returns an HTML string that wraps `content` in a `<span>` with
#' `data-bs-toggle="tooltip"`. When `title` is not `NA`, the span also
#' carries `data-bs-placement` and `data-bs-title` attributes so that
#' Bootstrap will render a tooltip on hover.
#'
#' @param content A character string of HTML content to wrap.
#' @param title A scalar string used as the tooltip text, or `NA` (the
#'   default) to omit the tooltip title attributes.
#'
#' @return A character string containing the HTML `<span>` element.
with_tooltip <- function(content, title = NULL) {
  if (is.na(title)) {
    return(paste0(
      '<span data-bs-toggle="tooltip">',
      content,
      "</span>"
    ))
  }

  paste0(
    '<span data-bs-toggle="tooltip" data-bs-placement="top" ',
    'data-bs-title="',
    title,
    '">',
    content,
    '</span>'
  )
}

#' Format a single date-column cell
#'
#' Converts a list with `$value` and `$extra_info` elements into an HTML
#' fragment suitable for use inside a \pkg{gt} table cell. Special values
#' `"checked"` and `"unchecked"` are replaced with \pkg{bsicons} SVG icons;
#' `NA` values render as a styled em-dash; all other values are rendered as-is.
#' The content is wrapped in a Bootstrap tooltip when `$extra_info` is present.
#'
#' @param y A named list with elements:
#'   \describe{
#'     \item{`value`}{A scalar string (or `NA`) representing the cell value.}
#'     \item{`extra_info`}{A scalar string (or `NA`) used as tooltip text.}
#'   }
#'
#' @return A [gt::html()] object.
#'
#' @seealso [with_tooltip()]
format_date_cell <- function(y) {
  val <- y[["value"]]
  ei <- y[["extra_info"]]

  if (is.na(val)) {
    return(gt::html('<span class="cell-missing">&mdash;</span>'))
  }

  rendered <- switch(
    val,
    "checked" = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\" class=\"bi bi-check-lg \" style=\"height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;\" aria-hidden=\"true\" role=\"img\" ><path d=\"M12.736 3.97a.733.733 0 0 1 1.047 0c.286.289.29.756.01 1.05L7.88 12.01a.733.733 0 0 1-1.065.02L3.217 8.384a.757.757 0 0 1 0-1.06.733.733 0 0 1 1.047 0l3.052 3.093 5.4-6.425a.247.247 0 0 1 .02-.022Z\"></path></svg>", #as.character(bsicons::bs_icon("check-lg")),
    "unchecked" = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\" class=\"bi bi-x-circle \" style=\"height:1em;width:1em;fill:currentColor;vertical-align:-0.125em;color:#e0e0e0;\" aria-hidden=\"true\" role=\"img\" ><path d=\"M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z\"></path>\n<path d=\"M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z\"></path></svg>", #as.character(bsicons::bs_icon("x-circle", color = "#e0e0e0")),
    val
  )

  gt::html(with_tooltip(rendered, ei %||% NA))
}

#' Format a single sublabel cell
#'
#' Converts a list with `$value` and `$extra_info` elements into an HTML
#' fragment for a sublabel column cell. The cell text is optionally wrapped in
#' a Bootstrap tooltip when `$extra_info` is present.
#'
#' @param x A named list with elements:
#'   \describe{
#'     \item{`value`}{A scalar string containing the display text.}
#'     \item{`extra_info`}{A scalar string (or `NA`) used as tooltip text.}
#'   }
#'
#' @return A [gt::html()] object.
#'
#' @seealso [with_tooltip()]
format_sublabel_cell <- function(x) {
  val <- x[["value"]]
  ei <- x[["extra_info"]]

  gt::html(with_tooltip(val, ei %||% NA))
}

#' Wrap row-group labels for sticky left-edge positioning
#'
#' Transforms a character vector of row-group label strings by wrapping
#' non-empty values in a `<span class="sticky-rowgroup">` element. This allows
#' CSS `position: sticky` to keep the label visible while the table scrolls
#' horizontally.
#'
#' @param x A character vector of row-group label strings, as supplied by
#'   [gt::text_transform()].
#'
#' @return A list of [gt::html()] objects the same length as `x`.
wrap_sticky_rowgroup <- function(x) {
  out <- ifelse(
    is.na(x) | trimws(x) == "",
    x,
    paste0('<span class="sticky-rowgroup">', x, '</span>')
  )
  lapply(out, gt::html)
}


# ---- Styles & scripts (scoped to #table_id) ----------------------------------

#' Generate scoped CSS for a behavioral table
#'
#' Produces a CSS string with all rules scoped to the given `table_id`. The
#' rules implement a sticky first column, sticky row-group label spans,
#' tooltip cursor styling, and a muted colour for missing-value cells.
#'
#' @param table_id A single string matching the `id` attribute of the target
#'   table (see [behavioral_table()]).
#'
#' @return A character string of CSS.
#'
#' @seealso [behavioral_table()], [gt::opt_css()]
table_css <- function(table_id) {
  gsub(
    "__ID__",
    table_id,
    "
    .__ID__-container { container-type: inline-size; }

    #__ID__ tbody td:first-child,
    #__ID__ thead th:first-child {
      position: sticky;
      left: 0;
      background: white;
      z-index: 1;
      box-shadow: inset -2px 0 0 #e0e0e0;
    }
    #__ID__ thead th:first-child { z-index: 2; }

    #__ID__ .sticky-rowgroup {
      position: sticky;
      left: 12px;
      display: inline-block;
    }

    #__ID__ td span[data-bs-title] {
      cursor: help;
      border-bottom: 1px dotted #999;
    }

    #__ID__ .cell-missing { color: #e0e0e0; }
  ",
    fixed = TRUE
  )
}

#' Generate the Bootstrap tooltip initialisation script
#'
#' Returns a self-executing JavaScript snippet scoped to a single table. The
#' script attaches Bootstrap tooltips to cells whose text overflows or that
#' carry an `extra_info` value, rebuilds them on resize, and uses a
#' `MutationObserver` to handle Shiny re-renders that replace the table node.
#'
#' @param table_id A single string matching the `id` attribute of the target
#'   table (see [behavioral_table()]).
#'
#' @return A character string of JavaScript.
#'
#' @seealso [behavioral_table()], [shiny::HTML()]
tooltip_script <- function(table_id) {
  gsub(
    "__ID__",
    table_id,
    "
    (function() {
    const TABLE_ID = '__ID__';
    let currentTable = null;
    let resizeObserver = null;
    let resizeTimer = null;

    function rebuildTooltips() {
      if (!currentTable || !window.bootstrap) return;

      currentTable.querySelectorAll('td span[data-bs-toggle=\"tooltip\"]').forEach(el => {
        const td = el.closest('td');
        if (!td) return;

        // Tear down any prior tooltip on this cell before recreating.
        const existing = bootstrap.Tooltip.getInstance(td);
        if (existing) existing.dispose();

        const isOverflowing = td.scrollWidth > td.clientWidth;
        const extraInfo = el.getAttribute('data-bs-title');
        if (!isOverflowing && !extraInfo) return;

        const title = isOverflowing && extraInfo
          ? `${el.textContent}\n${extraInfo}`
          : isOverflowing
          ? el.textContent
          : extraInfo;

        new bootstrap.Tooltip(td, { title: title, placement: 'top' });
      });
    }

    function scheduleRebuild() {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(rebuildTooltips, 150);
    }

    function bindTable(table) {
      if (table === currentTable) return;
      currentTable = table;

      rebuildTooltips();

      if (resizeObserver) resizeObserver.disconnect();
      if ('ResizeObserver' in window) {
        resizeObserver = new ResizeObserver(scheduleRebuild);
        resizeObserver.observe(table);
      }
    }

    function init() {
      const table = document.getElementById(TABLE_ID);
      if (table) bindTable(table);

      // Window resize (sidebar, devtools, etc.)
      window.addEventListener('resize', scheduleRebuild);

      // Watch for the table being inserted or replaced. Covers:
      //   * Shiny initial render (table arrives after DOMContentLoaded)
      //   * Shiny re-render (gt_output swaps the node on reactive updates)
      //   * Static HTML (no-op, but harmless)
      new MutationObserver(() => {
        const table = document.getElementById(TABLE_ID);
        if (table && table !== currentTable) bindTable(table);
      }).observe(document.body, { childList: true, subtree: true });
    }

    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', init);
    } else {
      // Script ran after DOM was already parsed - start immediately.
      init();
    }
  })();
  ",
    fixed = TRUE
  )
}
