long_bio_plot <- function(
  dat = tmp,
  y_val = "csf_ratio_roche_ptau181_ab42_local_raw",
  dens = all_densities$csf_ratio_roche_ptau181_ab42_local_raw,
  cuts = all_cuts[[1]][name == "csf_ratio_roche_ptau181_ab42_local"],
  height = 100,
  width = 400,
  new_id = NULL
) {
  if (is.null(dat) || nrow(dat) == 0) {
    return()
  }

  density_df <- with(
    dens,
    data.table::data.table(
      x = x,
      y = y
    )
  )

  dat <- data.table::copy(dat)
  dat$obs <- dat[[y_val]]

  # get x_min, x_max, and x_axis BEFORE filtering dat.
  x_min <- min(dat$date) - 7
  x_max <- max(dat$date) + 7

  x_axis <- list(
    label = "",
    range = c(x_min, x_max),
    tickvals = unique(dat$date),
    ticktext = as.character(unique(dat$date))
  )

  dat <- dat[!is.na(obs)]

  dat[,
    percentiles := sapply(obs, \(z) {
      with(
        density_df[density_df$x <= z, ],
        paste0(
          round(
            sum((x - data.table::shift(x)) * data.table::shift(y), na.rm = T) *
              100,
            digits = 2
          ),
          "%"
        )
      )
    })
  ]

  cut_values <- unique(unlist(cuts[, list(min_obs, max_obs)]))

  cuts_not_in_df <- cut_values[
    cut_values > 0 &
      is.finite(cut_values) &
      !cut_values %in% density_df$x
  ]

  if (length(cuts_not_in_df) > 0) {
    y_at_cuts <- sapply(cuts_not_in_df, \(cut) {
      mean(density_df$y[
        which(density_df$x == min(density_df$x[density_df$x > cut])) - c(0, 1)
      ])
    })

    density_df <- rbind(
      density_df,
      data.frame(x = cuts_not_in_df, y = y_at_cuts)
    )

    density_df <- density_df[order(density_df$x)]
  }

  y_max <- density_df[
    # cdf <= max of largest observed percentile, 0.95
    cumsum((x - data.table::shift(x, n = 1, type = "lag", fill = 0)) * y) <=
      max(as.numeric(gsub("%", "", dat$percentiles)) + 5, 95) / 100,
    max(x, cut_values[is.finite(cut_values)], max(dat$obs) * 1.12)
  ]

  density_df <- density_df[x <= y_max]

  # cuts <- cuts[, list(cuts = unique(c(min_obs, max_obs)))]$cuts
  cut_values[is.infinite(cut_values)] <- y_max

  trace_colors <- cuts$color

  dat[, obs_where := findInterval(obs, vec = cut_values)]
  dat[,
    hover_text := paste0(
      date,
      "<br>Age: ",
      age,
      "<br>",
      "Value: ",
      round(obs, digits = 3),
      "<extra></extra>"
    )
  ]

  p_scatter <- plotly::plot_ly(
    type = "scatter",
    mode = "lines"
  )

  ## Add shaded areas
  for (i in seq_along(cut_values)[-1]) {
    p_scatter <- p_scatter |>
      plotly::add_trace(
        x = c(x_min, x_min, x_max, x_max),
        y = c(
          cut_values[i - 1],
          cut_values[i],
          cut_values[i],
          cut_values[i - 1]
        ),
        fill = "toself",
        fillcolor = gsub("alpha|0.5", "0.2", trace_colors[i - 1]),
        # hovertemplate = "none",
        hoverinfo = "skip",
        name = paste0("band", i - 1),
        mode = "none"
      )
  }

  p_scatter <- p_scatter |>
    plotly::add_trace(
      data = dat,
      x = ~date,
      y = ~obs,
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0), # invisible; only the error bars show
      error_y = list(
        type = "data",
        array = ~ obs * 0.1, # upper extent
        arrayminus = ~ pmin(obs * 0.1, obs), # lower, clamped so it can't go below 0
        symmetric = FALSE,
        color = "rgba(0,0,0,1)",
        thickness = 2,
        width = 0
      ),
      name = "obs_outer",
      hoverinfo = "skip", # never intercepts hover
      showlegend = FALSE
    ) |>
    plotly::add_trace(
      data = dat,
      x = ~date,
      y = ~obs,
      type = "scatter",
      mode = "markers+lines",
      color = I("black"),
      hovertemplate = ~hover_text,
      name = "obs",
      error_y = list(
        type = "data",
        array = ~ obs * 0.05, # upper extent
        arrayminus = ~ pmin(obs * 0.05, obs), # lower, clamped so it can't go below 0
        symmetric = FALSE,
        color = "rgba(0,0,0,1)",
        thickness = 3,
        width = 0
      )
    ) |>
    plotly::layout(
      yaxis = list(
        title = "",
        autorangeoptions = list(
          minallowed = 0
        )
      ),
      xaxis = x_axis
    )

  # Normalize densities so that max = 1
  density_df$y <- density_df$y / max(density_df$y)

  p_density <- plotly::plot_ly(
    data = density_df,
    type = "scatter",
    mode = "lines"
  )

  for (i in seq_along(cut_values)[-1]) {
    p_density <- p_density |>
      plotly::add_trace(
        y = density_df$x[
          density_df$x <= cut_values[i] & density_df$x >= cut_values[i - 1]
        ],
        x = density_df$y[
          density_df$x <= cut_values[i] & density_df$x >= cut_values[i - 1]
        ],
        fill = "tozerox",
        fillcolor = gsub("alpha", "1", trace_colors[i - 1]),
        line = list(
          color = gsub("alpha", "1", trace_colors[i - 1]) #trace_colors[i - 1]
        ),
        hoverinfo = "none"
      )
  }

  cut_points <- cut_values[-c(1, length(cut_values))]

  p_density <- p_density |>
    plotly::add_trace(
      y = 0.95 * max(density_df$x),
      x = 0.95 * max(density_df$y),
      text = "&#8505;",
      mode = "text",
      hovertemplate = paste(
        "Density curve info<br> - Sample size:",
        dens$n,
        "<br> - Bandwidth:",
        dens$bw,
        "<extra></extra>"
      ),
      hoverlabel = list(
        align = "left"
      )
    ) |>
    plotly::layout(
      xaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        zeroline = TRUE,
        title = "",
        fixedrange = TRUE
        # range = c(-0.125, 1.05) * max(density_df$y)
      ),
      # yaxis = list(title = ""),
      shapes = lapply(cut_points, \(y) {
        list(
          type = "line",
          y0 = y,
          y1 = y,
          x0 = 0,
          x1 = 1,
          xref = "paper",
          yref = "y",
          line = list(color = "black", dash = "dot")
        )
      }),
      annotations = lapply(cut_points, function(yv) {
        list(
          x = 1,
          xref = "paper", # right edge of the plot
          y = yv,
          yref = "y",
          text = as.character(yv),
          showarrow = FALSE,
          xanchor = "left", # anchor text to start just past x=1
          xshift = 5, # small nudge further right, in pixels
          font = list(size = 10),
          align = "left"
        )
      }),
      showlegend = F,
      margin = list(r = 60)
    )

  return(list(scatter = p_scatter, density = p_density))
}

add_row_titles <- function(fig, titles, font_size = 13) {
  b <- plotly::plotly_build(fig)

  # one "obs" trace per row, in panel order -> its y-axis -> that axis's domain
  obs_axes <- unlist(lapply(b$x$data, function(tr) {
    if (identical(tr$name, "obs")) tr$yaxis %||% "y" else NULL
  }))

  tops <- vapply(
    obs_axes,
    function(ax) {
      key <- sub("^y", "yaxis", ax) # "y" -> "yaxis", "y3" -> "yaxis3"
      b$x$layout[[key]]$domain[2]
    },
    numeric(1)
  )

  new_anns <- lapply(seq_along(titles), function(i) {
    list(
      text = paste0(
        "<span style='font-weight: bold; text-decoration:underline'>",
        titles[i],
        "</span>"
      ),
      x = 0,
      xshift = -55,
      xref = "paper",
      xanchor = "left",
      y = tops[i],
      yref = "paper",
      yanchor = "bottom",
      yshift = 0,
      showarrow = FALSE,
      font = list(size = font_size)
    )
  })

  plotly::layout(fig, annotations = c(b$x$layout$annotations, new_anns))
}


long_bio_plot_js <- "function(el, x, data) {
  var gd = el;

  gd.removeAllListeners('plotly_hover');
  gd.removeAllListeners('plotly_unhover');

  // Group bands and obs traces by the y-axis they live on
  var panels = {};   // yaxis -> {bands: [{idx, lo, hi, low, high}], obs: [idx,...]}
  gd.data.forEach(function(tr, i) {
    var ax = tr.yaxis || 'y';
    panels[ax] = panels[ax] || {bands: [], obs: []};

    if (tr.name && tr.name.indexOf('band') !== -1 && tr.fill === 'toself') {
      var ys = tr.y.filter(function(v) { return v !== null; });
      panels[ax].bands.push({
        idx: i,
        lo: Math.min.apply(null, ys),
        hi: Math.max.apply(null, ys),
        low: tr.fillcolor,
        high: tr.fillcolor.replace(/[\\d.]+\\s*\\)$/, '0.8)')
      });
    } else if (tr.mode && tr.mode.indexOf('markers') !== -1) {
      panels[ax].obs.push(i);
    }
  });

  var current = null;   // {idx, low}

  function clear() {
    if (current !== null) {
      Plotly.restyle(gd, {fillcolor: current.low}, [current.idx]);
      current = null;
    }
  }

  gd.on('plotly_hover', function(d) {
    var pt = d.points[0];
    var tr = gd.data[pt.curveNumber];
    var ax = tr.yaxis || 'y';
    var p = panels[ax];
    if (!p || p.obs.indexOf(pt.curveNumber) === -1) return;  // only marker traces

    for (var i = 0; i < p.bands.length; i++) {
      var b = p.bands[i];
      if (pt.y >= b.lo && pt.y <= b.hi) {
        if (current && current.idx === b.idx) return;
        clear();
        Plotly.restyle(gd, {fillcolor: b.high}, [b.idx]);
        current = {idx: b.idx, low: b.low};
        return;
      }
    }
    clear();
  });

  gd.on('plotly_unhover', clear);
}"


on_render <- function(x, jsCode, data = NULL) {
  if (length(jsCode) > 1) {
    jsCode <- paste(jsCode, collapse = "\n")
  }
  x$jsHooks$render <- c(
    x$jsHooks$render,
    list(list(code = jsCode, data = data))
  )
  x
}
