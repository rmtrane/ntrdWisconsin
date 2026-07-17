# simulate_data_table.R
#
# Build a de-identified, panel-structured test fixture from a real data.table.
# The result mirrors the input's structure (column names, order, per-column
# types) but every value is independently re-sampled from its column's pool, so
# NO real row survives intact and each synthetic participant's profile is an
# independent assembly of real values -- de-identified by construction.
#
# One synthetic participant per drawn id, with within-participant constraints:
#   * `constant_cols` are drawn once per id and held constant across visits.
#   * Visit dates -- supplied EITHER as a single `date_col` OR as a `date_parts`
#     year/month/day triplet -- are drawn per row then sorted ascending within
#     id. The triplet is composed into a date, sorted, and decomposed back, so
#     the three columns stay mutually consistent and chronological.
#   * `age_col` is a per-id baseline incremented by the elapsed years since the
#     id's first visit, so it climbs coherently across the visit dates.
# A flat (one-row-per-person) source degenerates cleanly: each id gets a single
# visit and the constraints reduce to plain independent resampling.
#
# The sampling and constraint logic (including both date representations) was
# verified in R across character/integer/numeric/factor/Date/logical columns:
# types & factor levels preserved, NAs preserved, the sample(5)->1:5 length-1
# trap avoided, demographics constant within id, dates chronological within id,
# age anchored to visit dates, and output fully de-identified. The data.table
# layer is a thin wrapper over that verified vector logic.

#' Simulate a de-identified panel data.table matching an input's structure
#'
#' @param dat A `data.table` (or coercible).
#' @param id_col Participant-id column. Its real visit-count-per-participant
#'   distribution drives the panel structure; ids are replaced with synthetic
#'   values (`"sim00001"`, ...). Rows with a missing `id_col` are dropped.
#' @param constant_cols Columns held constant within each synthetic id (drawn
#'   once per id, independently per column, replicated across visits). E.g.
#'   `SEX`, `EDUC`, `RACE`, `HANDED`. Only the columns you name are constrained.
#' @param date_col Optional single visit-date column; drawn per row then sorted
#'   ascending within each id. Mutually exclusive with `date_parts`.
#' @param date_parts Optional length-3 character vector naming the year, month,
#'   and day columns (in that order), for sources that store the visit date as
#'   separate components (e.g. `c("VISITYR", "VISITMO", "VISITDAY")`). Composed
#'   into a date, sorted within id, and decomposed back into the three columns.
#'   Mutually exclusive with `date_col`.
#' @param age_col Optional age column; a per-id baseline plus the elapsed years
#'   since that id's first visit, so age is coherent across the visit dates.
#'   Requires a date representation (`date_col` or `date_parts`); without one,
#'   age is constant within id. Whole-number source ages are rounded (kept
#'   integer when source is integer).
#' @param n_ids Number of synthetic participants to generate. Default: the real
#'   participant count (an exact 1:1 mirror of the source structure). When
#'   supplied, per-participant visit counts are resampled from the real
#'   distribution, letting you make smaller or larger fixtures.
#' @param seed Optional integer for reproducibility.
#'
#' @return A `data.table` with the same columns (names, order, types) as `dat`.
#'
#' @examples
#' \dontrun{
#' # Raw pull with split date components:
#' real <- readRDS("data-raw/real_uds_pull.rds")
#' fixture <- simulate_data_table(
#'   real,
#'   id_col        = "NACCID",
#'   constant_cols = c("SEX", "EDUC", "RACE", "HANDED"),
#'   date_parts    = c("VISITYR", "VISITMO", "VISITDAY"),
#'   age_col       = "NACCAGE",
#'   n_ids         = 20,
#'   seed          = 1
#' )
#' }
simulate_data_table <- function(
  dat,
  id_col,
  constant_cols = character(),
  date_col = NULL,
  date_parts = NULL,
  age_col = NULL,
  n_ids = NULL,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  dat <- data.table::as.data.table(dat)
  cols <- names(dat)

  if (missing(id_col) || length(id_col) != 1L) {
    cli::cli_abort("`id_col` must be a single column name.")
  }
  if (!is.null(date_col) && !is.null(date_parts)) {
    cli::cli_abort("Supply only one of `date_col` or `date_parts`.")
  }
  if (!is.null(date_parts) && length(date_parts) != 3L) {
    cli::cli_abort(
      "`date_parts` must name the year, month, and day columns (length 3)."
    )
  }
  chk <- function(x, what) {
    if (length(x) && !all(x %in% cols)) {
      cli::cli_abort("{what} not found in `dat`: {.val {setdiff(x, cols)}}.")
    }
  }
  chk(id_col, "`id_col`")
  chk(constant_cols, "`constant_cols`")
  chk(date_col, "`date_col`")
  chk(date_parts, "`date_parts`")
  chk(age_col, "`age_col`")

  if (nrow(dat) == 0L) {
    return(data.table::copy(dat))
  }

  # index-based draw: preserves type/levels/NA and avoids the sample() length-1 trap
  resample_vec <- function(x, size) {
    x[sample.int(length(x), size = size, replace = TRUE)]
  }

  compose_date <- function(y, m, d) {
    res <- rep(as.Date(NA), length(y))
    ok <- !is.na(y) & !is.na(m) & !is.na(d)
    res[ok] <- as.Date(sprintf(
      "%04d-%02d-%02d",
      as.integer(y[ok]),
      as.integer(m[ok]),
      as.integer(d[ok])
    ))
    res
  }
  match_num <- function(x, template) {
    if (is.integer(template)) {
      as.integer(x)
    } else if (is.double(template)) {
      as.numeric(x)
    } else {
      x
    }
  }
  sort_within <- function(v, g) {
    unsplit(lapply(split(v, g), sort, na.last = TRUE), g)
  }

  # ---- panel structure: visits-per-participant ----
  counts_real <- as.integer(table(dat[[id_col]]))
  if (length(counts_real) == 0L) {
    cli::cli_abort("`id_col` {.val {id_col}} has no non-missing values.")
  }
  if (is.null(n_ids)) {
    counts <- counts_real # exact 1:1 mirror
  } else {
    counts <- counts_real[sample.int(
      length(counts_real),
      size = n_ids,
      replace = TRUE
    )]
  }
  n_ids <- length(counts)
  N <- sum(counts)
  gid <- rep(seq_len(n_ids), times = counts)

  out <- list()
  out[[id_col]] <- sprintf("sim%05d", gid)

  # constant within id: one draw per id, replicated
  for (cc in constant_cols) {
    out[[cc]] <- resample_vec(dat[[cc]], n_ids)[gid]
  }

  # per-visit columns: independent per row (exclude id/constant/date/age)
  date_cols_all <- c(date_col, date_parts)
  handled <- c(id_col, constant_cols, date_cols_all, age_col)
  for (col in setdiff(cols, handled)) {
    out[[col]] <- resample_vec(dat[[col]], N)
  }

  # visit dates: draw + sort ascending within id (single column or y/m/d triplet)
  visit_dates <- NULL
  if (!is.null(date_col)) {
    drawn <- sort_within(resample_vec(dat[[date_col]], N), gid)
    out[[date_col]] <- drawn
    visit_dates <- as.Date(drawn)
  } else if (!is.null(date_parts)) {
    yc <- date_parts[[1]]
    mc <- date_parts[[2]]
    dc <- date_parts[[3]]
    drawn <- sort_within(
      resample_vec(compose_date(dat[[yc]], dat[[mc]], dat[[dc]]), N),
      gid
    )
    parts <- list(
      as.integer(format(drawn, "%Y")),
      as.integer(format(drawn, "%m")),
      as.integer(format(drawn, "%d"))
    )
    out[[yc]] <- match_num(parts[[1]], dat[[yc]])
    out[[mc]] <- match_num(parts[[2]], dat[[mc]])
    out[[dc]] <- match_num(parts[[3]], dat[[dc]])
    visit_dates <- drawn
  }

  # age: per-id baseline + elapsed years since first visit
  if (!is.null(age_col)) {
    age <- resample_vec(dat[[age_col]], n_ids)[gid]
    if (!is.null(visit_dates)) {
      first_d <- unsplit(
        lapply(split(visit_dates, gid), function(z) {
          rep(if (all(is.na(z))) z[1] else min(z, na.rm = TRUE), length(z))
        }),
        gid
      )
      age <- age + as.numeric(visit_dates - first_d) / 365.25
    }
    if (isTRUE(all(dat[[age_col]] == round(dat[[age_col]]), na.rm = TRUE))) {
      age <- round(age)
      if (is.integer(dat[[age_col]])) age <- as.integer(age)
    }
    out[[age_col]] <- age
  }

  data.table::setDT(out)
  data.table::setcolorder(out, cols)
  out[]
}
