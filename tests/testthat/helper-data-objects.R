# Shared checkers for the bundled (lazy-loaded) data objects defined in
# R/nacc_to_wadrc_uds*.R, R/wadrc_uds*_to_nacc.R and R/wadrc_uds*_redcap_fields.R.
#
# These live in a helper so each per-object test file stays a faithful 1:1
# mirror of its R/ file without repeating the same battery of assertions.

# ---- named translation maps (NACC <-> WADRC) ------------------------------
# Documented format: a *named* character vector used as a lookup table, so
# names are keys (must be unique, non-empty) and values are the translated
# names (non-empty; not required to be unique -- several source fields may
# legitimately map onto one target field).
expect_valid_translation_map <- function(map, required_keys = character()) {
  obj <- deparse(substitute(map))

  expect_type(map, "character")
  expect_gt(length(map), 0L)

  nms <- names(map)
  expect_false(is.null(nms), info = paste0(obj, " must be named"))
  expect_true(all(nzchar(nms)), info = paste0(obj, ": all names must be non-empty"))
  expect_false(anyDuplicated(nms) > 0L, info = paste0(obj, ": names (keys) must be unique"))

  expect_false(anyNA(map), info = paste0(obj, ": values must not be NA"))
  expect_true(all(nzchar(map)), info = paste0(obj, ": values must be non-empty"))

  for (k in required_keys) {
    expect_true(k %in% nms, info = paste0(obj, " must contain the key '", k, "'"))
  }

  invisible(map)
}

# ---- plain REDCap field vectors -------------------------------------------
# Documented format: an (unnamed) character vector of REDCap field names.
expect_valid_field_vector <- function(fields) {
  obj <- deparse(substitute(fields))

  expect_type(fields, "character")
  expect_gt(length(fields), 0L)
  expect_false(anyNA(fields), info = paste0(obj, ": must not contain NA"))
  expect_true(all(nzchar(fields)), info = paste0(obj, ": must not contain empty strings"))
  expect_false(anyDuplicated(fields) > 0L, info = paste0(obj, ": field names must be unique"))

  invisible(fields)
}

# ---- inverse consistency on the shared overlap ----------------------------
# Only checks that where BOTH maps define a mapping for the same field the two
# directions agree (i.e. forward-then-back is the identity). It deliberately
# ignores fields covered by only one map, so differing sizes / asymmetric
# coverage never trigger a failure -- this catches *contradictions*, not
# incompleteness. Remove/relax if the two maps are not intended as inverses.
expect_inverse_on_overlap <- function(forward, reverse) {
  fwd <- deparse(substitute(forward))
  rev <- deparse(substitute(reverse))

  target <- unname(forward)
  shared <- target %in% names(reverse)
  if (!any(shared)) {
    skip(paste0("No overlapping fields between ", fwd, " and ", rev))
  }

  round_tripped <- unname(reverse[target[shared]])
  expect_equal(
    round_tripped,
    names(forward)[shared],
    info = paste0(fwd, " -> ", rev, " must be a consistent inverse on shared fields")
  )
}
