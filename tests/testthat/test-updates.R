# Covers R/updates.R -> ntrd_update_available, ntrd_update_extension
#
# Both are closures built by ntrd's GitHub factories (whose logic is tested in
# ntrd). What this package controls is that both hooks are exported, and the
# repo string. Both are checked offline:
# - ntrd_update_extension(): remotes::install_github() is replaced by a recorder.
# - ntrd_update_available(): ntrd's GitHub version fetch is replaced by a stub.
#
# covr: updates.R has no function bodies of its own (the closures' code lives
# in ntrd), so these tests add no covered lines. They guard the contract.
#
# Code vs docs (the tests assert the code): @returns says
# ntrd_update_available() returns "a list with fields ...", but the factory
# returns an ntrd::update_result S7 object.

repo <- "rmtrane/ntrdWisconsin"

test_that("both update hooks are exported, argument-free functions", {
  # This mirrors what ntrd checks before enabling in-app updates. If only one
  # were exported, ntrd would warn and disable updates.
  exports <- getNamespaceExports("ntrdWisconsin")
  for (nm in c("ntrd_update_available", "ntrd_update_extension")) {
    expect_true(nm %in% exports, label = nm)
    f <- getExportedValue("ntrdWisconsin", nm)
    expect_true(is.function(f), label = nm)
    expect_length(formals(f), 0L) # ntrd calls them with no arguments
  }
})

test_that("ntrd_update_extension() installs this package's GitHub repo", {
  seen <- NULL
  local_mocked_bindings(
    install_github = function(repo, ...) seen <<- list(repo = repo, ...),
    .package = "remotes"
  )

  expect_invisible(ntrd_update_extension())
  expect_identical(seen$repo, repo)
  expect_identical(seen$upgrade, "always")
})

test_that("ntrd_update_available() checks this package against its GitHub repo", {
  seen <- NULL
  local_mocked_bindings(
    get_github_version = function(repo) {
      seen <<- repo
      package_version("999.0.0")
    },
    .package = "ntrd"
  )

  res <- ntrd_update_available()

  expect_identical(seen, repo)
  expect_true(S7::S7_inherits(res, ntrd::update_result)) # not a list (see header)
  expect_identical(
    S7::prop(res, "current"),
    as.character(utils::packageVersion("ntrdWisconsin"))
  )
  expect_identical(S7::prop(res, "latest"), "999.0.0")
  expect_true(S7::prop(res, "available"))
  expect_identical(
    S7::prop(res, "news_url"),
    "https://github.com/rmtrane/ntrdWisconsin/blob/HEAD/NEWS.md"
  )
})
