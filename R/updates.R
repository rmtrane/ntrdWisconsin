#' In-app update hooks for ntrdWisconsin
#'
#' @description
#' These two functions are the convention-named exports that ntrd looks for
#' to enable in-app update notifications. They are produced by ntrd's GitHub
#' default factories and require no further configuration.
#'
#' \itemize{
#'   \item \code{ntrd_update_available()} compares the installed version of
#'     ntrdWisconsin against the latest version on GitHub and returns a
#'     structured list describing whether an update is available.
#'   \item \code{ntrd_update_extension()} installs the latest version from
#'     GitHub, including dependency upgrades. It is intended to be called
#'     from a freshly restarted R session in which ntrdWisconsin is not
#'     loaded.
#' }
#'
#' Users do not normally call these directly; ntrd's Shiny app discovers
#' and invokes them when the ntrdWisconsin data source is selected.
#'
#' @returns
#' For \code{ntrd_update_available()}: a list with fields \code{available},
#' \code{current}, \code{latest}, and \code{news_url}. See
#' \code{\link[ntrd]{default_github_update_available}} for details.
#'
#' For \code{ntrd_update_extension()}: invisibly returns \code{NULL}.
#'
#' @importFrom remotes install_github
#' @name ntrd_update
NULL

#' @rdname ntrd_update
#' @export
ntrd_update_available <- ntrd::default_github_update_available(
  "rmtrane/ntrdWisconsin"
)

#' @rdname ntrd_update
#' @export
ntrd_update_extension <- ntrd::default_github_update_extension(
  "rmtrane/ntrdWisconsin"
)
