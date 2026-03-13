.onLoad <- function(...) {
  S7::methods_register()
}

.onAttach <- function(...) {
  ## Set defaults when attached, i.e. when library(ntrdWisconsin) is called (or, as is the case in ntrd shiny app, when attachNamespace(ntrdWisconsin) is called.)
  .set_defaults()
}

.set_defaults <- function() {
  ntrs::set_std_defaults(ntrs::WAIS(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REY1REC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REY2REC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REY3REC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REY4REC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REY5REC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REY6REC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REYDREC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REYTOTAL(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REYAREC(), "tscores", overwrite = TRUE)
  ntrs::set_std_defaults(ntrs::REYDLIST(), "tscores", overwrite = TRUE)
}
