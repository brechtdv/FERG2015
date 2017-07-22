## package startup message
## added 22/07/2017

.onAttach <- function(...) {
  if (!interactive()) return()

  packageStartupMessage(
    paste("FERG2015 / version", packageVersion("FERG2015")))
}
