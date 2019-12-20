# New env for global constants
nhlscrape.globals <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  nhlscrape.globals$db_file <- system.file("extdata", "nhl.sqlite", package = "nhlscrape")
  nhlscrape.globals$api_url <- "https://statsapi.web.nhl.com/api/v1/"

  invisible()
}
