# New env for global constants
nhlscrape.globals <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  nhlscrape.globals$user_set_db = FALSE
  nhlscrape.globals$db_file <- "No file found. Please set with SetDbPath()"
  nhlscrape.globals$api_url <- "https://statsapi.web.nhl.com/api/v1/"
  nhlscrape.globals$htmlrep_url <- "http://www.nhl.com/scores/htmlreports/"

  invisible()
}
