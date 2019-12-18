# API and db global config
.onLoad <- function(libname, pkgname) {
  db_path <- system.file("extdata", "nhl.sqlite", package = "nhlscrape")
  options(api_url='https://statsapi.web.nhl.com/api/v1/', db_file=db_path)
}
