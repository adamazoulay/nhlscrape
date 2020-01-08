#' Call the nhl api and parse the retun
#'
#' Send a call to the 'NHL' api and get the result back in a list
#'
#' @param call character, contains the api query
#'
#' @examples
#' \donttest{
#' GetApiJson("game/2019020001/feed/live")
#' }
#'
#' @return list, contains all the data in the api call
#'
#' @export
GetApiJson <- function(call) {
  request <- paste(nhlscrape.globals$api_url, call, sep="")
  r <- httr::GET(request)

  # Logging
  #log <- paste("[", Sys.time(), "] ", request, sep="")
  #write(log, file="./nhlscrape/requests.log", append=TRUE)

  # Make sure we have the correct data from the GET request
  httr::stop_for_status(r)

  # Get the text and parse it from JSON to a table
  txt <- httr::content(r, "text")
  json <- jsonlite::fromJSON(txt)
  return(json)
}

#' @keywords internal
#' Scrapes and returns the html report for game_id
GetGameLiveFeedHtml <- function(game_id) {
  # Get years in correct format
  year <- substring(game_id, 1, 4)
  next_year <- as.character(as.integer(year) + 1)
  years <- paste(year, next_year, sep="")

  id <- paste("PL", substring(game_id, 5), sep="")

  url <- paste(nhlscrape.globals$htmlrep_url, years, "/", id, ".HTM", sep="")

  content <- xml2::read_html(url)

  plays_data <- rvest::html_nodes(content, xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "bborder", " " ))]')
  visitor_abbr <- substring(rvest::html_text(plays_data[7]), 1, 3)
  home_abbr <- substring(rvest::html_text(plays_data[8]), 1, 3)

  col <- 1
  row <- c()
  options(stringsAsFactors=FALSE)
  data <- data.frame()
  for (i in 1:length(plays_data)) {

    # If col 4 (time), we need to take only the time elapsed
    if (col == 4) {
      str <- as.character(plays_data[i])
      str <- gsub("<br>", "\n", str)
      val <- rvest::html_text(xml2::read_html(str))
      val <- strsplit(val, "\n")[[1]][1]
    } else if (col == 7) {
      # Parse players into lists
      str <- rvest::html_text(plays_data[i])
      plrs <- strsplit(str, "\r\n\r\n")[[1]]

      away_p1 <- substring(plrs[2], 3)
      away_p2 <- substring(plrs[5], 3)
      away_p3 <- substring(plrs[8], 3)
      away_p4 <- substring(plrs[11], 3)
      away_p5 <- substring(plrs[14], 3)
      away_p6 <- substring(plrs[17], 3)

      # Add a check for the goalies on the ice for 5v5 stat calculations
      goalie <- FALSE
      check <- plrs[3] == "G" ||
        plrs[6] == "G" ||
        plrs[9] == "G" ||
        plrs[12] == "G" ||
        plrs[15] == "G" ||
        plrs[18] == "G"
      if (!is.na(check) && check) {
        goalie <- TRUE
      }

      row <- c(row, goalie, away_p1, away_p2, away_p3, away_p4, away_p5)
      val <- away_p6

    } else if (col == 8) {
      # Parse players into lists
      str <- rvest::html_text(plays_data[i])
      plrs <- strsplit(str, "\r\n\r\n")[[1]]

      home_p1 <- substring(plrs[2], 3)
      home_p2 <- substring(plrs[5], 3)
      home_p3 <- substring(plrs[8], 3)
      home_p4 <- substring(plrs[11], 3)
      home_p5 <- substring(plrs[14], 3)
      home_p6 <- substring(plrs[17], 3)

      # Add a check for the goalies on the ice for 5v5 stat calculations
      goalie <- FALSE
      check <- plrs[3] == "G" ||
        plrs[6] == "G" ||
        plrs[9] == "G" ||
        plrs[12] == "G" ||
        plrs[15] == "G" ||
        plrs[18] == "G"
      if (!is.na(check) && check) {
        goalie <- TRUE
      }

      row <- c(row, goalie, home_p1, home_p2, home_p3, home_p4, home_p5)
      val <- home_p6

    } else {
      val <- rvest::html_text(plays_data[i])
    }

    row <- c(row, val)

    # Reset the column number every 8 nodes, and append row
    col <- col + 1
    if (col == 9) {
      data <- rbind(data, row)
      row <- c()
      col <- 1
    }
  }
  names(data) <- c("ev_html", "period_html", "strength_htm", "time_elapsed", "event_html", "description_html",
                   "visitor_goalie", "visitor_p1", "visitor_p2", "visitor_p3", "visitor_p4", "visitor_p5", "visitor_p6",
                   "home_goalie", "home_p1", "home_p2", "home_p3", "home_p4", "home_p5", "home_p6")
  data <- data[-1,]
  return(data)
}
