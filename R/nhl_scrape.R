api_url <- "https://statsapi.web.nhl.com/api/v1/"

GetApiJson <- function(call) {
  request <- paste(api_url, call, sep="")
  r <- httr::GET(request)

  # Logging
  log <- paste("[", Sys.time(), "] ", request, sep="")
  write(log, file="requests.log", append=TRUE)

  # Make sure we have the correct data from the GET request
  httr::stop_for_status(r)

  # Get the text and parse it from JSON to a table
  txt <- httr::content(r, "text")
  json <- jsonlite::fromJSON(txt)
  return(json)
}

GetTeams <- function() {
  return(GetApiJson("teams"))
}

GetTeamId <- function(team_name) {
  # Expect name to be either full name or abbreviation
  team_list <- GetTeams()
  team_names_full <- team_list$teams$name
  team_names_abbr <- team_list$teams$abbreviation

  team_id <- match(team_name, team_names_full)
  if (!is.na(team_id)) {
    return(team_id)
  }

  team_id <- match(team_name, team_names_abbr)
  if (!is.na(team_id)) {
    return(team_id)
  }

  stop("Could not find team with name: ", team_name)
}

GetGameIdNext <- function(team_id) {
  request <- paste("teams/", team_id, "?expand=team.schedule.next", sep="")
  r <- GetApiJson(request)
  return(r$teams$nextGameSchedule$dates[[1]]$games[[1]]$gamePk)
}

GetGameIdPrevious <- function(team_id) {
  request <- paste("teams/", team_id, "?expand=team.schedule.previous", sep="")
  r <- GetApiJson(request)
  return(r$teams$previousGameSchedule$dates[[1]]$games[[1]]$gamePk)
}

GetGameIdRange <- function(team_id, date_range) {
  request <- paste("schedule?teamId=", team_id, sep="")
  request <- paste(request, "&startDate=", date_range[1], sep="")
  request <- paste(request, "&endDate=", date_range[2], sep="")
  r <- GetApiJson(request)

  game_ids <- c()
  for (game in r$dates$games) {
    game_ids <- c(game_ids, game$gamePk)
  }

  return(game_ids)
}

GetGameLiveFeed <- function(game_id) {
  request <- paste("game/", game_id, "/feed/live", sep="")
  r <- GetApiJson(request)
  plays <- r$liveData$plays$allPlays

  # Gives times and other info about each play
  # "eventIdx"            "eventId"             "period"              "periodType"          "ordinalNum"
  # "periodTime"          "periodTimeRemaining" "dateTime"            "goals"
  about <- plays$about

  # Gives the location of all events, (0, 0) is center ice
  # "x" "y"
  coords <- plays$coordinates

  # Gives the type and descriptions of each event
  # "event"           "eventCode"       "eventTypeId"     "description"     "secondaryType"   "penaltySeverity"
  # "penaltyMinutes"  "strength"        "gameWinningGoal" "emptyNet"
  results <- plays$result

  feed <- c(results, coords, plays)
  return(feed)
}
