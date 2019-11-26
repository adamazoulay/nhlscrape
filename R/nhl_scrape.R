#================================================================
# Setup
#================================================================
api_url <- "https://statsapi.web.nhl.com/api/v1/"

# Check if a db exists, if not then create an empty db
db_file <-"nhl.sqlite"
db_location <- "data_raw/"
db_path <- paste(db_location, db_file, sep="")
if (!file.exists(db_path)) {
  dir.create(db_location, showWarnings = FALSE)
  file.create(db_path)
}

#================================================================
# Functions
#================================================================


#----------------------------------------------------------------
# Database manipulation block

#' @keywords internal
#' @return boolean
#' Check if the record is in the db already
ExistsInDb <- function(table, pk) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  query <- paste("SELECT * FROM ", table, " WHERE pk='", pk, "'", sep="")
  record <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)

  if (nrow(record) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

#' @keywords internal
#' Add the dataframe to the database under 'table'
AddDb <- function(table, df) {
  # Add all rows to db, checking if the exist already
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Create the table if it doesn't exist yet (keep 0 rows)
  if (!DBI::dbExistsTable(conn, table)) {
    DBI::dbWriteTable(conn, table, df[FALSE,])
  }

  # Append the row if it doesn't exist yet
  for (i in 1:nrow(df)) {
    row <- df[i,]
    pk <- row$pk
    if (!ExistsInDb(table, pk)) {
      DBI::dbWriteTable(conn, table, row, append=TRUE, row.names=FALSE)
    }
  }
  DBI::dbDisconnect(conn)

  message(paste("'", table, "' rows added successfully", sep=""))
}

#' Add all teams to the database.
#'
#' @examples
#' AddAllTeamdDb()
AddAllTeamsDb <- function() {
  df <- jsonlite::flatten(GetApiJson("teams")$teams)

  # Make names db friendly
  cols <- gsub("\\.", "_", names(df))
  cols <- gsub("id", "pk", cols)
  names(df) <- cols

  # Remove duplicate id
  df <- within(df, rm("franchise_franchiseId"))

  # Add to database
  AddDb("teams", df)
}

#' Add a teams roster to the 'roster' table for 'season'.
#'
#' @param team_id Identity number for the team. Use GetTeamId to find.
#' @param season A year range you want to add.
#' @examples
#' AddTeamRoster(10, 20192020)
#' AddTeamRoster(3, 20142015)
AddTeamRoster <- function(team_id, season) {
  request <- paste("teams/", team_id, "?expand=team.roster&season=", season, sep="")
  df <- jsonlite::flatten(GetApiJson(request)$teams$roster$roster[[1]])

  # Make names db friendly
  cols <- gsub("\\.", "_", names(df))
  names(df) <- cols

  # Make a new col for the pk
  df$pk <- paste(df$person_id, season, sep="_")
  df$team_id <- as.integer(team_id)

  # Add to database
  AddDb("rosters", df)
}


#----------------------------------------------------------------
# Database retrieval block

GetTeamId <- function(team_name) {
  # Expect name to be either full name or abbreviation
  team_id <- rbind(GetQuery("teams",
                            conds=paste("WHERE name='", team_name, "'", sep="")),
                   GetQuery("teams",
                            conds=paste("WHERE abbreviation='", team_name, "'", sep=""))
  )
  if (nrow(team_id) == 0) {
    stop("Could not find team with name: ", team_name)
  }
  return(team_id$id)
}

GetTeamRoster <- function(team_id, year) {
  request <- paste("teams/", team_id, "/roster", sep="")
  r <- GetApiJson(request)

  roster <- r$roster
  return(c(roster$person,
           data.frame("jerseyNumber" = roster$jerseyNumber, stringsAsFactors = FALSE),
           roster$position))
}

GetPlayerStatsYears <- function(player_id, year_range) {
  request <- paste("people/",
                   player_id,
                   "/stats?stats=statsSingleSeason&season=",
                   year_range,
                   sep="")
  r <- GetApiJson(request)
  return(r$stats$splits[[1]]$stat)
}

GetGameIdNext <- function(team_id) {
  request <- paste("people/", team_id, "?expand=team.schedule.next", sep="")
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

  # Live data
  live_data <- r$liveData$plays

  # Extra data
  game_data <- r$gameData

  live_feed <- c(live_data, game_data)
  return(live_feed)
}


#----------------------------------------------------------------
# Scraping block (api and html)

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
