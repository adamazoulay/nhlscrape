#' Send a SQL query to the database.
#'
#' @param query A string containing the SQL query.
#'
#' @examples
#' QueryDb("SELECT * FROM events")
#' QueryDb("SELECT result_description FROM events WHERE game_id=2019020001 AND player_id=8475166")
#'
#' @return Dataframe containing the SQL query result.
#'
#' @export
QueryDb <- function(query) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), getOption("db_file"))
  record <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)

  return(record)
}

#' Returns the current path to the database file.
#'
#' @examples
#' GetDbPath()
#'
#' @return String containing the path to the database.
#'
#' @export
GetDbPath <- function() {
  return(getOption("db_file"))
}

#' Sets the current path to the database file.
#'
#' @param db_path A string containing the path to the db file.
#'
#' @examples
#' SetDbPath(system.file("extdata", "nhl.sqlite", package = "nhlscrape"))
#'
#' @return String containing the path to the database.
#'
#' @export
SetDbPath <- function(db_path) {
  return(options(db_file=db_path))
}

#' Check if the events table exists, returns boolean
#' @keywords internal
EventsExists <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), getOption("db_file"))
  result <- DBI::dbListTables(conn)
  DBI::dbDisconnect(conn)

  return("events" %in% result)
}

#' Retrieve the team ID using the abbreviation, or full name of the team.
#'
#' @param team_name String containing either abbreviation or full name.
#'
#' @return Int, team ID number.
#'
#' @examples
#' AddAllTeamsDb()
#' GetTeamId("TOR")
#' GetTeamId("Toronto Maple Leafs")
#'
#' @export
GetTeamId <- function(team_name) {
  # Expect name to be either full name or abbreviation
  team_id <- rbind(QueryDb(paste("SELECT * FROM teams WHERE name='", team_name, "'", sep="")),
                   QueryDb(paste("SELECT * FROM teams WHERE abbreviation='", team_name, "'", sep=""))
  )
  if (nrow(team_id) == 0) {
    stop("Could not find team with name: ", team_name)
  }
  return(team_id$id)
}

#' @keywords internal
GetTeamRoster <- function(team_id, year) {
  request <- paste("teams/", team_id, "/roster", sep="")
  r <- GetApiJson(request)

  roster <- r$roster
  return(c(roster$person,
           data.frame("jerseyNumber" = roster$jerseyNumber, stringsAsFactors = FALSE),
           roster$position))
}

#' @keywords internal
GetPlayerStatsYears <- function(player_id, year_range) {
  request <- paste("people/",
                   player_id,
                   "/stats?stats=statsSingleSeason&season=",
                   year_range,
                   sep="")
  r <- GetApiJson(request)
  return(r$stats$splits[[1]]$stat)
}

#' @keywords internal
GetGameIdNext <- function(team_id) {
  request <- paste("people/", team_id, "?expand=team.schedule.next", sep="")
  r <- GetApiJson(request)
  return(r$teams$nextGameSchedule$dates[[1]]$games[[1]]$gamePk)
}

#' @keywords internal
GetGameIdPrevious <- function(team_id) {
  request <- paste("teams/", team_id, "?expand=team.schedule.previous", sep="")
  r <- GetApiJson(request)
  return(r$teams$previousGameSchedule$dates[[1]]$games[[1]]$gamePk)
}

#' Gets a list of game ids for team_id in a specific date range.
#'
#' @param team_id Team ID number.
#' @param start_date Starting date of the games, inclusive. Format: "yyyy-mm-dd".
#' @param end_date Ending date of the games, inclusive. Format: "yyyy-mm-dd".
#'
#' @return List of ints, each element is a game ID in selected range.
#'
#' @examples
#' GetGameIdRange(10, "2019-09-30", "2019-12-16")
#'
#' @export
GetGameIdRange <- function(team_id, start_date, end_date) {
  request <- paste("schedule?teamId=", team_id, sep="")
  request <- paste(request, "&startDate=", start_date, sep="")
  request <- paste(request, "&endDate=", end_date, sep="")
  r <- GetApiJson(request)

  game_ids <- c()
  for (game in r$dates$games) {
    game_ids <- c(game_ids, game$gamePk)
  }

  return(game_ids)
}

#' @keywords internal
#' Helper used to get the player id from their jersey number in a specific game.
GetPlayerIdFromNumber <- function(number, player_list) {
  player_id <- NA
  if (is.na(number)) {
    return(player_id)
  }
  for (player in player_list) {
    if (number == player$jerseyNumber) {
      player_id <- player$person$id
    }
  }
  return(player_id)
}

#' @keywords internal
#' Helper function for checking if a play is even strength, checks the goalies
#' and the total player count, returns boolean
IsEven <- function(row) {
  if(is.na(row["players_on_ice"])) {
    return(FALSE)
  }
  # Goalie in net check
  home_goalie <- as.logical(row["home_goalie"])
  visitor_goalie <- as.logical(row["visitor_goalie"])

  plrs <- strsplit(row["players_on_ice"], ",")[[1]]
  plrs <- setdiff(plrs, "NA")
  is_even <- length(plrs) == 12 && home_goalie && visitor_goalie
  return(is_even)
}

#' WIP - Get advanced statistics for player_id on team_id in a list of games.
#'
#' @param player_id Player ID number.
#' @param game_ids List of game ids to check. Must already be in the database.
#' @param team_id The ID of the team the player plays for.
#'
#' @return Dataframe containing a row of stats for even strength and for all situations.
#'
#' @examples
#' AddGameEvents(2019020001)
#' GetPlayerStats(8475166, 2019020001, 10)
#'
#' @export
GetPlayerStats <- function(player_id, game_ids, team_id) {

  # Initialize stats df
  stats <- data.frame(matrix(ncol = 3, nrow = 0))
  CF_all <- 0
  CA_all <- 0
  CF_even <- 0
  CA_even <- 0

  for (game_id in game_ids) {

    # Corsi
    #----------------------------------------------------------------
    # CF all situations
    query <- paste("SELECT * FROM events WHERE game_id=", game_id,
                   " AND (playerType='Shooter' OR playerType='Scorer')",
                   " AND players_on_ice LIKE '%", player_id, "%'",
                   " AND player_team_id='", team_id, "'",
                   sep="")
    rows <- QueryDb(query)
    CF_all <- CF_all + nrow(rows)

    # CF in even strength situations
    rows <- rows[apply(rows, 1, IsEven),]
    CF_even <- CF_even + nrow(rows)

    # CA in all situations
    query <- paste("SELECT * FROM events WHERE player_id!=", player_id,
                   " AND game_id=", game_id,
                   " AND (playerType='Shooter' OR playerType='Scorer')",
                   " AND players_on_ice LIKE '%", player_id, "%'",
                   " AND player_team_id!='", team_id, "'",
                   sep="")
    rows <- QueryDb(query)
    CA_all <- CA_all + nrow(rows)

    # CA at even strength
    rows <- rows[apply(rows, 1, IsEven),]
    CA_even <- CA_even + nrow(rows)




  }


  # Finalize stats df
  # Corsi
  corsi_all <- CF_all - CA_all
  corsi_even <- CF_even - CA_even


  stats <- rbind(stats, c(CF_all, CA_all, corsi_all), c(CF_even, CA_even, corsi_even))
  names(stats) <- c("CF", "CA", "C")
  rownames(stats) <- c("All_situations", "Even_strength")
  return(stats)
}
