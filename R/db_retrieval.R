#' Send query to database
#'
#' Send a SQL query to the database. Retruns the SQL result as a data.frame. Useful for
#' seeing the structure of the database for construction of queries
#'
#' @param query A string containing the SQL query
#'
#' @examples
#' SetDbPath(example = TRUE)
#' AddGameEvents(2019020001)
#' QueryDb("SELECT * FROM events")
#' QueryDb("SELECT result_description FROM events WHERE game_id=2019020001 AND player_id=8475166")
#'
#' @return List, contains the SQL query result.
#'
#' @export
QueryDb <- function(query) {
  if (nhlscrape.globals$user_set_db == FALSE){
    stop("No user defined database found. Please use SetDbPath().")
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), nhlscrape.globals$db_file)
  record <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)

  return(record)
}

#' Retrieve database location on system
#'
#' Returns the current path to the local database file
#'
#' @examples
#' GetDbPath()
#'
#' @return Character, system path to the database.
#'
#' @export
GetDbPath <- function() {
  return(nhlscrape.globals$db_file)
}

#' Set database location on system
#'
#' Sets the path to the database file. This function must be run to allow operation
#' of other functions which require saving to a database
#'
#' @param db_path Character, contains the system path to the db file
#' @param example boolean, set to FALSE by default, if TRUE it will
#' copy the example db file and set the path pointing to it
#'
#' @examples
#' SetDbPath(example = TRUE)
#'
#' @return Character, contains the path to the database.
#'
#' @export
SetDbPath <- function(db_path="nhl.sqlite", example=FALSE) {
  if (example) {
    test_folder = tempdir()
    file.copy(system.file("extdata", "nhl.sqlite", package = "nhlscrape"), test_folder)
    db_path = file.path(test_folder, "nhl.sqlite")
  }
  nhlscrape.globals$user_set_db <-TRUE
  nhlscrape.globals$db_file <- db_path
}

#' @keywords internal
#' Check if the events table exists, returns boolean
EventsExists <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), nhlscrape.globals$db_file)
  result <- DBI::dbListTables(conn)
  DBI::dbDisconnect(conn)

  return("events" %in% result)
}

#' Search for team id from team name
#'
#' Retrieve the team ID using the abbreviation, or full name of the team
#'
#' @param team_name Character, contains either abbreviation or full name
#'
#' @return Int, team ID number.
#'
#' @examples
#' SetDbPath(example = TRUE)
#' AddAllTeamsDb()
#' GetTeamId("TOR")
#' GetTeamId("tor")
#' GetTeamId("Toronto Maple Leafs")
#'
#' @export
GetTeamId <- function(team_name) {
  # Expect name to be either full name or abbreviation
  team_id <- rbind(QueryDb(paste("SELECT * FROM teams WHERE name='", team_name, "'", sep="")),
                   QueryDb(paste("SELECT * FROM teams WHERE UPPER(abbreviation)='", toupper(team_name), "'", sep=""))
  )
  if (nrow(team_id) == 0) {
    stop("Could not find team with name: ", team_name)
  }
  return(team_id$pk)
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

#' Find game ids for a specific team and date range
#'
#' Gets a list of game ids for team_id in a specific date range, inclusive
#'
#' @param team_id Int, team ID number
#' @param start_date Starting date of the games, inclusive. Format: "yyyy-mm-dd"
#' @param end_date Ending date of the games, inclusive. Format: "yyyy-mm-dd"
#'
#' @return List of ints, each element is a game ID in selected range
#'
#' @examples
#' \donttest{
#' GetGameIdRange(10, "2019-09-30", "2019-12-16")
#' }
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

#' Search player id using name
#'
#' Gets a player id from their name. Will only work for players that were active in a game that
#' has already been added to the database
#'
#' @param player_name character, players full name
#'
#' @return int, player id number
#'
#' @examples
#' SetDbPath(example = TRUE)
#' AddGameEvents(2019020001)
#' GetPlayerId("John Tavares")
#'
#' @export
GetPlayerId <- function(player_name) {
  query <- paste("SELECT id FROM players WHERE UPPER(fullName)='", toupper(player_name), "'",
                 sep="")
  return(QueryDb(query)$id)
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

#' @keywords internal
#' Helper function to cut out rows based on conditional function, ex. IsEven
CutRows <- function(rows, fun) {
  return(rows[apply(rows, 1, fun),])
}

#' Get the heatmap coordinates for certain events in the database
#'
#' This function will take in a team_id and a list of game_ids, and a list of
#' events to looks for, and resturn a list of x, y coordinates, transformed to
#' be attacking zone on the left of the rink, and defending zone on the right of the rink.
#' Some anomalies due to how the NHL tracks positions
#'
#' @param team_id Int, id of the team to transfrom for
#' @param gids Int, list of game ids to check for events
#' @param events_list Character, string of events to select from the database
#' Note that events must be quoted in single quotes inside the string
#'
#' @return List, containing all x, y pairs for events and additional metadata
#'
#' @examples
#' SetDbPath(example = TRUE)
#' GetHeatmapCoords(10, 2019020001, "'Shot', 'Goal'")
#'
#' @export
GetHeatmapCoords <- function(team_id, gids, events_list) {
  all_rows <- 0
  for (game_id in gids) {
    #query <- paste("SELECT home_team, away_team FROM game_info WHERE game_id=", game_id, sep="")
    #row <- QueryDb(query)

    #if (GetTeamId(row$home_team) == team_id) {
    #  team_home <- TRUE
    #} else {
    #  team_home <- FALSE
    #}

    query <- paste("SELECT coordinates_x FROM events WHERE game_id=", game_id, " AND team_id=", team_id,
                   " AND result_event='Shot' AND about_period=1", sep = "")
    row <- QueryDb(query)
    avg = mean(row$coordinates_x)

    if (avg < 0) {
      flip <- TRUE
    } else {
      flip <- FALSE
    }

    query <- paste("SELECT DISTINCT coordinates_x,
    coordinates_y,
    result_description,
    about_period,
    about_periodTime,
    about_dateTime,
    game_id,
                   result_event FROM events WHERE game_id=", game_id,
                   " AND team_id=", team_id,
                   " AND result_event IN (", events_list, ")",
                   sep="")
    rows <- QueryDb(query)
    for (i in 1:nrow(rows)) {
      if (flip && rows[i,]$about_period %% 2 == 0) {
        # Flip second period
        rows[i,]$coordinates_x <- -1 * rows[i,]$coordinates_x
      } else if (!flip && rows[i,]$about_period %% 2 == 1) {
        # Flip first and last period
        rows[i,]$coordinates_x <- -1 * rows[i,]$coordinates_x
      }
    }
    if (typeof(all_rows) == "double") {
      all_rows <- rows
    } else {
      all_rows <- rbind(all_rows, rows)
    }
  }
  return(all_rows)
}

#' WIP - Get advanced statistics for player_id on team_id in a list of games.
#'
#'
#' This function will search all games in game_ids and return a list of stats for
#' the player id selected. The current stats returned are:
#' - Shots
#' - Goals
#' - Corsi
#' - Fenwick
#'
#' @param player_id Int, player ID number
#' @param game_ids List, game ids to check. Must already be in the database
#' @param team_id Int, the ID of the team the player plays for
#'
#' @return List, contains a row of stats for even strength and for all situations
#'
#' @examples
#' SetDbPath(example = TRUE)
#' AddGameEvents(2019020001)
#' GetPlayerStats(8475166, 2019020001, 10)
#'
#' @export
GetPlayerStats <- function(player_id, game_ids, team_id) {

  num_stats <- 3
  num_situations <- 2
  # Initialize stats df
  stats <- data.frame(matrix(ncol = num_stats, nrow = num_situations))
  names(stats) <- c("CF", "CA", "S")
  rownames(stats) <- c("All_situations", "Even_strength")


  gids_str <- paste(game_ids, collapse=",")

  # Corsi ----------------------------------------------------------------
  # CF in all situations
  query <- paste("SELECT * FROM events WHERE game_id IN (", gids_str, ")",
                 " AND (playerType='Shooter' OR playerType='Scorer')",
                 " AND players_on_ice LIKE '%", player_id, "%'",
                 " AND player_team_id='", team_id, "'",
                 sep="")
  CF_all <- QueryDb(query)

  # CA in all situations
  query <- paste("SELECT * FROM events WHERE player_id!=", player_id,
                 " AND game_id IN (", gids_str, ")",
                 " AND (playerType='Shooter' OR playerType='Scorer')",
                 " AND players_on_ice LIKE '%", player_id, "%'",
                 " AND player_team_id!='", team_id, "'",
                 sep="")
  CA_all <- QueryDb(query)

  # CF at even strength
  CF_even <- CutRows(CF_all, IsEven)

  # CA at even strength
  CA_even <- CutRows(CA_all, IsEven)

  stats["All_situations", c("CF", "CA")] <- c(nrow(CF_all), nrow(CA_all))
  stats["Even_strength", c("CF", "CA")] <- c(nrow(CF_even), nrow(CA_even))

  # Shots ----------------------------------------------------------------
  # Shots All Situations
  query <- paste("SELECT * FROM events WHERE game_id IN (", gids_str, ")",
                 " AND player_id=", player_id,
                 " AND (playerType='Shooter' OR playerType='Scorer')",
                 " AND (result_eventTypeId='SHOT' OR result_eventTypeId='GOAL')",
                 " AND about_periodType!='SHOOTOUT'",
                 sep="")
  S_all <- QueryDb(query)

  # Shots at even strength
  S_even <- CutRows(S_all, IsEven)


  stats["All_situations", "S"] <- nrow(S_all)
  stats["Even_strength", "S"] <- nrow(S_even)

  # Fenwic ----------------------------------------------------------------
  # FF in all situations
  query <- paste("SELECT * FROM events WHERE game_id IN (", gids_str, ")",
                 " AND result_eventTypeId!='BLOCKED_SHOT'",
                 " AND (playerType='Shooter' OR playerType='Scorer')",
                 " AND players_on_ice LIKE '%", player_id, "%'",
                 " AND player_team_id='", team_id, "'",
                 sep="")
  FF_all <- QueryDb(query)

  #FF at even strength NOT MATCHING UP!?!?
  FF_even <- CutRows(FF_all, IsEven)

  #FA in all situations
  query <- paste("SELECT * FROM events WHERE player_id!=", player_id,
                 " AND game_id IN (", gids_str, ")",
                 " AND result_eventTypeId!='BLOCKED_SHOT'",
                 " AND (playerType='Shooter' OR playerType='Scorer')",
                 " AND players_on_ice LIKE '%", player_id, "%'",
                 " AND player_team_id!='", team_id, "'",
                 sep="")
  FA_all <- QueryDb(query)

  #FA at even strength
  FA_even <- CutRows(FA_all, IsEven)

  # Goals ----------------------------------------------------------------
  # GF in all situations
  query <- paste("SELECT * FROM events  WHERE game_id IN (", gids_str, ")",
                 " AND playerType='Scorer'",
                 " AND players_on_ice LIKE '%", player_id, "%'",
                 " AND player_team_id='", team_id, "'",
                 sep="")
  GF_all <- QueryDb(query)

  #GF at even strength
  GF_even <- CutRows(GF_all, IsEven)

  # GA in all situations
  query <- paste("SELECT * FROM events  WHERE game_id IN (", gids_str, ")",
                 " AND playerType='Scorer'",
                 " AND players_on_ice LIKE '%", player_id, "%'",
                 " AND player_team_id!='", team_id, "'",
                 sep="")
  GA_all <- QueryDb(query)

  #GA at even strength
  GA_even <- CutRows(GA_all, IsEven)


  return(stats)
}
