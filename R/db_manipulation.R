#' @keywords internal
#' Check if the record is in the db already
ExistsInDb <- function(table, pk) {
  query <- paste("SELECT * FROM ", table, " WHERE pk='", pk, "'", sep="")
  record <- QueryDb(query)

  if (nrow(record) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

#' @keywords internal
#' Add the dataframe to the database under 'table
AddDb <- function(table, df) {
  # Check that we have a user defined database. If not, stop with error.
  if (nhlscrape.globals$user_set_db == FALSE){
    stop("No user defined database found. Please use SetDbPath().")
  }
  # Add all rows to db, checking if the exist already
  conn <- DBI::dbConnect(RSQLite::SQLite(), nhlscrape.globals$db_file)

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


#' Add team metadata to the database
#'
#' Add all teams to the database. The teams will get stored in the database in the 'teams' table.
#' Will not write if teams already exist in the database!
#'
#' @examples
#' SetDbPath(example = TRUE)
#' AddAllTeamsDb()
#'
#' @export
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

#' Adds rosters for team id and a specific season
#'
#' Add a teams roster to the 'roster' table for season. Can be accessed with a primary key
#' of 'season_teamid_personid'
#'
#' @param team_id Int, identity number for the team. Use GetTeamId to find
#' @param season Int, A year range you want to add
#'
#' @examples
#' SetDbPath(example = TRUE)
#' AddTeamRoster(10, 20192020)
#'
#' @export
AddTeamRoster <- function(team_id, season) {
  request <- paste("teams/", team_id, "?expand=team.roster&season=", season, sep="")
  df <- jsonlite::flatten(GetApiJson(request)$teams$roster$roster[[1]])

  # Make names db friendly
  cols <- gsub("\\.", "_", names(df))
  names(df) <- cols

  # Make a new col for the pk
  df$pk <- paste(season, team_id, df$person_id, sep="_")
  df$team_id <- as.integer(team_id)

  # Add to database
  AddDb("rosters", df)
}

#' @keywords internal
#' Add the player rosters to the 'players' table, not adding duplicates
#' We also add the player toi to the player_toi table
AddRoster <- function(player_list, game_id) {
  # Add the players to the players table for searching
  # Add the time-on-ice for each player to the player_toi table
  player_df <- data.frame()
  toi_df <- data.frame(stringsAsFactors = FALSE)
  for (player in player_list) {
    # Player lists
    player_row <- data.frame(player[[1]])[,1:7]
    player_row <- within(player_row, rm("primaryNumber"))
    player_row <- cbind("pk"=player_row$id, player_row)
    player_row <- player_row[1:7] # This fixes the error if the player is missing a primary number
    player_df <- rbind(player_df, player_row)

    # Player toi
    stats <- player[[4]]
    if (length(stats) > 0) {
      toi_row <- cbind("pk"=paste(game_id, player_row$id, sep="_"),
                       "player_id"=player_row$id,
                       "time_on_ice"=player[[4]][[1]][[1]])
      toi_df <- rbind(toi_df, toi_row)
    }
  }
  AddDb("players", player_df)
  AddDb("player_toi", toi_df)
}

#' @keywords internal
#' Add some game metadata to the game_info table. Used for plotting and such
AddGameInfo <- function(game_data, game_id) {

  # Fields
  season <- game_data$gameData$game$season
  type <- game_data$gameData$game$type # Regular season, playoffs
  date_time <- game_data$gameData$datetime$dateTime
  home_team <- game_data$gameData$teams$home$name
  away_team <- game_data$gameData$teams$away$name
  venue <- game_data$gameData$venue$name

  row <- data.frame(game_id, season, type, date_time, home_team, away_team, venue)
  row <- cbind("pk"=row$game_id, row)
  AddDb("game_info", row)
}


#' Adds all game events for game_id to the database for analysis
#'
#' Add all events from a game_id to the 'events' table. Also adds all players in the
#' game to the 'players' table to allow for searching by name to retrieve player_id.
#' Finally, adds the total time on ice for each player to the 'player_toi' table. This
#' allows for calculation of certain statistics based on time usage.
#' Will not write if game already exists in the database!
#'
#' @param game_ids List, list of game_ids. Use GetGameIdRange to find
#'
#' @examples
#' SetDbPath(example = TRUE)
#' AddGameEvents(2019020001)
#'
#' @export
AddGameEvents <- function(game_ids) {
  for (game_id in game_ids) {
    if (EventsExists() && nrow(QueryDb(paste("SELECT * FROM events WHERE game_id=", game_id))) > 0) {
      message(paste("game with game_id:'", game_id,"' already in database", sep=""))
      next
    }

    message("Adding events for game_id:", game_id)

    request <- paste("game/", game_id, "/feed/live", sep="")
    df <- GetApiJson(request)

    # Need this for constant roster data
    request <- paste("game/", game_id, "/boxscore", sep="")
    boxscore <- GetApiJson(request)
    home_roster <- boxscore$teams$home$players
    away_roster <- boxscore$teams$away$players
    player_list <- c(home_roster, away_roster)

    # Add players to players table and boxscores to the players table
    # and add players time-on-ice to the players_toi table
    AddRoster(player_list, game_id)

    # Add game metadata info to the game_info table
    AddGameInfo(df, game_id)

    players <- df$liveData$plays$allPlays$players
    plays <- df$liveData$plays$allPlays
    plays <- within(plays, rm("players"))

    home_abbr <- df$gameData$teams$home$triCode
    visitor_abbr <- df$gameData$teams$away$triCode
    home_id <- GetTeamId(home_abbr)
    visitor_id <- GetTeamId(visitor_abbr)

    # We need to get the html report of the game so we an tell who was on the ice on any given play
    # All we need to do is match the times to any time in the report and insert the players
    html_report <- GetGameLiveFeedHtml(game_id)

    df_final <- data.frame()
    for (i in 1:length(players)) {
      row <- players[[i]]

      if (!is.null(row)) {
        for (j in 1:nrow(row)) {
          player <- row[j,]
          event <- plays[i,]

          # Now combine all info with the game id for the pk
          df <- jsonlite::flatten(cbind(player, event))
          cols <- gsub("\\.", "_", names(df))
          names(df) <- cols

          # Check if we have the seasonTotal field missing
          if (!("seasonTotal" %in% cols)) {
            df <- cbind(df, "seasonTotal"=NA)
          }

          # Finally make the pk field
          df$pk <- paste(game_id, df$about_eventIdx, df$player_id, sep="_")

          # Append player current team id for ident
          player_id_num <- paste("ID", row$player$id[[j]], sep="")
          player_team_id <- player_list[[player_id_num]]$person$currentTeam$id

          # If the player_team_id is NULL, we have to check the rosters to see
          # which one the player is on, and assign the id manually.
          # Thanks, David
          if (is.null(player_team_id)) {
            if (is.null(home_roster[[player_id_num]])) {
              # Then he plays for the away team
              player_team_id <- visitor_id
            } else {
              # He plays for the home team
              player_team_id <- home_id
            }
          }

          df <- cbind(df, "player_team_id"=player_team_id)

          # Get time stamp for player on ice identification
          period <- df$about_period
          time_period <- df$about_periodTime
          # Remove first zero to match html format
          if (substring(time_period, 1, 1) == "0"){
            time_period <- substring(time_period, 2)
          }

          # Set period_html and time_elapsed to NULL to avoid undefined global variable NOTEs
          period_html <- time_elapsed <- NULL
          tmp_event <- subset(html_report, period_html==period & time_elapsed==time_period)
          # Select first row
          tmp_row <- tmp_event[1,]

          # Sometimes we have to take the last row, like with the start of
          # period faceoffs
          if (is.na(tmp_row$visitor_p1)) {
            tmp_row <- tmp_event[nrow(tmp_event),]
          }

          # Add each player id to the current row
          df <- cbind(df, "players_on_ice"=paste(
            GetPlayerIdFromNumber(tmp_row$visitor_p1, away_roster),
            GetPlayerIdFromNumber(tmp_row$visitor_p2, away_roster),
            GetPlayerIdFromNumber(tmp_row$visitor_p3, away_roster),
            GetPlayerIdFromNumber(tmp_row$visitor_p4, away_roster),
            GetPlayerIdFromNumber(tmp_row$visitor_p5, away_roster),
            GetPlayerIdFromNumber(tmp_row$visitor_p6, away_roster),
            GetPlayerIdFromNumber(tmp_row$home_p1, home_roster),
            GetPlayerIdFromNumber(tmp_row$home_p2, home_roster),
            GetPlayerIdFromNumber(tmp_row$home_p3, home_roster),
            GetPlayerIdFromNumber(tmp_row$home_p4, home_roster),
            GetPlayerIdFromNumber(tmp_row$home_p5, home_roster),
            GetPlayerIdFromNumber(tmp_row$home_p6, home_roster), sep=",") )

          # Add goalie booleans from home and visitor
          df <- cbind(df, "home_goalie"=tmp_row$home_goalie)
          df <- cbind(df, "visitor_goalie"=tmp_row$visitor_goalie)

          df_final <- rbind(df_final, df)

        }
      } else {
        event <- plays[i,]

        df <- jsonlite::flatten(event)
        cols <- gsub("\\.", "_", names(df))
        names(df) <- cols
        df$pk <- paste(game_id, df$about_eventIdx, sep="_")

        # Add NA entries so column numbers match
        df <- cbind(df,
                    "playerType"=NA,
                    "player_id"=NA,
                    "player_team_id"=NA,
                    "player_fullName"=NA,
                    "player_link"=NA,
                    "seasonTotal"=NA,
                    "players_on_ice"=NA,
                    "home_goalie"=FALSE,
                    "visitor_goalie"=FALSE)
        df_final <- rbind(df_final, df)
      }

    }
    # Finally, add game_id column
    df_final <- cbind(df_final, game_id=rep(game_id, nrow(df_final)))

    AddDb("events", df_final)
    message(paste("for game_id:'", game_id, "'", sep=""))
  }
}
