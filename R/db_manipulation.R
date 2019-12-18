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
#' Add the dataframe to the database under 'table'
AddDb <- function(table, df) {
  # Add all rows to db, checking if the exist already
  conn <- DBI::dbConnect(RSQLite::SQLite(), getOption("db_file"))

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

#' Add a teams roster to the 'roster' table for 'season'.
#'
#' @param team_id Identity number for the team. Use GetTeamId to find.
#' @param season A year range you want to add.
#'
#' @examples
#' AddTeamRoster(10, 20192020)
#' AddTeamRoster(3, 20142015)
#'
#' @export
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

#' Add all events from a 'game_id' to the 'events' table
#'
#' @param game_ids List of identifying numbers for the game. Use GetGameIds to find.
#'
#' @examples
#' AddGameEvents(2019020001)
#'
#' @export
AddGameEvents <- function(game_ids) {
  for (game_id in game_ids) {
    if (EventsExists() && nrow(QueryDb(paste("SELECT * FROM events WHERE game_id=", game_id))) > 0) {
      message(paste("game with game_id:'", game_id,"'already in database", sep=""))
      next
    }

    request <- paste("game/", game_id, "/feed/live", sep="")
    df <- GetApiJson(request)

    # Need this for constant roster data
    request <- paste("game/", game_id, "/boxscore", sep="")
    boxscore <- GetApiJson(request)
    home_roster <- boxscore$teams$home$players
    away_roster <- boxscore$teams$away$players


    players <- df$liveData$plays$allPlays$players
    plays <- df$liveData$plays$allPlays
    plays <- within(plays, rm("players"))

    home_abbr <- df$gameData$teams$home$triCode
    visitor_abbr <- df$gameData$teams$away$triCode

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
          player_list <- c(home_roster, away_roster)
          player_id_num <- paste("ID", row$player$id[[j]], sep="")
          player_team_id <- player_list[[player_id_num]]$person$currentTeam$id
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
