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
EVENTS_EMPTY = TRUE

#================================================================
# Functions
#================================================================


#----------------------------------------------------------------
# Database manipulation block

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

QueryDb <- function(query) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  record <- DBI::dbGetQuery(conn, query)
  DBI::dbDisconnect(conn)

  return(record)
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

#' Add all events from a 'game_id' to the 'events' table
#'
#' @param game_id Identifying number for the game. Use GetGameIds to find.
#' @examples
#' AddGameEvents(2019020357)
#' AddGameEvents(2019020405)
AddGameEvents <- function(game_ids) {
  for (game_id in game_ids) {
    if (EVENTS_EMPTY == FALSE && nrow(QueryDb(paste("SELECT * FROM events WHERE game_id=", game_id))) > 0) {
      message(paste("game with game_id:'", game_id,"'already in database", sep=""))
      next
    }
    EVENTS_EMPTY = FALSE

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

#----------------------------------------------------------------
# Database retrieval block

GetTeamId <- function(team_name) {
  # Expect name to be either full name or abbreviation
  team_id <- rbind(QueryDb(paste("SELECT * FROM teams WHERE name='", team_name, "'", sep="")),
                   QueryDb(paste("SELECT * FROM teams WHERE abbreviation='", team_name, "'", sep="")),
                   QueryDb(paste("SELECT * FROM teams WHERE name_upper='", team_name, "'", sep=""))
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

# Date format "yyyy-mm-dd"
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

GetPlayerCorsi <- function(player_id, game_ids, team_id) {

  corsi <- data.frame(matrix(ncol = 3, nrow = 0))
  corsi_for_all <- 0
  corsi_against_all <- 0
  corsi_for_even <- 0
  corsi_against_even <- 0

  for (game_id in game_ids) {
    # CF all situations
    query <- paste("SELECT * FROM events WHERE game_id=", game_id,
                   " AND (playerType='Shooter' OR playerType='Scorer')",
                   " AND players_on_ice LIKE '%", player_id, "%'",
                   " AND player_team_id='", team_id, "'",
                   sep="")
    rows <- QueryDb(query)
    corsi_for_all <- corsi_for_all + nrow(rows)

    # CF in even strength situations
    for (i in 1:nrow(rows)) {
      if(i == 0) {
        next
      }
      row <- rows[i,]

      # Goalie in net check
      home_goalie <- as.logical(row$home_goalie)
      visitor_goalie <- as.logical(row$visitor_goalie)

      plrs <- strsplit(row$players_on_ice, ",")[[1]]
      plrs <- setdiff(plrs, "NA")
      if (length(plrs) == 12 && home_goalie && visitor_goalie) {
        corsi_for_even <- corsi_for_even + 1
      }
    }

    # CA in all situations
    query <- paste("SELECT * FROM events WHERE player_id!=", player_id,
                   " AND game_id=", game_id,
                   " AND (playerType='Shooter' OR playerType='Scorer')",
                   " AND players_on_ice LIKE '%", player_id, "%'",
                   " AND player_team_id!='", team_id, "'",
                   sep="")
    rows <- QueryDb(query)
    corsi_against_all <- corsi_against_all + nrow(rows)

    # CA at even strength
    for (i in 1:nrow(rows)) {
      if(i == 0) {
        next
      }
      row <- rows[i,]

      # Goalie in net check
      home_goalie <- as.logical(row$home_goalie)
      visitor_goalie <- as.logical(row$visitor_goalie)

      plrs <- strsplit(row$players_on_ice, ",")[[1]]
      plrs <- setdiff(plrs, "NA")
      if (length(plrs) == 12 && home_goalie && visitor_goalie) {
        corsi_against_even <- corsi_against_even + 1
      }
    }
  }

  corsi_all <- corsi_for_all - corsi_against_all
  corsi_even <- corsi_for_even - corsi_against_even
  corsi <- rbind(corsi, c(corsi_for_all, corsi_against_all, corsi_all), c(corsi_for_even, corsi_against_even, corsi_even))
  names(corsi) <- c("CF", "CA", "C")
  rownames(corsi) <- c("All_situations", "Even_strength")
  return(corsi)
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

GetGameLiveFeedHtml <- function(game_id) {
  # Get years in correct format
  year <- substring(game_id, 1, 4)
  next_year <- as.character(as.integer(year) + 1)
  years <- paste(year, next_year, sep="")

  id <- paste("PL", substring(game_id, 5), sep="")

  url <- paste("http://www.nhl.com/scores/htmlreports/", years, "/", id, ".HTM", sep="")

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
