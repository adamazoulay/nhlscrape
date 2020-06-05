# nhlscrape

A package for scraping the nhl API and HTML reports to build a database. Can be used to generate statistics from the information pulled.

## Usage

To use this package:
1. Install the nhlscrape package from GitHub or from CRAN (see below for details).
2. Import 'nhlscrape' and run SetDbPath to set the location for the database file.
3. Use AddAllTeams() and the find team_id using the GetTeamId function.
4. Find game_ids using the GetGameIdRange() function.
5. Add game reports to the database using AddGameEvents().
6. Find a player_id using the GetPlayerId('player_name') function.
7. Get player statistics using GetPlayerStats(). You can also manually query the database using QueryDb.


#### Installing:

From GitHub:
```
install.packages("devtools")
library(devtools)
install_github("adamazoulay/nhlscrape")
```

From CRAN:
```
install.packages("nhlscrape")
```

#### Usage example:

```
library(nhlscrape)

# Set db path to somewhere
SetDbPath("C:/Users/Adam/Documents/nhl.sqlite")

# Select the leafs
AddAllTeamsDb()
team_id <- GetTeamId("Toronto Maple Leafs")
gids <- GetGameIdRange(team_id, "2019-09-30", "2019-12-18")

# Add games
AddGameEvents(gids)

# Get stats for player
# Tavares
player_id <- GetPlayerId("john tavares")

stats <- GetPlayerStats(player_id, gids, team_id)
```

## Example Shiny App

You can see an example of the heatmap function being plotted in this demo shiny app at:
https://aazoulay.shinyapps.io/nhl_shiny/
