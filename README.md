# nhlscrape
A package for scraping the nhl API and HTML reports to build a database. Can be used to generate statistics from the information pulled.

## Usage
To use this package:
1. Import 'it'nhlscraper' and a SQL database will be created in the package directory. It can be found and set using GetDbPath and SetDbPath commands.
2. Find team_id using the GetTeamId function.
3. Find game_ids using the GetGameIdRange function.
4. Add game reports to the database using GameEvents.
5. Find a player_id using the GetPlayerId('player_name') function.
6. Get player statistics using GetPlayerStats. You can also manually query the database using QueryDb.


Example:
```
library(nhlscrape)

# Set db path to somewhere
SetDbPath("C:/Users/Adam/Documents/nhl.sqlite")

# Select the leafs
AddAllTeamsDb()
team_id <- GetTeamId("toronto maple leafs")
gids <- GetGameIdRange(team_id, "2019-09-30", "2019-12-18")

# Add games
AddGameEvents(gids)

# Get stats for player
# Tavares
player_id <- GetPlayerId("john tavares")

stats <- GetPlayerStats(player_id, gids, team_id)
```
