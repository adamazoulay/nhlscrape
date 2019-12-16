# nhlscrape
A package for scraping the nhl API and HTML reports to build a database. Can be used to generate statistics from the information pulled.

## Usage
To use this package:
1. Import 'it'nhlscraper' and a SQL database will be created under ./data_raw (TODO: config file with db location).
2. Find team_id using the GetTeamId function.
3. Find game_id using the GetGameIdRange function.
4. Add game reports to the database using GameEvents.
5. Get player statistics using GetPlayerStats. You can also manually query the database using QueryDb.

(TODO: You can search for player IDs by using the FindPlayerId function.)

Example:
```
team_id <- GetTeamId("TOR")
gids <- GetGameIds(team_id, "2019-09-30", "2019-12-16")
AddGameEvents(gids)
player_id <- FindPlayerId("John Tavares")
stats <- GetPlayerStats(player_id, gids, team_id)
```
