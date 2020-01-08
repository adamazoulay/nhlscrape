context("Statistics equal")
library(nhlscrape)
SetDbPath(example = TRUE)

player_id <- 8475166
game_id <- 2019020001
team_id <- 10
test_that("Statistics for even strength and all situtations is the same", {
  expect_equal(GetPlayerStats(player_id, game_id, team_id)$CF[1], 28)
  expect_equal(GetPlayerStats(player_id, game_id, team_id)$CA[1], 12)
  expect_equal(GetPlayerStats(player_id, game_id, team_id)$CF[2], 13)
  expect_equal(GetPlayerStats(player_id, game_id, team_id)$CA[2], 10)
  expect_equal(GetPlayerStats(player_id, game_id, team_id)$S[1], 2)
  expect_equal(GetPlayerStats(player_id, game_id, team_id)$S[2], 2)
})
