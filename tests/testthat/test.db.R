context("Db tests")
library(nhlscrape)
SetDbPath(example = TRUE)

test_that("We can connect to the database", {
  expect_equal(QueryDb("SELECT * FROM events WHERE game_id=2019020001")$result_event[1], "Game Scheduled")
})
