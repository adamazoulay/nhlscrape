context("Db tests")
library(nhlscrape)

test_that("We can connect to the database", {
  expect_equal(QueryDb("SELECT * FROM events WHERE game_id=2019020001")$result_event[1], "Game Scheduled")
})
