test_that("computer player bets 10 or less", {
  players <- list(
    Bot = list(is_computer = TRUE, coins = 8)
  )
  updated <- get_bets(players)

  expect_equal(updated$Bot$bets, 8)
  expect_equal(updated$Bot$coins, 8)
})


test_that("computer player bets 10 when coins >= 10", {
  players <- list(
    Bot = list(is_computer = TRUE, coins = 15)
  )
  updated <- get_bets(players)

  expect_equal(updated$Bot$bets, 10)
  expect_equal(updated$Bot$coins, 15)  # unchanged
})
