test_that("simulate_blackjack_with_split runs without error", {
  res <- simulate_multiplayer_blackjack(num_rounds = 10)
  expect_true(is.list(res))
  expect_true("players" %in% names(res))
})
test_that("simulate works for different split/buy_insurance strategies", {
  res1 <- simulate_multiplayer_blackjack(num_rounds = 10, split = TRUE, buy_insurance = FALSE)
  res2 <- simulate_multiplayer_blackjack(num_rounds = 10, split = FALSE, buy_insurance = TRUE)
  expect_true(is.numeric(res1$players$Player1$win_rate))
  expect_true(is.numeric(res2$players$Player2$win_rate))
})

test_that("simulate handles custom split function", {
  res <- simulate_multiplayer_blackjack(
    num_rounds = 10,
    split = function(hand) { FALSE }
  )
  expect_true(is.list(res))
})
test_that("simulate_blackjack fails on bad split", {
  expect_error(simulate_multiplayer_blackjack(num_rounds = 5, split = "xxx"))
})
test_that("simulation returns correct structure", {
  res <- simulate_multiplayer_blackjack(num_rounds = 10)
  expect_true(all(c("win_rate", "lose_rate", "tie_rate", "final_coins") %in% names(res$players$Player1)))
})
test_that("simulate handles custom insurance function", {
  res <- simulate_multiplayer_blackjack(
    num_rounds = 10,
    buy_insurance = function(player, dealer_hand) { FALSE }
  )
  expect_true(is.list(res))
  expect_true("insurance_win_rate" %in% names(res$players$Player1))
})
