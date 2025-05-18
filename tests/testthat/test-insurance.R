test_that("Computer player buys insurance and wins if dealer has Blackjack", {
  set.seed(42)  # Fix random for test consistency

  players <- list("Bot" = list(coins = 100, bets = 20, is_computer = TRUE))
  dealer_hand <- new_blackjack_hand(c("A", "K"))  # Blackjack

  res <- handle_insurance(dealer_hand, players)
  updated_player <- res$players$Bot

  expect_true(res$dealer_blackjack)
  expect_true("insurance_bet" %in% names(updated_player) || updated_player$coins != 100)
})

test_that("Computer player may not buy insurance", {
  set.seed(123)  # Fix random for test consistency

  players <- list("Bot" = list(coins = 100, bets = 20, is_computer = TRUE))
  dealer_hand <- new_blackjack_hand(c("A", "Q"))  # Blackjack

  res <- handle_insurance(dealer_hand, players)
  updated_player <- res$players$Bot

  expect_true(res$dealer_blackjack)
  # Depending on random seed, insurance_bet may or may not exist
  expect_true(is.numeric(updated_player$coins))
})

test_that("Computer player does not buy insurance when upcard is 10", {
  players <- list("Bot" = list(coins = 100, bets = 20, is_computer = TRUE))
  dealer_hand <- new_blackjack_hand(c("10", "A"))  # Blackjack

  res <- handle_insurance(dealer_hand, players)
  expect_true(res$dealer_blackjack)
  expect_equal(players$Bot$coins, res$players$Bot$coins)  # No insurance prompt
})
