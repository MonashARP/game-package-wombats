test_that("Computer player splits pair of 8s or Aces", {
  deck <- c("5♠", "9♦", "2♥", "K♣")
  hand <- new_blackjack_hand(c("8♠", "8♥"))
  player_hands <- list("Bot" = list(hand))
  players <- list("Bot" = list(is_computer = TRUE))

  res <- handle_splitting(player_hands, deck, players)

  expect_length(res$player_hands$Bot, 2)
  expect_true(all(length(res$player_hands$Bot[[1]]$cards) == 2))
  expect_equal(res$player_hands$Bot[[1]]$cards[1], "8♠")
  expect_equal(res$player_hands$Bot[[2]]$cards[1], "8♥")
})

test_that("Computer player does NOT split pair of 9s", {
  deck <- c("5♠", "9♦", "2♥", "K♣")
  hand <- new_blackjack_hand(c("9♠", "9♥"))
  player_hands <- list("Bot" = list(hand))
  players <- list("Bot" = list(is_computer = TRUE))

  res <- handle_splitting(player_hands, deck, players)

  expect_length(res$player_hands$Bot, 1)  # Should not split
  expect_equal(res$player_hands$Bot[[1]]$cards, c("9♠", "9♥"))
})
