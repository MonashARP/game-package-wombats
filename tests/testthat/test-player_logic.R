test_that("player_action handles stand", {
  hand <- new_blackjack_hand(card(rank = c("5", "3"), suit = c("♠", "♦")))
  deck <- card(rank = "8", suit = "♣")
  player <- list(coins = 100, bets = 10)

  res <- player_action(hand, deck, "stand", player)
  expect_equal(res$hand, hand)
  expect_equal(res$deck, deck)
  expect_equal(res$player, player)
})

test_that("player_action handles hit", {
  hand <- new_blackjack_hand(card(rank = c("5", "3"), suit = c("♠", "♦")))
  deck <- card(rank = "8", suit = "♣")
  player <- list(coins = 100, bets = 10)

  res <- player_action(hand, deck, "hit", player)
  expect_equal(length(res$hand$cards), 3)
  expect_equal(length(res$deck), 0)
})

test_that("player_action handles double with insufficient coins", {
  hand <- new_blackjack_hand(card(rank = c("5", "6"), suit = c("♠", "♦")))
  deck <- card(rank = "8", suit = "♣")
  player <- list(coins = 5, bets = 10)

  res <- player_action(hand, deck, "double", player)
  expect_equal(res$player$bets, 10)  # bet unchanged
  expect_equal(res$player$coins, 5)  # coins unchanged
  expect_equal(length(res$hand$cards), 3)
})
