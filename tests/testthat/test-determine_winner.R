test_that("Player wins with higher score", {
  player_hand <- new_blackjack_hand(c("10", "8"))  # 18
  dealer_hand <- new_blackjack_hand(c("9", "7"))   # 16
  expect_equal(determine_winner(player_hand, dealer_hand), "Player wins")
})


test_that("Dealer wins with higher score", {
  player_hand <- new_blackjack_hand(c("9", "7"))   # 16
  dealer_hand <- new_blackjack_hand(c("10", "8"))  # 18
  expect_equal(determine_winner(player_hand, dealer_hand), "Dealer wins")
})


test_that("Push when scores are equal", {
  player_hand <- new_blackjack_hand(c("10", "7"))
  dealer_hand <- new_blackjack_hand(c("9", "8"))
  expect_equal(determine_winner(player_hand, dealer_hand), "Push")
})

test_that("Player wins with Blackjack", {
  player_hand <- new_blackjack_hand(c("A", "K"))
  dealer_hand <- new_blackjack_hand(c("10", "8"))
  expect_equal(determine_winner(player_hand, dealer_hand), "Player wins with Blackjack")
})

test_that("Dealer wins with Blackjack", {
  player_hand <- new_blackjack_hand(c("9", "9"))
  dealer_hand <- new_blackjack_hand(c("A", "K"))
  expect_equal(determine_winner(player_hand, dealer_hand), "Dealer wins with Blackjack")
})

test_that("Push when both get Blackjack", {
  player_hand <- new_blackjack_hand(c("A", "10"))
  dealer_hand <- new_blackjack_hand(c("A", "K"))
  expect_equal(determine_winner(player_hand, dealer_hand), "Push (both Blackjack)")
})

test_that("Player busts", {
  player_hand <- new_blackjack_hand(c("10", "8", "6"))  # 24
  dealer_hand <- new_blackjack_hand(c("9", "8"))        # 17
  expect_equal(determine_winner(player_hand, dealer_hand), "Dealer wins")
})

test_that("Dealer busts", {
  player_hand <- new_blackjack_hand(c("10", "7"))       # 17
  dealer_hand <- new_blackjack_hand(c("10", "8", "5"))  # 23
  expect_equal(determine_winner(player_hand, dealer_hand), "Player wins")
})

test_that("Both bust", {
  player_hand <- new_blackjack_hand(c("10", "9", "5"))  # 24
  dealer_hand <- new_blackjack_hand(c("10", "8", "6"))  # 24
  expect_equal(determine_winner(player_hand, dealer_hand), "Both bust - Dealer wins by rule")
})

test_that("Player wins with 5-card Charlie", {
  player_hand <- new_blackjack_hand(c("2", "3", "4", "5", "6"))  # 20
  dealer_hand <- new_blackjack_hand(c("10", "8"))               # 18
  expect_equal(determine_winner(player_hand, dealer_hand), "Player wins with 5-card Charlie")
})

test_that("Multiple hands return correct outcomes", {
  hand1 <- new_blackjack_hand(c("A", "10"))     # Blackjack
  hand2 <- new_blackjack_hand(c("9", "8", "6")) # Bust
  dealer_hand <- new_blackjack_hand(c("10", "9"))

  result <- determine_winner(list(hand1, hand2), dealer_hand)

  expect_named(result, c("Hand 1", "Hand 2"))
  expect_equal(result[["Hand 1"]], "Player wins with Blackjack")
  expect_equal(result[["Hand 2"]], "Dealer wins")
})
