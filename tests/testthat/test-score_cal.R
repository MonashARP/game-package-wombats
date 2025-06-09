test_that("calculate_score returns correct totals", {
  # Hand with no Aces
  hand1 <- new_blackjack_hand(card(rank = c("10", "7"), suit = c("♠", "♦")))
  expect_equal(calculate_score(hand1), 17)

  # Hand with a face card
  hand2 <- new_blackjack_hand(card(rank = c("K", "9"), suit = c("♣", "♠")))
  expect_equal(calculate_score(hand2), 19)

  # Hand with one Ace that doesn’t need adjustment
  hand3 <- new_blackjack_hand(card(rank = c("A", "7"), suit = c("♣", "♥")))
  expect_equal(calculate_score(hand3), 18)

  # Hand with one Ace that needs adjustment (11 → 1)
  hand4 <- new_blackjack_hand(card(rank = c("A", "10", "5"), suit = c("♣", "♦", "♥")))
  expect_equal(calculate_score(hand4), 16)

  # Hand with multiple Aces
  hand5 <- new_blackjack_hand(card(rank = c("A", "A", "9"), suit = c("♠", "♦", "♣")))
  expect_equal(calculate_score(hand5), 21)

  # Hand with multiple Aces needing adjustments
  hand6 <- new_blackjack_hand(card(rank = c("A", "A", "9", "3"), suit = c("♠", "♦", "♣", "♥")))
  expect_equal(calculate_score(hand6), 14)
})

test_that("calculate_score errors for non-blackjack_hand", {
  expect_error(calculate_score(card(rank = c("A", "10"), suit = c("♠", "♣"))),
               "Input must be of class 'blackjack_hand'")
})
