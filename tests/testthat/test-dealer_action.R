test_that("dealer_action doesn't hit if score already >= 17", {
  hand <- new_blackjack_hand(card(rank = c("10", "7"), suit = c("♠", "♦")))
  deck <- card(rank = c("2", "3", "4"), suit = c("♣", "♥", "♠"))

  result <- dealer_action(hand, deck)
  expect_equal(length(vctrs::field(result$hand, "cards")), 2)
  expect_equal(length(result$deck), 3)
})

test_that("dealer_action errors when input is not a blackjack_hand", {
  expect_error(dealer_action("not a hand", deck = character()), "must be of class 'blackjack_hand'")
})

test_that("dealer_action errors if deck runs out before reaching 17", {
  starting_hand <- new_blackjack_hand(card(rank = c("2", "2"), suit = c("♠", "♦")))
  short_deck <- card(rank = c("2"), suit = c("♣"))

  expect_error(dealer_action(starting_hand, short_deck), "Deck is empty")
})
