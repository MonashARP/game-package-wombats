test_that("calculate_score computes hand totals correctly", {
  hand1 <- new_blackjack_hand(c("A", "10"))
  expect_equal(calculate_score(hand1), 21)

  hand2 <- new_blackjack_hand(c("A", "9", "A"))
  expect_equal(calculate_score(hand2), 21)

  hand3 <- new_blackjack_hand(c("10", "9", "5"))
  expect_equal(calculate_score(hand3), 24)
})

test_that("calculate_score handles Aces correctly", {
  expect_equal(calculate_score(new_blackjack_hand(c("A", "10"))), 21)
  expect_equal(calculate_score(new_blackjack_hand(c("A", "9"))), 20)
  expect_equal(calculate_score(new_blackjack_hand(c("A", "9", "A"))), 21)
  expect_equal(calculate_score(new_blackjack_hand(c("A", "A", "A"))), 13)
})

test_that("calculate_score handles busts correctly", {
  expect_equal(calculate_score(new_blackjack_hand(c("K", "10", "5"))), 25)
})

test_that("format.blackjack_hand works", {
  hand <- new_blackjack_hand(c("A", "5", "Q"))
  expect_match(format(hand), "Hand: \\[A, 5, Q\\]")
})
