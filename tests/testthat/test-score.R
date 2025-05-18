test_that("blackjack_hand is created correctly", {
  hand <- new_blackjack_hand(c("A", "10"))
  expect_s3_class(hand, "blackjack_hand")
  expect_equal(vctrs::field(hand, "cards"), c("A", "10"))
})

test_that("format method outputs correct string", {
  hand <- new_blackjack_hand(c("K", "3"))
  expect_equal(format(hand), "Hand: [K, 3]")
})

test_that("calculate_score works without Aces", {
  hand <- new_blackjack_hand(c("10", "9"))
  expect_equal(calculate_score(hand), 19)
})

test_that("calculate_score handles one Ace", {
  hand <- new_blackjack_hand(c("A", "9"))
  expect_equal(calculate_score(hand), 20)
})

test_that("calculate_score adjusts Ace to avoid bust", {
  hand <- new_blackjack_hand(c("A", "9", "3"))
  expect_equal(calculate_score(hand), 13)  # A counts as 1
})

test_that("calculate_score handles multiple Aces", {
  hand <- new_blackjack_hand(c("A", "A", "8"))
  expect_equal(calculate_score(hand), 20)  # A(11) + A(1) + 8
})

test_that("calculate_score returns >21 when busting", {
  hand <- new_blackjack_hand(c("A", "A", "10", "K"))
  expect_equal(calculate_score(hand), 22)  # A(1) + A(1) + 10 + 10
})
