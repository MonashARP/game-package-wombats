test_that("dealer stops at 17", {
  hand <- new_blackjack_hand(c("10", "7"))
  deck <- rep("2", 10)
  result <- dealer_action(hand, deck)
  expect_equal(calculate_score(result$hand), 17)
  expect_length(vctrs::field(result$hand, "cards"), 2)
})
