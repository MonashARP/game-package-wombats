test_that("deal_cards works with default arguments", {
  res <- deal_cards()

  expect_type(res, "list")
  expect_length(res$player_hands, 1)
  expect_s3_class(res$player_hands[[1]], "blackjack_hand")
  expect_s3_class(res$dealer_hand, "blackjack_hand")
  expect_equal(length(vctrs::field(res$player_hands[[1]], "cards")), 2)
  expect_equal(length(vctrs::field(res$dealer_hand, "cards")), 2)
  expect_equal(length(res$deck), 52 - 4)  # 4 cards dealt
})
