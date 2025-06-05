test_that("needs_split detects pairs correctly", {
  hand1 <- card(rank = c("8", "8"), suit = c("♠", "♥"))
  hand2 <- card(rank = c("10", "J"), suit = c("♣", "♦"))
  hand3 <- card(rank = c("A", "A"), suit = c("♠", "♦"))
  hand4 <- card(rank = c("5", "5", "5"), suit = c("♠", "♦", "♣"))

  expect_true(needs_split(hand1))
  expect_false(needs_split(hand2))
  expect_true(needs_split(hand3))
  expect_false(needs_split(hand4))
})

test_that("ai_decide_split returns correct decision", {
  hand1 <- card(rank = c("A", "A"), suit = c("♠", "♣"))
  hand2 <- card(rank = c("8", "8"), suit = c("♦", "♥"))
  hand3 <- card(rank = c("10", "10"), suit = c("♣", "♠"))

  expect_true(ai_decide_split(hand1))
  expect_true(ai_decide_split(hand2))
  expect_false(ai_decide_split(hand3))
})

test_that("perform_split creates two new hands with additional cards", {
  hand <- card(rank = c("A", "A"), suit = c("♠", "♣"))
  deck <- card(rank = c("9", "10", "J"), suit = c("♦", "♥", "♠"))

  result <- perform_split(hand, deck, deck_index = 1)

  expect_length(result$split_hands, 2)
  expect_s3_class(result$split_hands[[1]], "blackjack_hand")
  expect_s3_class(result$split_hands[[2]], "blackjack_hand")
  expect_equal(result$new_deck_index, 3)

  # Check that each hand has 2 cards
  expect_equal(length(result$split_hands[[1]]$cards), 2)
  expect_equal(length(result$split_hands[[2]]$cards), 2)
})
