test_that("needs_insurance correctly detects Ace upcard", {
  hand1 <- new_blackjack_hand(card(rank = c("A", "K"), suit = c("♠", "♦")))
  hand2 <- new_blackjack_hand(card(rank = c("10", "A"), suit = c("♣", "♥")))

  expect_true(needs_insurance(hand1))
  expect_false(needs_insurance(hand2))
})

test_that("ai_decide_insurance returns logical and respects coin balance", {
  expect_false(ai_decide_insurance(coins = 5, insurance_bet = 10)) # Not enough coins
  set.seed(1)
  result <- replicate(100, ai_decide_insurance(coins = 100, insurance_bet = 10, prob_buy = 0.8))
  expect_true(mean(result) > 0.7)  # Should buy most of the time
})

test_that("resolve_insurance works as expected", {
  # Player wins insurance
  res1 <- resolve_insurance(coins = 100, insurance_bet = 10, dealer_has_blackjack = TRUE)
  expect_equal(res1$coins, 100 + 10)  # -10 + 20
  expect_equal(res1$payout, 20)

  # Player loses insurance
  res2 <- resolve_insurance(coins = 100, insurance_bet = 10, dealer_has_blackjack = FALSE)
  expect_equal(res2$coins, 90)
  expect_equal(res2$payout, 0)
})

test_that("handle_insurance updates player state correctly for computer", {
  dealer_hand <- new_blackjack_hand(card(rank = c("A", "K"), suit = c("♠", "♦")))
  players <- list(
    Alice = list(is_computer = TRUE, coins = 100, bets = 20)
  )

  set.seed(42)
  res <- handle_insurance(dealer_hand, players, ai_prob = 1.0)  # Force insurance buy

  expect_true(res$dealer_blackjack)
  expect_equal(res$players$Alice$insurance_bet, 10)
  expect_equal(res$players$Alice$coins, 110)  # 100 - 10 + 20
})
