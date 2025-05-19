test_that("Player wins and gets correct payout", {
  player_hand <- list(Alice = list(new_blackjack_hand(c("10", "9"))))  # Score: 19
  dealer_hand <- new_blackjack_hand(c("10", "7"))                       # Score: 17

  players <- list(
    Alice = list(coins = 100, bets = 10)
  )

  updated_players <- display_final_results(player_hand, dealer_hand, players)
  expect_equal(updated_players$Alice$coins, 110)
})


test_that("Player wins with Blackjack gets 1.5x payout", {
  player_hand <- list(Bob = list(new_blackjack_hand(c("A", "K"))))
  dealer_hand <- new_blackjack_hand(c("10", "9"))

  players <- list(
    Bob = list(coins = 100, bets = 20)
  )

  updated_players <- display_final_results(player_hand, dealer_hand, players)
  expect_equal(updated_players$Bob$coins, 130)  # 100 + 30 (1.5x payout)
})

test_that("Player loses and loses full bet", {
  player_hand <- list(Chloe = list(new_blackjack_hand(c("10", "5"))))  # 15
  dealer_hand <- new_blackjack_hand(c("10", "8"))                      # 18

  players <- list(
    Chloe = list(coins = 100, bets = 10)
  )

  updated_players <- display_final_results(player_hand, dealer_hand, players)
  expect_equal(updated_players$Chloe$coins, 90)
})

test_that("Push results in no change", {
  player_hand <- list(Dan = list(new_blackjack_hand(c("10", "8"))))
  dealer_hand <- new_blackjack_hand(c("9", "9"))  # Also 18

  players <- list(
    Dan = list(coins = 100, bets = 10)
  )

  updated_players <- display_final_results(player_hand, dealer_hand, players)
  expect_equal(updated_players$Dan$coins, 100)
})

test_that("Player wins with 5-card Charlie", {
  player_hand <- list(Eve = list(new_blackjack_hand(c("2", "3", "4", "5", "6"))))  # 20
  dealer_hand <- new_blackjack_hand(c("10", "9"))  # 19

  players <- list(
    Eve = list(coins = 50, bets = 10)
  )

  updated_players <- display_final_results(player_hand, dealer_hand, players)
  expect_equal(updated_players$Eve$coins, 60)
})

test_that("Split hands return correct separate results", {
  player_hand <- list(
    Alice = list(
      new_blackjack_hand(c("A", "K")),     # Blackjack
      new_blackjack_hand(c("10", "9"))     # Wins normally
    )
  )
  dealer_hand <- new_blackjack_hand(c("10", "8"))

  players <- list(
    Alice = list(coins = 100, bets = 10)
  )

  updated_players <- display_final_results(player_hand, dealer_hand, players)

  # 1st hand: 1.5x bet = 15, 2nd hand: 1x bet = 10, total = 25
  expect_equal(updated_players$Alice$coins, 125)
})
