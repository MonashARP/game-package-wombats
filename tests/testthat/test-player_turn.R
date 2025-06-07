test_that("computer player hits and updates hand", {
  deck <- c("2♣", "3♦", "4♠", "5♥", "6♣", "7♦", "8♠", "9♥", "10♣", "J♦", "Q♠", "K♥", "A♣")
  player_hand <- list(new_blackjack_hand(c("2♠", "3♥")))  # score = 5
  players <- list(Bot = list(coins = 100, bets = 10, is_computer = TRUE))
  hands <- list(Bot = player_hand)

  result <- play_player_turns(hands, deck, players)

  expect_true(length(result$player_hands$Bot[[1]]$cards) > 2)
  expect_equal(result$players$Bot$coins, 100)  # no coins spent except original bet
})

test_that("computer player doubles down with valid score and coins", {
  hand <- new_blackjack_hand(c("5♥", "6♦"))  # score = 11
  deck <- c("10♠", "J♣", "Q♥")
  players <- list(Bot = list(coins = 100, bets = 10, is_computer = TRUE))
  hands <- list(Bot = list(hand))

  result <- play_player_turns(hands, deck, players)

  updated_hand <- result$player_hands$Bot[[1]]
  updated_cards <- updated_hand$cards

  expect_equal(length(updated_cards), 3)  # double down = one card added
  expect_equal(result$players$Bot$coins, 90)  # 10 coins spent on double
  expect_equal(result$players$Bot$bets, 20)  # doubled bet
})

