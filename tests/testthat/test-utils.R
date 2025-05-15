sample_hand <- function() {
  new_blackjack_hand(c("K", "7"))
}

sample_blackjack <- function() {
  new_blackjack_hand(c("A", "10"))
}

sample_deck <- function() {
  sample(rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4))
}

sample_players <- function() {
  list(
    "Alice" = list(type = "human", coins = 1000, bets = 100),
    "Bob" = list(type = "computer", coins = 800, bets = 50)
  )
}
