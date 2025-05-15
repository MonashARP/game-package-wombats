#' @title Set up the game and display initial hands with bets
#' @description Shuffles the deck, deals initial hands, deducts player bets, and shows hands.
#' @param players A list of player objects with 'name' and 'coins' fields.
#' @return A list: deck, player_hands, dealer_hand, and updated players (with bets deducted).
#' @export

setup_and_display_initial <- function(players) {
  num_players <- length(players)

  # Prepare deck
  deck <- sample(rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4))

  # Ask for bets
  for (i in seq_along(players)) {
    repeat {
      cat("\n", players[[i]]$name, ", you have", players[[i]]$coins, "coins.\n")
      bet <- suppressWarnings(as.numeric(readline("Enter your bet: ")))
      if (!is.na(bet) && bet > 0 && bet <= players[[i]]$coins) {
        players[[i]]$bets <- bet
        players[[i]]$coins <- players[[i]]$coins - bet
        break
      }
      cat("❌ Invalid bet. Try again.\n")
    }
  }

  # Deal cards
  deal_result <- deal_cards(deck, num_players)
  player_hands <- deal_result$player_hands
  dealer_hand <- deal_result$dealer_hand
  deck <- deal_result$deck

  # Display initial hands
  cat("\n=== 🃏 Blackjack Begins ===\n")
  for (i in seq_along(player_hands)) {
    cat(players[[i]]$name, "'s hand:",
        paste(vctrs::field(player_hands[[i]][[1]], "cards"), collapse = ", "),
        "| Bet:", players[[i]]$bets, "\n")
  }
  cat("Dealer shows:", vctrs::field(dealer_hand, "cards")[1], ", ?\n")

  return(list(
    deck = deck,
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    players = players
  ))
}
