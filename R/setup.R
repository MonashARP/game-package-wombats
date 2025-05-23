#' @title Set up the game and display initial hands with bets
#' @description Shuffles the deck, deals initial hands, deducts player bets, and shows hands.
#' @param players A named list of player objects with 'name' and 'coins' fields.
#' @return A list: deck, player_hands, dealer_hand, and updated players (with bets deducted).
#' @export

setup_and_display_initial <- function(players) {
  num_players <- length(players)

  # Prepare deck
  deck <- sample(rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4))

  # Deal cards
  deal_result <- deal_cards(deck, num_players)
  player_hands_raw <- deal_result$player_hands
  dealer_hand <- deal_result$dealer_hand
  deck <- deal_result$deck

  player_hands <- list()
  player_names <- names(players)

  for (i in seq_along(player_names)) {
    player_hands[[player_names[i]]] <- list(player_hands_raw[[i]])  # Wrap in list

    # Get card values and suits
    cards <- vctrs::field(player_hands_raw[[i]], "cards")
    suits <- vctrs::field(player_hands_raw[[i]], "suits")
    display_cards <- paste0(suits, cards)

    # Show full hand for humans, only first card for computers
    if (players[[player_names[i]]]$is_computer) {
      cat(player_names[i], "'s hand: ", display_cards[1], " | Bet: ", players[[player_names[i]]]$bets, "\n", sep = "")
    } else {
      cat(player_names[i], "'s hand: ", paste(display_cards, collapse = " "), " | Bet: ", players[[player_names[i]]]$bets, "\n", sep = "")
    }
  }

  cat("Dealer shows: ", paste0(dealer_hand$suits[1], dealer_hand$cards[1]), " ?\n")

  return(list(
    deck = deck,
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    players = players
  ))
}
