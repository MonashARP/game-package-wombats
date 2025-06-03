#' @title Set up the game and display initial hands with bets
#' @description Shuffles the deck, deals initial hands, deducts player bets, and shows hands.
#' @param players A named list of player objects with 'name' and 'coins' fields.
#' @return A list: deck, player_hands, dealer_hand, and updated players (with bets deducted).
#' @export

setup_and_display_initial <- function(players) {
  num_players <- length(players)

  # Prepare deck
  ranks <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)
  suits <- rep(c("♠","♥","♦","♣"), each = 13)

  deck_char <- paste0(ranks, suits) # Create character vector of cards
  deck_char <- sample(deck_char, length(deck_char)) # Shuffle the deck
  deck <- vctrs::vec_cast(deck_char, card()) # Convert to card class

  # Deal cards
  deal_result <- deal_cards(deck, num_players)
  player_hands_raw <- deal_result$player_hands
  dealer_hand <- deal_result$dealer_hand
  deck <- deal_result$deck

  player_hands <- vector("list", num_players)
  names(player_hands) <- names(players)

  for (i in seq_along(player_hands)) {
    player_hands[[names(players)[i]]] <- list(player_hands_raw[[i]])  # Wrap in list
  }

    for (player_name in names(players)) {
      hand_card <- player_hands_raw[[ player_name ]]
      # Get card values and suits
      ranks <- vctrs::field(hand_card, "rank")  # c("A","10")
      suits <- vctrs::field(hand_card, "suit")  # c("♠","♦")
      display_cards <- paste0(suits, ranks)

    # Show full hand for humans, only first card for computers
    if (isTRUE(players[[player_name]]$is_computer)) {
      cat(player_name, "'s hand: ", display_cards[1], " | Bet: ", players[[player_name]]$bets, "\n", sep = "")
    } else {
      cat(player_name, "'s hand: ", paste(display_cards, collapse = " "), " | Bet: ", players[[player_name]]$bets, "\n", sep = "")
    }
    }

  # Show dealer's hand
  dealer_rank <- vctrs::field(dealer_hand, "rank")
  dealer_suit <- vctrs::field(dealer_hand, "suit")
  dealer_display <- paste0(dealer_suit, dealer_rank)
  cat("Dealer shows: ", paste0(dealer_display, collapse = " "), " ?\n", sep = "")

  return(list(
    deck = deck,
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    players = players
  ))
}

