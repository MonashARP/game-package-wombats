#' @title Deal initial cards in Blackjack
#' @description Deal two cards to the player and dealer each, from a shuffled deck.
#' @param deck A character vector representing the deck. If NULL, a fresh 52-card deck is used.
#' @param num_players An integer (default = 1). Number of players to deal cards to.
#' @return A list with `player_hands` (a list of blackjack_hand objects) and `dealer_hand`.
#' @export
deal_cards <- function(deck = NULL, num_players = 1) {
  # Create a standard 52-card deck if none supplied
  if (is.null(deck)) {
    ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
    suits <- c("♠", "♥", "♦", "♣")
    deck <- as.vector(outer(ranks, suits, paste0))
    deck <- sample(deck, length(deck))
  }

  if (!all(grepl("^(A|10|[2-9]|J|Q|K)[♠♥♦♣]$", deck))) {
    stop("Invalid card values in deck.")
  }

  # Ensure enough cards to deal
  total_needed <- 2 * (num_players + 1)
  if (length(deck) < total_needed) stop("Not enough cards to deal.")

  # Sample required number of cards
  drawn <- deck[1:total_needed]
  deck <- deck[-(1:total_needed)]

  # Create hands
  player_hands <- vector("list", num_players)
  for (i in seq_len(num_players)) {
    player_hands[[i]] <- new_blackjack_hand(drawn[(2*i-1):(2*i)])
  }
  dealer_hand <- new_blackjack_hand(drawn[(2*num_players+1):(2*num_players+2)])

  return(list(
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    deck = deck
  ))
}
