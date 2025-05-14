#' @title Deal initial cards in Blackjack
#' @description Deal two cards to the player and dealer each, from a shuffled deck.
#' @param deck A character vector representing the deck. If NULL, a fresh 52-card deck is used.
#' @return A list with two `blackjack_hand` objects: `player_hand` and `dealer_hand`.
#' @export
deal_cards <- function(deck = NULL) {
  # Create a standard 52-card deck if none supplied
  if (is.null(deck)) {
    ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
    deck <- rep(ranks, times = 4)
  }

  # Metaprogramming: ensure cards are valid before sampling
  card_check_expr <- rlang::expr(all(!!deck %in% c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")))
  if (!rlang::eval_tidy(card_check_expr)) {
    stop("Invalid card values in deck.")
  }

  # Sample 4 cards without replacement
  drawn <- sample(deck, size = 4, replace = FALSE)

  # Return hands as S3 blackjack_hand objects
  list(
    player_hand = new_blackjack_hand(drawn[1:2]),
    dealer_hand = new_blackjack_hand(drawn[3:4])
  )
}
