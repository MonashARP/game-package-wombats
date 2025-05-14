#' @title Player action: hit, stand, or double
#' @param hand A `blackjack_hand` object
#' @param deck A character vector of remaining cards
#' @param action A string: "hit", "stand", or "double"
#' @return A list with updated `hand` and `deck`
#' @export
player_action <- function(hand, deck, action) {
  if (!inherits(hand, "blackjack_hand")) {
    stop("Input 'hand' must be of class 'blackjack_hand'")
  }

  if (!action %in% c("hit", "stand", "double")) {
    stop("Invalid action: must be 'hit', 'stand', or 'double'")
  }

  # "stand" doesn't modify anything
  if (action == "stand") {
    return(list(hand = hand, deck = deck))
  }

  # "hit" or "double" → draw one card
  if (length(deck) == 0) stop("Deck is empty!")

  new_card <- deck[1]
  updated_hand <- new_blackjack_hand(c(vctrs::field(hand, "cards"), new_card))
  updated_deck <- deck[-1]

  return(list(hand = updated_hand, deck = updated_deck))
}
