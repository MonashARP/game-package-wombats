# R/dealer_action.R
# Dealer action logic

#' @title Dealer action logic
#' @description Simulate dealer's automatic behavior: keep hitting until score >= 17.
#' @param hand A `blackjack_hand` object
#' @param deck A character vector of remaining cards
#' @return A list with updated `hand` and `deck`
#' @export
dealer_action <- function(hand, deck) {
  if (!inherits(hand, "blackjack_hand")) {
    stop("Input 'hand' must be of class 'blackjack_hand'")
  }

  current_score <- calculate_score(hand)
  current_hand <- hand
  current_deck <- deck

  # Metaprogramming loop condition: score < 17
  while (rlang::eval_tidy(rlang::expr(!!current_score < 17))) {
    if (length(current_deck) == 0) stop("Deck is empty!")
    new_card <- current_deck[1]
    updated_cards <- c(vctrs::field(current_hand, "cards"), new_card)
    current_hand <- new_blackjack_hand(updated_cards)
    current_deck <- current_deck[-1]
    current_score <- calculate_score(current_hand)
  }

  return(list(hand = current_hand, deck = current_deck))
}
