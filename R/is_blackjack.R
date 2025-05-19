#' @title Check if a hand is Blackjack
#' @description Returns TRUE if the hand has exactly 2 cards and total is 21
#' @param hand A `blackjack_hand` object
#' @return TRUE if Blackjack, else FALSE
#' @export
is_blackjack <- function(hand) {
  cards <- vctrs::field(hand, "cards")
  return(length(cards) == 2 && calculate_score(hand) == 21)
}
