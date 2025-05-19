#' @title Check 5-card Charlie condition
#' @description Returns TRUE if the hand has 5 or more cards and score ≤ 21
#' @param hand A `blackjack_hand` object
#' @return TRUE if qualifies as 5-card Charlie, else FALSE
#' @export
is_five_card_charlie <- function(hand) {
  cards <- vctrs::field(hand, "cards")
  return(length(cards) >= 5 && calculate_score(hand) <= 21)
}
