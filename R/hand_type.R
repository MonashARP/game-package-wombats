# R/hang_type.R
# blackjack or five-card charlie

#' @title Check 5-card Charlie condition
#' @description Returns TRUE if the hand has 5 or more cards and score ≤ 21
#' @param hand A `blackjack_hand` object
#' @return TRUE if qualifies as 5-card Charlie, else FALSE
#' @export
is_five_card_charlie <- function(hand) {
  if (!inherits(hand, "blackjack_hand")) stop("Input must be of class 'blackjack_hand'")
  cards <- hand$cards
  length(cards) == 5 && calculate_score(hand) <= 21
}

#' @title Check if a hand is Blackjack
#' @description Returns TRUE if the hand has exactly 2 cards and total is 21
#' @param hand A `blackjack_hand` object
#' @return TRUE if Blackjack, else FALSE
#' @export
is_blackjack <- function(hand) {
  cards <- hand$cards
  ranks <- card_rank(cards)
  length(cards) == 2 && any(ranks == "A") && any(ranks %in% c("10", "J", "Q", "K"))
}
