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

