# R/score_cal.R

#' @useDynLib wombat21
#' @importFrom Rcpp sourceCpp
NULL

#' @title Calculate Blackjack Score
#' @description Calculates the total point value of a blackjack hand using a fast C++ backend.
#' @param hand A \code{blackjack_hand} object.
#' @return Integer. Total blackjack score for the hand.
#' @examples
#' hand <- new_blackjack_hand(c("A♠", "10♦"))
#' calculate_score(hand)   # 21
#' hand2 <- new_blackjack_hand(c("7♣", "8♦", "6♥"))
#' calculate_score(hand2)  # 21
#' @export
calculate_score <- function(hand) {
  if (!inherits(hand, "blackjack_hand")) {
    stop("Input must be of class 'blackjack_hand'")
  }

  cards <- vctrs::field(hand, "cards")
  ranks <- card_rank(cards)

  # Use C++ implementation
  return(calculate_score_cpp(ranks))
}


