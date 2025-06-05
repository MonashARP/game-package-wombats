#' @title Calculate Blackjack Score
#' @description Calculates the total point value of a blackjack hand using C++
#' @param hand A `blackjack_hand` object
#' @return Numeric score of the hand (integer)
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

#' @useDynLib wombat21
#' @importFrom Rcpp sourceCpp
NULL
