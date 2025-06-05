#  R/score.R

#' @title Calculate Blackjack Score
#' @description Calculates the total point value of a blackjack hand
#' @param hand A `blackjack_hand` object
#' @return Numeric score of the hand (integer)
#' @export
calculate_score <- function(hand) {
  if (!inherits(hand, "blackjack_hand")) {
    stop("Input must be of class 'blackjack_hand'")
  }

  cards <- vctrs::field(hand, "cards")
  ranks <- card_rank(cards)

  # Map ranks to numeric values explicitly
  values <- sapply(ranks, function(rank) {
    if (rank %in% c("J", "Q", "K")) {
      10
    } else if (rank == "A") {
      11
    } else {
      as.numeric(rank)
    }
  })

  total <- sum(values)
  ace_count <- sum(ranks == "A")

  while (total > 21 && ace_count > 0) {
    total <- total - 10
    ace_count <- ace_count - 1
  }

  return(total)
}
