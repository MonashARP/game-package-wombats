#' @title Create a blackjack hand (S3 vctrs)
#' @description Create a hand object representing a set of cards
#' @param cards Character vector of card values, e.g. c("A", "10", "5")
#' @return A `blackjack_hand` S3 object
#' @export

new_blackjack_hand <- function(cards) {
  if (!inherits(cards, "card")) {
    stop("`cards` must be a `card` vector (rank + suit).")
  }
  structure(
    list(cards = cards),
    class = "blackjack_hand"
  )
}


#' @title Format a Blackjack Hand
#' @description Custom print method for `blackjack_hand` objects.
#' @param x A `blackjack_hand` object
#' @param ... Additional arguments (ignored)
#' @return A character string representing the hand
#' @export

format.blackjack_hand <- function(x, ...) {
  paste0("Hand: [", paste0(vctrs::field(x, "cards"), collapse = ", "), "]")
}

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

  values <- values <- blackjack_values[cards]

  total <- sum(values)
  ace_count <- sum(cards == "A")

  while (total > 21 && ace_count > 0) {
    total <- total - 10
    ace_count <- ace_count - 1
  }

  return(total)
}
