# R/hand_utils.R

#' @title Create a blackjack hand (S3 vctrs)
#' @description Create a hand object representing a set of cards
#' @param cards Character vector of card values, e.g. c("A", "10", "5")
#' @return A `blackjack_hand` S3 object
#' @export

new_blackjack_hand <- function(cards) {
  # Convert character to card if needed
  if (is.character(cards)) {
    cards <- vctrs::vec_cast(cards, card())
  }

  if (!inherits(cards, "card")) {
    stop("cards must be a card vector (rank + suit).")
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














