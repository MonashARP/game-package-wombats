# R/card_utils.R
# Provide tool functions for extracting and converting the attributes of playing cards,
# such as obtaining suits, points, and face card judgments, etc.

#' judging J/Q/K
#'
#' @param x  A card vector
#' @return   Logical vector, TRUE indicates that the corresponding rank is at c("J","Q","K").
#' @export
card_is_face <- function(x) {
  ranks <- vctrs::field(x, "rank")
  ranks %in% c("J","Q","K")
}

#' judging A (Ace)
#'
#' @param x  A card vector
#' @return   Logical vector, TRUE indicates that rank == "A"
#' @export
card_is_ace <- function(x) {
  ranks <- vctrs::field(x, "rank")
  ranks == "A"
}

#' Obtain the initial points of a single card in Blackjack
#' (ensure J/Q/K == 10，A == 11)
#'
#' @param x  A card vector
#' @return   Integer vector
#' @export
card_value <- function(x) {
  ranks <- vctrs::field(x, "rank")
  vapply(ranks, function(r) {
    if (r %in% c("J","Q","K")) return(10L)
    if (r == "A") return(11L)
    as.integer(r)
  }, integer(1))
}

#' Extract the suit from a card vector
#'
#' @param x   A card vector
#' @return    Character vectors, such as "♠","♥","♦","♣"
#' @export
#' @examples
#' cards <- as_card(c("A♠", "10♦", "Q♥"))
#' card_suit(cards)
card_suit <- function(x) {
  vctrs::field(x, "suit")
}

#' Extract the rank of a card
#' Generic function to extract the rank (e.g., "A", "10", "J") from an object representing a card.
#' This function supports objects of class \code{card} or character strings representing cards.
#'
#' @param x An object representing a card (e.g., a \code{card} object or character vector like "A♠").
#' @return A character vector of ranks.
#' @export
card_rank <- function(x) {
  UseMethod("card_rank")
}

#' Default method for card_rank
#'
#' Returns \code{NULL} for unsupported classes.
#'
#' @param x Object of any class.
#' @return NULL
#' @export
card_rank.default <- function(x) {
  warning("card_rank() not implemented for this class; returning NULL.")
  NULL
}

#' card_rank method for card class
#'
#' Extracts the rank field from a \code{card} object.
#'
#' @param x A \code{card} object.
#' @return Character vector of ranks.
#' @export
card_rank.card <- function(x) {
  vctrs::field(x, "rank")
}

#' card_rank method for character vectors
#'
#' Extracts the rank from character strings like "A♠", "10♦".
#'
#' @param x Character vector of card representations.
#' @return Character vector of ranks.
#' @export
card_rank.character <- function(x) {
  pattern <- "^(A|10|[2-9JQK])"
  ranks <- stringr::str_extract(x, pattern)
  if (any(is.na(ranks))) {
    stop("Invalid card string format in input.")
  }
  ranks
}

#' Return a string for display, e.g., "10♠", "A♥".
#' @noRd
format_hand_display <- function(cards) {paste(cards, collapse = " ")}

#' Blackjack point mapping table
#' @noRd
blackjack_values <- c(
  "2"  = 2, "3"  = 3, "4"  = 4, "5"  = 5, "6"  = 6,
  "7"  = 7, "8"  = 8, "9"  = 9, "10" = 10, "J" = 10,
  "Q"  = 10, "K"  = 10, "A"  = 11
)

#' card_rank method for blackjack_hand class
#'
#' Extracts ranks from the cards contained in a blackjack_hand.
#'
#' @param x A blackjack_hand object.
#' @return Character vector of ranks for each card in the hand.
#' @export
card_rank.blackjack_hand <- function(x) {
  cards <- vctrs::field(x, "cards")
  card_rank(cards)
}

#' Wrap a vector of card strings as a blackjack_hand S3 object, if not already.
#' @noRd
ensure_blackjack_hand <- function(hand) {
  if (!inherits(hand, "blackjack_hand")) new_blackjack_hand(hand) else hand
}
