#' Extract the rank of a card
#'
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
