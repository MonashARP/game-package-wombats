# R/card-utils.R
# for testing

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

#' @param x   A card vector
#' @return    Character vectors, such as "♠","♥","♦","♣"
#' @export
card_suit <- function(x) {
  vctrs::field(x, "suit")
}
