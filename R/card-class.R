#' Create a card vector
#'
#' Constructs a custom card object using vctrs::new_rcrd().
#'
#' @param rank Character vector, allowed values: "A","2",...,"10","J","Q","K"
#' @param suit Character vector, allowed values: "♠","♥","♦","♣"
#' @return A vctrs record vector with class "card"
#' @export
card <- function(rank = character(), suit = character()) {
  stopifnot(
    is.character(rank), is.character(suit),
    length(rank) == length(suit),
    all(rank %in% c("A","2","3","4","5","6","7","8","9","10","J","Q","K")),
    all(suit %in% c("♠","♥","♦","♣"))
  )
  vctrs::new_rcrd(
    list(rank = rank, suit = suit),
    class = "card"
  )
}

#' Format a card vector
#'
#' Returns a character representation like "A♠", "10♦".
#'
#' @param x A card vector
#' @return A character vector
#' @export
format.card <- function(x, ...) {
  ranks <- vctrs::field(x, "rank")
  suits <- vctrs::field(x, "suit")
  paste0(ranks, suits)
}

#' Cast character vector to card
#'
#' Converts character like "A♠", "10♦" into a card object.
#'
#' @param x A character vector
#' @param to A card prototype
#' @return A card vector
#' @exportS3Method vctrs::vec_cast card.character
vec_cast.card.character <- function(x, to, ...) {
  message("vec_cast.card.character called!")
  ranks <- sub("^(.+?)([♠♥♦♣])$", "\\1", x)
  suits <- sub("^(.+?)([♠♥♦♣])$", "\\2", x)
  card(ranks, suits)
}

#' Cast card to character vector
#'
#' Converts a card object back to character like "Q♥".
#'
#' @param x A card object
#' @param to Unused
#' @return Character vector
#' @exportS3Method vctrs::vec_cast character.card
vec_cast.character.card <- function(x, to, ...) {
  paste0(vctrs::field(x, "rank"), vctrs::field(x, "suit"))
}

#' Type promotion: card + character → card
#'
#' @exportS3Method vctrs::vec_ptype2 card.character
vec_ptype2.card.character <- function(x, y, ...) card()

#' Type promotion: character + card → card
#'
#' @exportS3Method vctrs::vec_ptype2 character.card
vec_ptype2.character.card <- function(x, y, ...) card()

#' Type promotion: card + card → card prototype
#'
#' @exportS3Method vctrs::vec_ptype2 card.card
vec_ptype2.card.card <- function(x, y, ...) {
  if (inherits(y, "card")) {
    vctrs::new_rcrd(
      list(rank = character(), suit = character()),
      class = "card"
    )
  } else {
    vctrs::stop_incompatible_type(x, y)
  }
}
