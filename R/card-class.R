#' Create a card vector
#'
#' @importFrom vctrs vec_cast vec_ptype2 new_rcrd
#' @importFrom stringr str_match
#' @param rank   Character vector, allowed values:："A","2",...,"10","J","Q","K"
#' @param suit   Character vector, allowed values："♠","♥","♦","♣"
#' @return       A vctrs record vector with the class "card"
#' @export
card <- function(suit, rank) {
  stopifnot(
    is.character(rank), is.character(suit),
    length(rank) == length(suit),
    all(rank %in% c("A","2","3","4","5","6","7","8","9","10","J","Q","K")),
    all(suit %in% c("♠","♥","♦","♣"))
  )
  vctrs::new_rcrd(
    list(suit = suit, rank = rank),
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
  paste0(suits, ranks)
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

# Support from the character vector (in the form "♠A", "♦10") -> card
#' @method vec_cast card.character
#' @export
vec_cast.card.character <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (!inherits(to, "card")) vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  if (inherits(x, "card")) return(x)
  if (!is.character(x)) vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)

  # Use stringr for proper UTF-8 matching
  pattern <- "^([♠♥♦♣])(A|10|[2-9JQK])$"
  matches <- stringr::str_match(x, pattern)

  if (any(is.na(matches[, 1]))) {
    bad <- x[is.na(matches[, 1])]
    vctrs::stop_incompatible_cast(
      x, to,
      details = paste0("Can't convert: ", paste(bad, collapse = ", ")),
      x_arg = x_arg, to_arg = to_arg
    )
  }

  suits <- matches[, 2]
  ranks <- matches[, 3]

  card(suits, ranks)
}

# Print method — user-facing display
#' @export
print.card <- function(x, ...) {
  cat(paste0(format(x), collapse = " "), "\n")
  invisible(x)
}

#' @method vec_cast card
#' @export
vec_cast.card <- function(x, to, ...) {
  if (inherits(x, "card")) return(x)
  vctrs::stop_incompatible_cast(x, to)
}

#' @export
as.character.card <- function(x, ...) {
  format(x, ...)
}
