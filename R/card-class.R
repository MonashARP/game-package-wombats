# R/card-class.R

#' Create a card vector
#'
#' @importFrom vctrs vec_cast vec_ptype2 new_rcrd
#' @importFrom stringr str_match
#' @param rank   Character vector, allowed values:："A","2",...,"10","J","Q","K"
#' @param suit   Character vector, allowed values："♠","♥","♦","♣"
#' @return       A vctrs record vector with the class "card"
#' @examples
#' cards <- card(rank = c("A", "10", "Q"), suit = c("♠", "♦", "♥"))
#' print(cards)
#' @export
card <- function(rank = character(), suit = character()) {
  stopifnot(
    is.character(rank), is.character(suit),
    length(rank) == length(suit),
    all(rank %in% c("A","2","3","4","5","6","7","8","9","10","J","Q","K")),
    all(suit %in% c("\u2660", "\u2665", "\u2666", "\u2663"))
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
#' @param ... Additional arguments passed to or from other methods
#' @keywords internal
#' @return A character vector
#' @examples
#' cards <- card(rank = c("A", "10", "Q"), suit = c("♠", "♦", "♥"))
#' format(cards)   # Returns: "A♠" "10♦" "Q♥"
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
#' @param ... Additional arguments passed to or from other methods
#' @param x_arg Name of the x argument (used for error messages)
#' @param to_arg Name of the to argument (used for error messages)
#' @return A card vector
#' @keywords internal
#' @exportS3Method vctrs::vec_cast card.character
vec_cast.card.character <- function(x, to, ...) {
  message("vec_cast.card.character called!")
  ranks <- sub("^(.+?)([\u2660\u2665\u2666\u2663])$", "\\1", x)
  suits <- sub("^(.+?)([\u2660\u2665\u2666\u2663])$", "\\2", x)
  card(ranks, suits)
}

#' Cast card to character vector
#'
#' Converts a card object back to character like "Q♥".
#'
#' @param x A card object
#' @param to Unused
#' @param ... Additional arguments passed to or from other methods
#' @keywords internal
#' @return Character vector
#' @exportS3Method vctrs::vec_cast character.card
vec_cast.character.card <- function(x, to, ...) {
  paste0(vctrs::field(x, "rank"), vctrs::field(x, "suit"))
}

#' Type promotion method for card objects
#'
#' Implements method dispatch for combining card and character/card types.
#'
#' @param x A card or character vector
#' @param y Another card or character vector
#' @param ... Additional arguments passed to or from other methods
#' @keywords internal
#' @return A card prototype
#' @exportS3Method vctrs::vec_ptype2 card.character
vec_ptype2.card.character <- function(x, y, ...) card()

#' Type promotion method for card objects
#'
#' Implements method dispatch for combining card and character/card types.
#'
#' @param x A card or character vector
#' @param y Another card or character vector
#' @param ... Additional arguments passed to or from other methods
#' @keywords internal
#' @return A card prototype
#' @exportS3Method vctrs::vec_ptype2 character.card
vec_ptype2.character.card <- function(x, y, ...) card()

#' Type promotion method for card objects
#'
#' Implements method dispatch for combining card and character/card types.
#'
#' @param x A card or character vector
#' @param y Another card or character vector
#' @param ... Additional arguments passed to or from other methods
#' @keywords internal
#' @return A card prototype
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

#' Cast character vector to card
#'
#' Converts character like "A♠", "10♦" into a card object.
#'
#' @param x A character vector
#' @param to A card prototype
#' @param ... Additional arguments passed to or from other methods
#' @param x_arg Name of the x argument (used for error messages)
#' @param to_arg Name of the to argument (used for error messages)
#' @return A card vector
#' @keywords internal
#' @exportS3Method vctrs::vec_cast card.character
vec_cast.card.character <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (!inherits(to, "card")) vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  if (inherits(x, "card")) return(x)
  if (!is.character(x)) vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)

  # Use stringr for proper UTF-8 matching
  pattern <- "^(A|10|[2-9JQK])([\u2660\u2665\u2666\u2663])$"
  matches <- stringr::str_match(x, pattern)

  if (any(is.na(matches[, 1]))) {
    bad <- x[is.na(matches[, 1])]
    vctrs::stop_incompatible_cast(
      x, to,
      details = paste0("Can't convert: ", paste(bad, collapse = ", ")),
      x_arg = x_arg, to_arg = to_arg
    )
  }

  ranks <- matches[, 2]
  suits <- matches[, 3]

  card(ranks, suits)
}

#' Print a card vector
#'
#' Nicely prints a card vector as "A♠ 10♦ Q♥".
#'
#' @param x A card vector
#' @param ... Additional arguments passed to or from other methods
#' @examples
#' cards <- card(rank = c("A", "10", "Q"), suit = c("♠", "♦", "♥"))
#' print(cards)
#' @export
print.card <- function(x, ...) {
  cat(paste0(format(x), collapse = " "), "\n")
  invisible(x)
}

#' Cast anything to card (identity or error)
#'
#' Used internally by vctrs for S3 casting.
#'
#' @param x Input object.
#' @param to Card prototype.
#' @param ... Additional arguments (ignored)
#' @return Card vector or error
#' @keywords internal
#' @export
vec_cast.card <- function(x, to, ...) {
  if (inherits(x, "card")) return(x)
  vctrs::stop_incompatible_cast(x, to)
}

#' Coerce card vector to character
#'
#' Returns a character vector like "A♠", "10♦".
#'
#' @param x A card vector
#' @param ... Additional arguments (ignored)
#' @return A character vector
#' @examples
#' cards <- card(rank = c("A", "10", "Q"), suit = c("♠", "♦", "♥"))
#' as.character(cards) # [1] "A♠" "10♦" "Q♥"
#' @export
as.character.card <- function(x, ...) {
  format(x, ...)
}
