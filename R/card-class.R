# R/card-class.R

#' Create a card vector
#'
#' @param rank   Character vector, allowed values:："A","2",...,"10","J","Q","K"
#' @param suit   Character vector, allowed values："♠","♥","♦","♣"
#' @return       A vctrs record vector with the class "card"
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

#' @export
format.card <- function(x, ...) {
  ranks <- vctrs::field(x, "rank")
  suits <- vctrs::field(x, "suit")
  paste0(ranks, suits)
}

# When both objects are cards, return an empty card prototype to facilitate the merging of vctrs
#' @export
vec_ptype2.card <- function(x, y, ...) {
  if (inherits(y, "card")) {
    # As long as both sides are cards, a record of length 0 and both fields rank/suit are Characters will be returned
    vctrs::new_rcrd(
      list(rank = character(), suit = character()),
      class = "card"
    )
  } else {
    # When the types on both sides do not match, return error
    vctrs::stop_incompatible_type(x, y)
  }
}

# Support from the character vector (in the form "A♠", "10♦") -> card
#' @export
vec_cast.card <- function(x, to, ...) {

  if (!inherits(to, "card")) vctrs::stop_incompatible_cast(x, to)
  if (inherits(x, "card")) return(x)
  if (!is.character(x)) vctrs::stop_incompatible_cast(x, to)

  ranks <- sub("^([0-9A-Z]{1,2})([♠♥♦♣])$", "\\1", x)
  suits <- sub("^([0-9A-Z]{1,2})([♠♥♦♣])$", "\\2", x)

  # checjk for invalid values
  invalid <- !(suits %in% c("♠","♥","♦","♣")) | grepl("^$", ranks)
  if (any(invalid)) {
    bad_vals <- x[invalid]
    msg <- paste0(
      "Can't convert these values to <card>: ",
      paste0("'", bad_vals, "'", collapse = ", ")
    )
    vctrs::stop_incompatible_cast(x, to, details = msg)
  }

  card(ranks, suits)
}
