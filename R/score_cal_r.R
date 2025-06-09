# R/score_r_version.R

#' Calculate Blackjack Score (R version)
#'
#' Pure R implementation of blackjack scoring logic, used for comparison with C++.
#' Not exported—used internally in vignettes or benchmarks.
#'
#' @param hand A \code{blackjack_hand} object.
#' @return Integer. Total blackjack score for the hand.
#' @examples
#' hand <- new_blackjack_hand(c("A♠", "10♦"))
#' calculate_score_r(hand)  # 21
#' @keywords internal
#' @noRd
calculate_score_r <- function(hand) {
  if (!inherits(hand, "blackjack_hand")) {
    stop("Input must be of class 'blackjack_hand'")
  }

  cards <- vctrs::field(hand, "cards")
  ranks <- vctrs::field(cards, "rank")


  values <- suppressWarnings(as.numeric(ranks))
  na_idx <- is.na(values)
  values[na_idx] <- ifelse(ranks[na_idx] %in% c("J", "Q", "K"), 10, 11)


  total <- sum(values)
  ace_count <- sum(ranks == "A")

  while (total > 21 && ace_count > 0) {
    total <- total - 10
    ace_count <- ace_count - 1
  }

  return(total)
}

#' Generate random blackjack hands
#'
#' Creates a list of `blackjack_hand` objects by sampling from a standard deck.
#' This is intended for testing and performance comparison only.
#'
#' @param n Integer. Number of hands to generate.
#' @param hand_size Integer. Number of cards per hand (default is 2).
#' @return A list of `blackjack_hand` objects.
#' @noRd
#' @keywords internal
generate_blackjack_hands <- function(n = 10000) {
  ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  suits <- c("\u2660", "\u2665", "\u2666", "\u2663")

  hands <- vector("list", n)
  set.seed(42)

  for (i in seq_len(n)) {
    hand_size <- sample(2:5, 1)
    sampled_ranks <- sample(ranks, hand_size, replace = TRUE)
    sampled_suits <- sample(suits, hand_size, replace = TRUE)
    card_vec <- card(rank = sampled_ranks, suit = sampled_suits)
    hands[[i]] <- new_blackjack_hand(card_vec)
  }

  return(hands)
}
