# R/ deal_card.R

#' @title Deal initial cards in Blackjack
#' @description
#' Deal two cards to each player and to the dealer from a shuffled deck.
#' Returns player and dealer hands in Blackjack format.
#' @param deck A character vector representing the deck. If NULL, a fresh 52-card deck is used.
#' @param num_players An integer (default = 1). Number of players to deal cards to.
#' @return A list with the following components:
#'   \describe{
#'     \item{player_hands}{A list of \code{blackjack_hand} objects, one for each player (each hand has 2 cards).}
#'     \item{dealer_hand}{A \code{blackjack_hand} object for the dealer (2 cards).}
#'     \item{deck}{The remaining deck as a character vector (undealt cards).}
#'   }
#' @examples
#' # Deal for 2 players from a fresh deck
#' hands <- deal_cards(num_players = 2)
#' hands$player_hands      # list of 2 blackjack_hand objects
#' hands$dealer_hand       # dealer's hand
#' hands$deck[1:5]         # top 5 cards of remaining deck
#'
#' # Deal from a given deck (e.g. after removing cards)
#' custom_deck <- c("A♠", "10♦", "5♥", "7♣", "Q♠", "3♦", "K♣", "8♠", "6♥", "2♣")
#' deal_cards(deck = custom_deck, num_players = 1)
#' @export

deal_cards <- function(deck = NULL, num_players = 1) {
  # Create a standard 52-card deck if none supplied
  if (is.null(deck)) {
    ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
    suits <- c("\u2660", "\u2665", "\u2666", "\u2663")
    deck <- as.vector(outer(ranks, suits, paste0))
    deck <- sample(deck, length(deck))
  }

  if (!all(grepl("^(A|10|[2-9]|J|Q|K)[\u2660\u2665\u2666\u2663]$", deck))) {
    stop("Invalid card values in deck.")
  }

  # Ensure enough cards to deal
  total_needed <- 2 * (num_players + 1)
  if (length(deck) < total_needed) stop("Not enough cards to deal.")

  # Sample required number of cards
  drawn <- deck[1:total_needed]
  deck <- deck[-(1:total_needed)]

  # Create hands
  player_hands <- vector("list", num_players)
  for (i in seq_len(num_players)) {
    player_hands[[i]] <- new_blackjack_hand(drawn[(2*i-1):(2*i)])
  }
  dealer_hand <- new_blackjack_hand(drawn[(2*num_players+1):(2*num_players+2)])

  return(list(
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    deck = deck
  ))
}
