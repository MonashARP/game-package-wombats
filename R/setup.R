# R/setup.R

#' @title Set up the game and display initial hands with bets
#' @description
#' Shuffles the deck, deals initial hands to all players and the dealer, deducts bets, and displays the initial cards (only one revealed for each hand).
#' @param players A named list of player objects; each should include 'name', 'coins', and 'bets' fields.
#' @return A list with:
#'   \describe{
#'     \item{deck}{Remaining shuffled deck as a card vector.}
#'     \item{player_hands}{Named list of each player's \code{blackjack_hand} (initial 2 cards).}
#'     \item{dealer_hand}{The dealer's \code{blackjack_hand}.}
#'     \item{players}{Updated players list (with bets already deducted).}
#'   }
#' @examples
#' players <- list(
#'   Alice = list(name = "Alice", coins = 1000, bets = 100),
#'   Bob = list(name = "Bob", coins = 1000, bets = 100)
#' )
#' res <- setup_and_display_initial(players)
#' # res$deck, res$player_hands, res$dealer_hand, res$players
#' @export

setup_and_display_initial <- function(players) {
  num_players <- length(players)

  # Prepare deck
  ranks <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)
  suits <- rep(c("\u2660", "\u2665", "\u2666", "\u2663"), each = 13)

  deck_char <- paste0(ranks, suits)
  deck_char <- sample(deck_char, length(deck_char)) # Shuffle the deck
  deck <- vctrs::vec_cast(deck_char, to = card(rank = character(), suit = character())) # Convert to card class

  # Deal cards
  deal_result <- deal_cards(deck, num_players)
  player_hands_raw <- deal_result$player_hands
  dealer_hand <- deal_result$dealer_hand
  deck <- deal_result$deck

  player_hands <- vector("list", num_players)
  names(player_hands) <- names(players)

  for (i in seq_along(player_hands)) {
    player_hands[[names(players)[i]]] <- player_hands_raw[[i]]
  }

  for (player_name in names(players)) {
    hand_obj <- player_hands[[player_name]]
    hand_cards <- hand_obj$cards
    ranks <- card_rank(hand_cards)
    suits <- card_suit(hand_cards)
    display_cards <- paste0(ranks, suits)

    # Show only the first card and "?" for all players
    cat(player_name, "'s hand: ", display_cards[1], " ?", " | Bet: ", players[[player_name]]$bets, "\n", sep = "")
  }

  # Dealer hand display outside loop
  dealer_rank <- card_rank(dealer_hand$cards)
  dealer_suit <- card_suit(dealer_hand$cards)
  dealer_display <- paste0(dealer_rank, dealer_suit)
  cat("Dealer shows: ", dealer_display[1], " ?", "\n", sep = "")

  return(list(
    deck = deck,
    player_hands = player_hands,
    dealer_hand = dealer_hand,
    players = players
  ))
}

