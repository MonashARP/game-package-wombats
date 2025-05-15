#' @title Player action: hit, stand, or double (with betting)
#' @param hand A `blackjack_hand` object
#' @param deck A character vector of remaining cards
#' @param action A string: "hit", "stand", or "double"
#' @param player A list representing the player (with 'coins' and 'bets')
#' @return A list with updated `hand`, `deck`, and `player`
#' @export

player_action <- function(hand, deck, action, player) {
  if (!inherits(hand, "blackjack_hand")) {
    stop("Input 'hand' must be of class 'blackjack_hand'")
  }

  action <- tolower(action)
  if (!action %in% c("hit", "stand", "double")) {
    stop("Invalid action: must be 'hit', 'stand', or 'double'")
  }

  if (action == "stand") {
    return(list(hand = hand, deck = deck, player = player))
  }

  if (length(deck) == 0) stop("Deck is empty!")
  new_card <- deck[1]
  updated_hand <- new_blackjack_hand(c(vctrs::field(hand, "cards"), new_card))
  updated_deck <- deck[-1]

  # Handle double down logic
  if (action == "double") {
    if (player$coins >= player$bets) {
      player$coins <- player$coins - player$bets
      player$bets <- player$bets * 2
      cat("💰 You doubled your bet. New bet:", player$bets, "\n")
    } else {
      cat("⚠️ Not enough coins to double. Treated as a hit instead.\n")
    }
  }

  return(list(hand = updated_hand, deck = updated_deck, player = player))
}
