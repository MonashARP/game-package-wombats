#' @title Determine winner(s) of a Blackjack game
#' @description Handles single hand or list of hands; supports Blackjack, Charlie, Split
#' @param player_hand A `blackjack_hand` or list of `blackjack_hand`
#' @param dealer_hand A `blackjack_hand` object
#' @return Outcome string or named character vector if multiple hands
#' @export

determine_winner <- function(player_hand, dealer_hand) {
  # STRONG defensive check: list of blackjack_hand only
  is_valid_hand_list <- function(x) {
    is.list(x) &&
      length(x) > 0 &&
      all(purrr::map_lgl(x, ~inherits(.x, "blackjack_hand"))) &&
      !inherits(x, "blackjack_hand")
  }

  if (is_valid_hand_list(player_hand)) {
    results <- purrr::map_chr(player_hand, ~determine_winner(.x, dealer_hand))
    names(results) <- paste0("Hand ", seq_along(player_hand))
    return(results)
  }

  # Single hand logic
  if (!inherits(player_hand, "blackjack_hand") || !inherits(dealer_hand, "blackjack_hand")) {
    stop("Both inputs must be 'blackjack_hand' objects, or a list of them.")
  }

  p_score <- calculate_score(player_hand)
  d_score <- calculate_score(dealer_hand)

  p_blackjack <- is_blackjack(player_hand)
  d_blackjack <- is_blackjack(dealer_hand)
  p_charlie <- is_five_card_charlie(player_hand)

  if (p_score > 21 && d_score > 21) return("Both bust - Dealer wins by rule")
  if (p_score > 21) return("Dealer wins")
  if (d_score > 21) return("Player wins")

  if (p_blackjack && d_blackjack) return("Push (both Blackjack)")
  if (p_blackjack) return("Player wins with Blackjack")
  if (d_blackjack) return("Dealer wins with Blackjack")

  if (p_charlie) return("Player wins with 5-card Charlie")

  if (p_score > d_score) return("Player wins")
  if (p_score < d_score) return("Dealer wins")

  return("Push")

}

