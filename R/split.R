#' @title Handle splitting
#' @description Allows players with pairs to split their hand.
#' @param player_hands A list of lists of blackjack_hand objects.
#' @param deck A character vector representing the remaining deck.
#' @return An updated list of lists of blackjack_hand objects after potential splits.

handle_splitting <- function(player_hands, deck) {
  updated_player_hands <- player_hands
  deck_index <- 1
  for (i in seq_along(updated_player_hands)) {
    hand <- updated_player_hands[[i]][[1]]
    ph_cards <- vctrs::field(hand, "cards")

    if (length(ph_cards) == 2 && ph_cards[1] == ph_cards[2]) {
      repeat {
        sp <- tolower(readline(paste0("Player ", i, ", you have a pair. Split hand? (yes/no): ")))
        if (sp %in% c("yes", "no")) break
        cat("Invalid input. Please enter 'yes' or 'no'.\n")
      }
      if (sp == "yes") {
        first <- new_blackjack_hand(c(ph_cards[1], deck[deck_index]))
        second <- new_blackjack_hand(c(ph_cards[2], deck[deck_index + 1]))
        updated_player_hands[[i]] <- list(first, second)
        cat("Player ", i, " split into:\n")
        cat("  Hand 1: [", paste(vctrs::field(first, "cards"), collapse = ", "), "]\n")
        cat("  Hand 2: [", paste(vctrs::field(second, "cards"), collapse = ", "), "]\n")
        deck_index <- deck_index + 2
      }
    }
  }
  return(updated_player_hands)
}
