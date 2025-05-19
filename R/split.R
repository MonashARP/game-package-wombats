#' @title Handle splitting
#' @description Allows players with pairs to split their hand.
#' @param player_hands A list of lists of blackjack_hand objects.
#' @param deck A character vector representing the remaining deck.
#' @param players A named list of player objects (including names and is_computer flag).
#' @return An updated list of lists of blackjack_hand objects after potential splits.
#' @export

handle_splitting <- function(player_hands, deck, players) {
  updated_player_hands <- player_hands
  deck_index <- 1
  player_names <- names(players)

  for (i in seq_along(updated_player_hands)) {
    hand <- updated_player_hands[[i]][[1]]
    ph_cards <- vctrs::field(hand, "cards")
    player_name <- player_names[i]
    is_computer <- players[[player_name]]$is_computer

    if (length(ph_cards) == 2 && ph_cards[1] == ph_cards[2]) {
      do_split <- FALSE

      if (is_computer) {
        # Simple computer logic: always split 8s or Aces
        if (ph_cards[1] %in% c("8", "A")) {
          do_split <- TRUE
          cat(player_name, "decides to split a pair of ", ph_cards[1], "s.\n")
        } else {
          cat(player_name, "decides NOT to split a pair of ", ph_cards[1], "s.\n")
        }
      } else {
        repeat {
          sp <- tolower(readline(paste0(player_name, ", you have a pair of ", ph_cards[1], "s. Split hand? (yes/no): ")))
          if (sp %in% c("yes", "no")) {
            do_split <- (sp == "yes")
            break
          }
          cat("Invalid input. Please enter 'yes' or 'no'.\n")
        }
      }

      if (do_split) {
        first <- new_blackjack_hand(c(ph_cards[1], deck[deck_index]))
        second <- new_blackjack_hand(c(ph_cards[2], deck[deck_index + 1]))
        updated_player_hands[[i]] <- list(first, second)
        cat(player_name, "split into:\n")
        # Hand 1
        f_cards <- vctrs::field(first, "cards")
        f_suits <- vctrs::field(first, "suits")
        f_display <- paste0(f_suits, f_cards)
        cat("  Hand 1: [", paste(f_display, collapse = " "), "]\n")
        # Hand 2
        s_cards <- vctrs::field(second, "cards")
        s_suits <- vctrs::field(second, "suits")
        s_display <- paste0(s_suits, s_cards)
        cat("  Hand 2: [", paste(s_display, collapse = " "), "]\n")
        deck_index <- deck_index + 2
      }
    }
  }

  return(updated_player_hands)
}
