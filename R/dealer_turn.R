#' @title Play player turns
#' @description Allows each player to take their turns (hit, stand, double).
#' @param player_hands A list of lists of blackjack_hand objects.
#' @param deck A character vector representing the remaining deck.
#' @return An updated list of lists of blackjack_hand objects after all player turns.

play_player_turns <- function(player_hands, deck) {
  updated_player_hands <- player_hands
  current_deck <- deck
  for (i in seq_along(updated_player_hands)) {
    for (j in seq_along(updated_player_hands[[i]])) {
      hand <- updated_player_hands[[i]][[j]]
      cat(paste0("\n--- Player ", i, " Playing Hand ", j, " ---\n"))
      first_move <- TRUE
      repeat {
        prompt_msg <- if (first_move) {
          "Hit, Stand or Double? (hit/stand/double/exit): "
        } else {
          "Hit or Stand? (hit/stand/exit): "
        }

        repeat {
          action <- readline(prompt_msg)
          action <- tolower(action)
          valid_actions <- if (first_move) c("hit", "stand", "double", "exit") else c("hit", "stand", "exit")
          if (action %in% valid_actions) break
          cat("Invalid input. Please enter one of: ", paste(valid_actions, collapse = "/"), "\n")
        }

        if (action == "exit") {
          cat("Goodbye!\n")
          return(invisible(NULL))
        }

        res <- player_action(hand, current_deck, action)
        hand <- res$hand
        current_deck <- res$deck
        score <- calculate_score(hand)

        cat("You now have: [", paste(vctrs::field(hand, "cards"), collapse = ", "), "]\n")
        cat("Score:", score, "\n")

        if (action == "double") {
          cat("You chose to double down: one card only.\n")
          break
        }
        if (action == "stand" || score > 21) break

        first_move <- FALSE
      }
      updated_player_hands[[i]][[j]] <- hand
    }
  }
  return(updated_player_hands)
}
