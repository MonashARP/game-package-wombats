#' @title Play player turns (with coins and computer players)
#' @description Allows each player (human or computer) to take their turns (hit, stand, double).
#' @param player_hands A named list of lists of blackjack_hand objects, keyed by player name.
#' @param deck A character vector representing the remaining deck.
#' @param players A named list of player info (each has 'coins', 'bets', and 'type' e.g. "human" or "computer").
#' @return A list containing updated player_hands, deck, and players.

play_player_turns <- function(player_hands, deck, players) {
  current_deck <- deck
  updated_player_hands <- player_hands

  for (player_name in names(updated_player_hands)) {
    hands <- updated_player_hands[[player_name]]
    player <- players[[player_name]]

    cat(paste0("\n--- ", player_name, " ('", player$type, "') Playing ---\n"))

    for (j in seq_along(hands)) {
      hand <- hands[[j]]
      cat(paste0("Hand ", j, ": [", paste(vctrs::field(hand, "cards"), collapse = ", "), "]\n"))

      first_move <- TRUE
      repeat {
        action <- NULL

        # Decide action: human prompt or computer logic
        if (player$type == "human") {
          prompt_msg <- if (first_move) {
            "Hit, Stand or Double? (hit/stand/double/exit): "
          } else {
            "Hit or Stand? (hit/stand/exit): "
          }

          repeat {
            action <- tolower(readline(prompt_msg))
            valid_actions <- if (first_move) c("hit", "stand", "double", "exit") else c("hit", "stand", "exit")
            if (action %in% valid_actions) break
            cat("Invalid input. Please enter one of: ", paste(valid_actions, collapse = "/"), "\n")
          }

          if (action == "exit") {
            cat("Goodbye!\n")
            return(invisible(NULL))
          }
        } else if (player$type == "computer") {
          # Simple AI: hit if score < 17, stand otherwise; double only if first move and coins allow and score in reasonable range
          score <- calculate_score(hand)

          if (first_move && player$coins >= player$bets && score >= 9 && score <= 11) {
            action <- "double"
            cat("Computer chooses to double down.\n")
          } else if (score < 17) {
            action <- "hit"
            cat("Computer chooses to hit.\n")
          } else {
            action <- "stand"
            cat("Computer chooses to stand.\n")
          }
        }

        # Execute action and update hand, deck, and player's coins/bets
        res <- player_action(hand, current_deck, action, player)
        hand <- res$hand
        current_deck <- res$deck
        player <- res$player

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
      # Update hand and player after turn
      updated_player_hands[[player_name]][[j]] <- hand
      players[[player_name]] <- player
    }
  }

  return(list(player_hands = updated_player_hands, deck = current_deck, players = players))
}
