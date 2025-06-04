#' @title Play player turns (with coins and computer players)
#' @description Allows each player (human or computer) to take their turns (hit, stand, double).
#' @param player_hands A named list of lists of blackjack_hand objects, keyed by player name.
#' @param deck A character vector representing the remaining deck.
#' @param players A named list of player info (each has 'coins', 'bets', and 'is_computer' flag).
#' @return A list containing updated player_hands, deck, and players.
#' @export

play_player_turns <- function(player_hands, deck, players) {
  current_deck <- deck
  updated_player_hands <- player_hands

  print(names(updated_player_hands))

  for (player_name in names(updated_player_hands)) {
    hand_list <- updated_player_hands[[player_name]]
    hand <- hand_list[[1]]
    is_cp <- players[[player_name]]$is_computer
    player <- players[[player_name]]
    player_type <- if (isTRUE(player$is_computer)) "computer" else "human"

    cat(paste0("\n--- ", player_name, " ('", player_type, "') Playing ---\n"))

    for (j in seq_along(hand_list)) {
      hand <- hand_list[[j]]

      # Force wrap into blackjack_hand if not already
      if (!inherits(hand, "blackjack_hand")) {
        hand <- new_blackjack_hand(hand)
        hand_list[[j]] <- hand  # Update it back in the hand list
      }

      score <- calculate_score(hand)

      ranks <- card_rank(hand$cards)
      suits <- card_suit(hand$cards)
      display_cards <- paste0(suits, ranks)

      cat(paste0("Hand ", j, ": [", paste(display_cards, collapse = " "), "]\n"))
      cat("Score:", score, "\n")

      first_move <- TRUE
      repeat {
        action <- NULL

        # Decide action: human prompt or computer logic
        if (!isTRUE(player$is_computer)) {
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
        } else {
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

        ranks <- card_rank(hand$cards)
        suits <- card_suit(hand$cards)
        display_cards <- paste0(suits, ranks)

        cat("You now have: [", paste(display_cards, collapse = " "), "]\n")
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
