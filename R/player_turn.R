# R/player_turn.R

#' @noRd
#' Return a string for display, e.g., "10♠", "A♥".
format_hand_display <- function(cards) {paste(cards, collapse = " ")}

#' @noRd
#' Decide action for AI player given hand, coins, bet, and current score.
ai_decide_action <- function(hand, player, first_move) {
  score <- calculate_score(hand)
  if (first_move && player$coins >= player$bets && score >= 9 && score <= 11) {
    cat("Computer chooses to double down.\n")
    return("double")
  } else if (score < 17) {
    cat("Computer chooses to hit.\n")
    return("hit")
  } else {
    cat("Computer chooses to stand.\n")
    return("stand")
  }
}

#' @noRd
#' Prompt human player for action. Returns string ("hit", "stand", "double", "exit").
human_prompt_action <- function(first_move) {
  prompt_msg <- if (first_move) {
    "Hit, Stand or Double? (hit/stand/double/exit): "
  } else {
    "Hit or Stand? (hit/stand/exit): "
  }
  repeat {
    action <- tolower(readline(prompt_msg))
    valid_actions <- if (first_move) c("hit", "stand", "double", "exit") else c("hit", "stand", "exit")
    if (action %in% valid_actions) return(action)
    cat("Invalid input. Please enter one of: ", paste(valid_actions, collapse = "/"), "\n")
  }
 }

#' @noRd
#' Wrap a vector of card strings as a blackjack_hand S3 object, if not already.
ensure_blackjack_hand <- function(hand) {
  if (!inherits(hand, "blackjack_hand")) new_blackjack_hand(hand) else hand
}


#' @title Play player turns (with coins and computer players)
#' @description Allows each player (human or computer) to take their turns (hit, stand, double).
#' @param player_hands A named list of lists of blackjack_hand objects, keyed by player name.
#' @param deck A character vector representing the remaining deck.
#' @param players A named list of player info (each has 'coins', 'bets', and 'is_computer' flag).
#' @return A list containing updated player_hands, deck, and players.
#' @export
play_player_turns <- function(player_hands, deck, players) {
  if (!is.list(player_hands) || length(player_hands) == 0) {
    stop("player_hands must be a non-empty named list.")
  }

  if (length(deck) == 0) {
    stop("Deck must not be empty.")
  }

  if (!is.list(players) || length(players) == 0) {
    stop("players must be a non-empty named list.")
  }

  current_deck <- deck
  updated_player_hands <- player_hands

  for (player_name in names(updated_player_hands)) {
    hand_list <- updated_player_hands[[player_name]]
    player <- players[[player_name]]
    player_type <- if (isTRUE(player$is_computer)) "computer" else "human"
    cat(paste0("\n--- ", player_name, " ('", player_type, "') Playing ---\n"))

    for (j in seq_along(hand_list)) {
      hand <- ensure_blackjack_hand(hand_list[[j]])
      score <- calculate_score(hand)

      # Show hand
      cat(paste0("Hand ", j, ": [", format_hand_display(hand$cards), "]\n"))
      cat("Score:", score, "\n")

      first_move <- TRUE
      repeat {
        # Decide action
        if (isTRUE(player$is_computer)) {
          action <- ai_decide_action(hand, player, first_move)
        } else {
          action <- human_prompt_action(first_move)
          if (action == "exit") {
            cat("Goodbye!\n")
            return(invisible(NULL))
          }
        }
        # Execute action (by your modular player_action, assumed ready)
        res <- player_action(hand, current_deck, action, player)
        hand <- res$hand
        current_deck <- res$deck
        player <- res$player

        # Show updated hand
        cat("You now have: [", format_hand_display(hand$cards), "]\n")
        cat("Score:", calculate_score(hand), "\n")

        if (action == "double") {
          cat("You chose to double down: one card only.\n")
          break
        }
        if (action == "stand" || calculate_score(hand) > 21) break

        first_move <- FALSE
      }
      # Update hand and player after turn
      updated_player_hands[[player_name]][[j]] <- hand
      players[[player_name]] <- player
    }
  }
  return(list(player_hands = updated_player_hands, deck = current_deck, players = players))
}
