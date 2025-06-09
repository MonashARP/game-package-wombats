# R/player_logic.R

# --------------------------- player_actions ---------------------------------
#' @title Player action: hit, stand, or double (with betting)
#' @description
#' Apply a player action ("hit", "stand", or "double") for one hand, updating hand, deck, and player's coin/bet info.
#' @param hand A \code{blackjack_hand} object.
#' @param deck A character vector of remaining cards.
#' @param action Character: "hit", "stand", or "double".
#' @param player A list representing the player (fields: 'coins', 'bets', etc.).
#' @return A list with updated elements:
#'   \describe{
#'     \item{hand}{Updated blackjack hand (\code{blackjack_hand}).}
#'     \item{deck}{Updated deck (character vector).}
#'     \item{player}{Updated player info (list).}
#'   }
#' @examples
#' hand <- new_blackjack_hand(c("8♠", "3♦"))
#' deck <- c("10♣", "4♥", "6♣")
#' player <- list(coins = 900, bets = 100)
#' player_action(hand, deck, "hit", player)
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

  if (action == "double") {
    if (player$coins >= player$bets) {
      player$coins <- player$coins - player$bets
      player$bets <- player$bets * 2
      cat("\U0001f4b0 You doubled your bet. New bet:", player$bets, "\n")
    } else {
      cat("\u26a0\ufe0f Not enough coins to double. Treated as a hit instead.\n")
      action <- "hit"  # downgrade to hit
    }
  }

  # Draw a card (only now!)
  new_card <- deck[1]
  updated_hand <- new_blackjack_hand(c(vctrs::field(hand, "cards"), new_card))
  updated_deck <- deck[-1]

  return(list(hand = updated_hand, deck = updated_deck, player = player))
}

# --------------------------- player_turn ----------------------------------

#' @title Decide action for AI player
#' @description
#' Logic for AI player: chooses "double" on first move if conditions are right, otherwise "hit" or "stand".
#' @param hand A \code{blackjack_hand} object.
#' @param player A list with player info (coins, bets, etc.).
#' @param first_move Logical. Is this the player's first move?
#' @return Character: one of "hit", "stand", or "double".
#' @keywords internal
#' @noRd
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

#' @title Prompt human for player action
#' @description Prompts a human for their action; returns string ("hit", "stand", "double", "exit").
#' @param first_move Logical. Is this the player's first move?
#' @return Character: one of "hit", "stand", "double", "exit".
#' @keywords internal
#' @noRd
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

#' @title Play player turns for all players
#' @description
#' Allows each player (human or computer) to play their turn for all hands (with hit/stand/double logic).
#' @param player_hands A named list of lists of \code{blackjack_hand} objects, one list per player.
#' @param deck Character vector of remaining cards.
#' @param players Named list of player info (fields: 'coins', 'bets', 'is_computer', etc.).
#' @return A list:
#'   \describe{
#'     \item{player_hands}{Updated list of player hands.}
#'     \item{deck}{Updated deck after all turns.}
#'     \item{players}{Updated player info.}
#'       }
#' @examples
#' # Automated play for 2 computer players (does not require user input)
#' \donttest{
#'   player_hands <- list(
#'     Alice = list(new_blackjack_hand(c("9♠", "7♦"))),
#'     Bob = list(new_blackjack_hand(c("5♣", "K♥")))
#'   )
#'   deck <- c("8♣", "2♦", "4♠")
#'   players <- list(
#'     Alice = list(coins = 800, bets = 100, is_computer = TRUE),
#'     Bob = list(coins = 800, bets = 100, is_computer = TRUE)
#'   )
#'   play_player_turns(player_hands, deck, players)
#' }
#'
#' # Human-computer mix (will prompt for user input)
#' \dontrun{
#'   player_hands <- list(
#'     Alice = list(new_blackjack_hand(c("9♠", "7♦"))),
#'     Bob = list(new_blackjack_hand(c("5♣", "K♥")))
#'   )
#'   deck <- c("8♣", "2♦", "4♠")
#'   players <- list(
#'     Alice = list(coins = 800, bets = 100, is_computer = FALSE), # Will prompt for input!
#'     Bob = list(coins = 800, bets = 100, is_computer = TRUE)
#'   )
#'   play_player_turns(player_hands, deck, players)
#' }
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
