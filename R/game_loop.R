# R/game_loop.R
# Interactive game loop functions

#' @title Play a Full Interactive Game of Blackjack
#' @description
#' Launches an interactive multiplayer game of Blackjack with full game mechanics.
#' The game supports up to six players (human or computer), betting, doubling down,
#' splitting hands, insurance, and dealer logic. Player data (names and coin balances)
#' is saved between sessions.
#'
#' Features include:
#' - Computer-controlled dealer and players
#' - Coin betting system with balance persistence
#' - Support for Blackjack rules: insurance, double down, and split
#' - Persistent storage of player profiles and bankroll history
#'
#' @details
#' This is the main function to run the full-featured Blackjack game. The game is played
#' in rounds. After each round, players are asked if they wish to continue. The game
#' loop will end if no players wish to continue or a player exits early.
#'#' @return No return value. This function is run for its side effects: interactive gameplay,
#' screen output, and updates to stored player data.
#'
#' @examples
#'\dontrun{
#'   play()
#'}
#'
#' @export

play <- function() {
  cat("\n\U0001f3b2 Welcome to Blackjack Deluxe! \U0001f3b2\n")
  cat("This is a multiplayer Blackjack game where you can:\n")
  cat("- Play with up to 6 players (human and computer opponents)\n")
  cat("- Bet coins, double down, split hands, and use insurance\n")
  cat("- Play against a dealer controlled by the computer\n")
  cat("- Save your player names and coin balances between sessions\n\n")
  cat("Get ready to test your luck and strategy!\n\n")

  saved_data <- load_players()
  players_db <- saved_data$players
  bankroll_history <- saved_data$bankroll_history

  repeat {

    input_res <- input_players(players_db)
    players <- input_res$session_players
    players_db <- input_res$players_db

    players <- get_bets(players)

    initial_setup <- setup_and_display_initial(players)
    deck <- initial_setup$deck

    player_hands <- initial_setup$player_hands
    dealer_hand <- initial_setup$dealer_hand
    players <- initial_setup$players

    insurance_res <- handle_insurance(dealer_hand, players)
    dealer_blackjack <- insurance_res$dealer_blackjack
    players <- insurance_res$players

    if (dealer_blackjack) {
      cat("Dealer has Blackjack. Round ends.\n")
      res <- suppressWarnings(end_round(player_hands, dealer_hand, players, players_db, bankroll_history))
      players <- res$players
      bankroll_history <- res$bankroll_history

      if (!ask_play_again()) break
      next
    }

    split_res <- handle_splitting(player_hands, deck, players)
    player_hands <- split_res$player_hands
    deck <- deck[-(1:(split_res$deck_index - 1))]

    res <- play_player_turns(player_hands, deck, players)
    if (is.null(res)) {
      cat("\nGame ended due to player exit during turns.\n")
      break
    }
    player_hands <- res$player_hands
    deck <- res$deck
    players <- res$players

    all_scores <- unlist(purrr::map(player_hands, ~ purrr::map_dbl(.x, calculate_score)))
    if (any(all_scores <= 21)) {
      dealer_res <- dealer_action(dealer_hand, deck)
      dealer_hand <- dealer_res$hand
      deck <- dealer_res$deck
    }

    res <- suppressWarnings(end_round(player_hands, dealer_hand, players, players_db, bankroll_history))
    players <- res$players
    bankroll_history <- res$bankroll_history

    players_db <- modifyList(players_db, players)

    save_players(players_db, bankroll_history)

    if (!ask_play_again()) break
  }
}

#' @title Ask to play again
#' @description Prompts the user to play another round or exit the game.
#' @return A logical indicating whether the player wants to play again (TRUE) or exit (FALSE).
#' @examples
#' \dontrun{
#' ask_play_again()
#' # User types 'again' or 'exit' at prompt
#' }
#' @export
ask_play_again <- function() {
  repeat {
    cat("\n")
    again <- tolower(readline(prompt = "Type 'again' to play another game, or 'exit' to quit: "))
    if (again == "again") return(TRUE)
    if (again == "exit") return(FALSE)
    cat("Invalid input. Please type 'again' or 'exit'.\n")
  }
}

#' Exit Blackjack session
#' @description
#' Intended for ending an interactive Blackjack session without using play().
#' @return Invisibly returns NULL.
#' @examples
#' exit()  # Prints "Exiting Blackjack session. Goodbye!"
#' @export
exit <- function() {
  cat("Exiting Blackjack session. Goodbye!\n")
  return(invisible(NULL))
}

