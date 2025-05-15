#' @title Play interactive Blackjack (full version)
#' @description Supports insurance, split, double down, computer players, and win logic
#' @export

play <- function() {
  cat("\n🎲 Welcome to Blackjack Deluxe! 🎲\n")
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
      res <- suppressWarnings(end_round(player_hands, dealer_hand, players_db, bankroll_history))
      players <- res$players
      bankroll_history <- res$bankroll_history

      if (!ask_play_again()) break
      next
    }

    player_hands <- handle_splitting(player_hands, deck, players)

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
