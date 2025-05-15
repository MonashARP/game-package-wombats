#' @title Play interactive Blackjack (full version)
#' @description Supports insurance, split, double down, computer players, and win logic
#' @export

play <- function() {
  repeat {
    # Input human and computer players
    players <- input_players()

    # Get bets from all players, update players with bets and coins
    players <- get_bets(players)

    # Setup initial deck, hands, and updated players
    initial_setup <- setup_and_display_initial(players)
    deck <- initial_setup$deck
    player_hands <- initial_setup$player_hands
    dealer_hand <- initial_setup$dealer_hand
    players <- initial_setup$players

    # Insurance and dealer blackjack check
    dealer_blackjack <- handle_insurance(dealer_hand)
    if (dealer_blackjack) next  # restart loop if dealer blackjack

    # Handle splitting hands
    player_hands <- handle_splitting(player_hands, deck)

    # Play player turns: update hands, deck, and players with updated coins and bets
    res <- play_player_turns(player_hands, deck, players)
    player_hands <- res$player_hands
    deck <- res$deck
    players <- res$players

    # Dealer plays only if at least one hand didn't bust
    all_scores <- unlist(purrr::map(player_hands, ~ purrr::map_dbl(.x, calculate_score)))
    if (any(all_scores <= 21)) {
      dealer_res <- dealer_action(dealer_hand, deck)
      dealer_hand <- dealer_res$hand
      deck <- dealer_res$deck
    }

    # Display final results with player coins and info
    display_final_results(player_hands, dealer_hand, players)

    # Prompt to play again
    if (!ask_play_again()) break
  }
}
