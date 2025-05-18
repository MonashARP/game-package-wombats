#' @title End round processing
#' @description Handles end of round operations: displaying results, saving players,
#' updating bankroll history, and plotting results.
#' @param player_hands Named list of player hands.
#' @param dealer_hand Dealer's hand.
#' @param players Named list of players with coin balances.
#' @param bankroll_history List tracking coin balances over rounds.
#' @param players_db Player Database in Local Directory
#' @return A list with updated `players` and `bankroll_history`.
#' @export

end_round <- function(player_hands, dealer_hand, players, players_db, bankroll_history) {
  players <- display_final_results(player_hands, dealer_hand, players)
  save_players(players)

  coin_snapshot <- sapply(players_db, function(p) p$coins)
  bankroll_history[[length(bankroll_history) + 1]] <- coin_snapshot

  # Create each plotly plot object
  p1 <- plot_bankroll_history(bankroll_history)
  p2 <- plot_player_ranking(players_db)

  # Combine plots side-by-side
  combined_plot <- plotly::subplot(
    p1, p2,
    nrows = 1,
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,  # allow x-axis titles
    titleY = TRUE   # allow y-axis titles
  )

  # Add layout without pipe
  combined_plot <- plotly::layout(
    combined_plot,
    title = "\U0001f0cf Blackjack Game Summary",
    margin = list(t = 80)
  )

  print(combined_plot)
  cat("\n\n")

  return(list(players = players, bankroll_history = bankroll_history))
}
