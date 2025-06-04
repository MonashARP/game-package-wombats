#' @title Display final results with betting outcomes
#' @description Determines the winner for each player, shows the results with player names,
#' their bets, and updates remaining coins accordingly.
#' @param player_hands A named list of lists of blackjack_hand objects, keyed by player name.
#' @param dealer_hand A blackjack_hand object.
#' @param players A named list of players (as returned by `input_players()`), where each player has a `money` field.
#' @return Updated players list with adjusted money balances after the round.
#' @export
display_final_results <- function(player_hands, dealer_hand, players) {
  cat("\n=== Final Results ===\n")
  d_ranks <- card_rank(dealer_hand$cards)
  d_suits <- card_suit(dealer_hand$cards)
  d_display <- paste0(d_suits, d_ranks)
  cat("Dealer hand: [", paste(d_display, collapse = " "), "]",
      " Score:", calculate_score(dealer_hand), "\n")

  all_player_hands <- unlist(player_hands, recursive = FALSE)
  outcomes <- determine_winner(all_player_hands, dealer_hand)
  outcome_index <- 1

  for (player_name in names(player_hands)) {
    hands <- player_hands[[player_name]]
    bet <- players[[player_name]]$bets
    cat(paste0(">>> ", player_name, " (Bet: ", bet, " coins):\n"))

    total_payout <- 0

    for (i in seq_along(hands)) {
      hand <- hands[[i]]
      hand_label <- if (length(hands) == 1) "Hand" else paste0("Hand ", i)
      p_score <- calculate_score(hand)
      outcome_msg <- outcomes[outcome_index]
      outcome_index <- outcome_index + 1

      ranks <- card_rank(hand$cards)
      suits <- card_suit(hand$cards)
      display_cards <- paste0(suits, ranks)
      cat(hand_label, ": [", paste(display_cards, collapse = " "), "]  Score:", p_score, "\n")

      # Calculate payout based on outcome message and bet
      payout <- 0
      if (grepl("^Player wins with Blackjack$", outcome_msg)) {
        payout <- 1.5 * bet
        cat("    \U0001f389 Wins with Blackjack! Payout:", payout, "\n")
      } else if (grepl("^Dealer wins with Blackjack$", outcome_msg)) {
        payout <- -bet
        cat("    \u274c Dealer wins with Blackjack. You lose your bet.\n")
      } else if (grepl("^Player wins with 5-card Charlie$", outcome_msg)) {
        payout <- bet
        cat("    \U0001f4ab Wins with 5-card Charlie! Payout:", payout, "\n")
      } else if (grepl("^Dealer wins with 5-card Charlie$", outcome_msg)) {
        payout <- -bet
        cat("    \u274c Dealer wins with 5-card Charlie. You lose your bet.\n")
      } else if (grepl("^Player wins", outcome_msg)) {
        payout <- bet
        cat("    \U0001f3c6 Wins with higher score! Payout:", payout, "\n")
      } else if (grepl("^Dealer wins", outcome_msg)) {
        payout <- -bet
        cat("    \u274c Dealer wins. You lose your bet.\n")
      } else if (grepl("Push", outcome_msg)) {
        payout <- 0
        cat("    \U0001f91d Pushes with dealer. Bet returned.\n")
      } else if (grepl("^Player busted$", outcome_msg)) {
        payout <- -bet
        cat("    \U0001f4a5 Busted! You lose your bet.\n")
      } else if (grepl("^Dealer busted$", outcome_msg)) {
        payout <- bet
        cat("    \U0001f4b0 Dealer busted! You win your bet.\n")
      } else {
        payout <- 0
        cat("    Outcome unclear. No payout.\n")
      }

      total_payout <- total_payout + payout
      cat("\n")
    }

    players[[player_name]]$coins <- players[[player_name]]$coins + total_payout
    cat(paste0(player_name, "'s remaining coins: ", players[[player_name]]$coins, "\n\n"))
  }

  return(players)
}
