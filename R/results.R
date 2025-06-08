# R/results.R
# Process and present game results, including settlement, winner and loser determination,
# and data visualization.

#' @title Display final results with betting outcomes
#' @description
#' Determines the winner for each player, displays results, and updates coin balances.
#' @param player_hands Named list of player hands (each can be a list for split hands).
#' @param dealer_hand Dealer's \code{blackjack_hand} object.
#' @param players Named list of player info (must include 'coins', 'bets', etc.).
#' @return Updated named list of players with adjusted coins after the round.
#' @examples
#' \dontrun{
#' # Example: display and settle bets after a round
#' player_hands <- list(
#'   Alice = list(new_blackjack_hand(c("K♠", "A♥"))),  # Blackjack!
#'   Bob   = list(new_blackjack_hand(c("9♦", "9♠")),
#'                new_blackjack_hand(c("5♣", "5♦", "Q♣"))) # Bob split
#' )
#'
#' dealer_hand <- new_blackjack_hand(c("10♠", "K♦"))
#'
#' players <- list(
#'   Alice = list(coins = 950, bets = 50, is_computer = FALSE),
#'   Bob   = list(coins = 1100, bets = 100, is_computer = TRUE)
#' )
#'
#' # Show round results and update player coin balances
#' updated_players <- display_final_results(
#'                              player_hands, dealer_hand, players
#'                              )
#' print(updated_players)
#' }
#' @export
display_final_results <- function(player_hands, dealer_hand, players) {
  cat("\n=== Final Results ===\n")
  # Dealer hand display
  d_display <- dealer_hand$cards
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

      display_cards <- hand$cards
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

#' @title Determine winner(s) of a Blackjack game
#' @description
#' Determines outcome ("Player wins", "Dealer wins", "Push", etc.) for a hand or list of hands.
#' @param player_hand A \code{blackjack_hand} or a list of them.
#' @param dealer_hand Dealer's \code{blackjack_hand} object.
#' @return A character outcome ("Player wins", ...) or a named vector if multiple hands.
#' @examples
#' hand <- new_blackjack_hand(c("A♠", "K♦"))
#' dealer <- new_blackjack_hand(c("10♣", "8♥"))
#' determine_winner(hand, dealer)
#' @export
determine_winner <- function(player_hand, dealer_hand) {
  # STRONG defensive check: list of blackjack_hand only
  is_valid_hand_list <- function(x) {
    is.list(x) &&
      length(x) > 0 &&
      all(purrr::map_lgl(x, ~inherits(.x, "blackjack_hand"))) &&
      !inherits(x, "blackjack_hand")
  }

  if (is_valid_hand_list(player_hand)) {
    results <- purrr::map_chr(player_hand, ~determine_winner(.x, dealer_hand))
    names(results) <- paste0("Hand ", seq_along(player_hand))
    return(results)
  }

  # Single hand logic
  if (!inherits(player_hand, "blackjack_hand") || !inherits(dealer_hand, "blackjack_hand")) {
    stop("Both inputs must be 'blackjack_hand' objects, or a list of them.")
  }

  p_score <- calculate_score(player_hand)
  d_score <- calculate_score(dealer_hand)

  p_blackjack <- is_blackjack(player_hand)
  d_blackjack <- is_blackjack(dealer_hand)
  p_charlie <- is_five_card_charlie(player_hand)

  if (p_score > 21 && d_score > 21) return("Both bust - Dealer wins by rule")
  if (p_score > 21) return("Dealer wins")
  if (d_score > 21) return("Player wins")

  if (p_blackjack && d_blackjack) return("Push (both Blackjack)")
  if (p_blackjack) return("Player wins with Blackjack")
  if (d_blackjack) return("Dealer wins with Blackjack")

  if (p_charlie) return("Player wins with 5-card Charlie")

  if (p_score > d_score) return("Player wins")
  if (p_score < d_score) return("Dealer wins")

  return("Push")

}

#' @title Plot current player ranking
#' @description
#' Creates a bar chart showing player coin rankings, with rank numbers on the chart.
#' @param players Named list of player objects with 'coins' field.
#' @return A plotly bar chart object.
#' @examples
#' \dontrun{
#' # Example: plot the coin ranking for three players at the end of a game session
#' players <- list(
#'   Alice   = list(coins = 1500),
#'   Bob     = list(coins = 900),
#'   Charlie = list(coins = 1200)
#' )
#' # Plot an interactive bar chart showing the ranking of players by their coins
#' plot_player_ranking(players)
#'
#' # Example: plot for only two players
#' players2 <- list(
#'   You = list(coins = 1200),
#'   Computer_1 = list(coins = 800)
#' )
#' plot_player_ranking(players2)
#' }
#' @export
plot_player_ranking <- function(players) {
  df <- data.frame(
    player = names(players),
    coins = sapply(players, function(p) p$coins),
    stringsAsFactors = FALSE
  )

  df <- df[order(-df$coins), ]
  df$rank <- seq_len(nrow(df))
  df$player <- factor(df$player, levels = df$player)

  # Tooltip only
  df$Tooltip <- paste0("Rank: ", df$rank,
                       "<br>Player: ", df$player,
                       "<br>Coins: ", df$coins)

  p <- plotly::plot_ly(
    data = df,
    x = ~player,
    y = ~coins,
    type = 'bar',
    marker = list(color = 'skyblue'),
    hovertext = ~Tooltip,
    hoverinfo = 'text',
    showlegend = FALSE
  )

  p <- plotly::layout(
    p,
    title = "\U0001f4ca Player Coin Ranking",
    xaxis = list(title = "Player"),
    yaxis = list(title = "Coins"),
    margin = list(b = 100),
    showlegend = FALSE
  )

  return(p)
}

