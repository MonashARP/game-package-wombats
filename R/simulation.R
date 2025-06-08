# R/simulation_multiplayer.R
# Multi-player Blackjack simulation

#' Simulate Blackjack games with multiple players (up to 6)
#'
#' @description
#' Simulate multiple rounds of Blackjack with 1-6 players using fully automated AI logic.
#' Each player can have different strategies for hit/stand, insurance, and splitting.
#'
#' @param num_rounds Number of games to simulate
#' @param num_players Number of players (1-6)
#' @param bet_amount Fixed bet per round (can be vector for different bets per player)
#' @param initial_coins Starting coins for players (can be vector for different amounts)
#' @param threshold Hit/stand threshold for each player (can be vector)
#' @param buy_insurance Insurance strategy for each player (can be vector of logical/functions)
#' @param split Split strategy for each player (can be vector of logical/functions)
#'
#' @return A list containing results for each player and overall game statistics
#' @examples
#' # 3 players with same strategy
#' res <- simulate_multiplayer_blackjack(
#'   num_rounds = 1000,
#'   num_players = 3,
#'   threshold = 17,
#'   buy_insurance = TRUE,
#'   split = FALSE
#' )
#'
#' # 3 players with different strategies
#' res <- simulate_multiplayer_blackjack(
#'   num_rounds = 1000,
#'   num_players = 3,
#'   threshold = c(17, 18, 16),
#'   buy_insurance = c(TRUE, FALSE, TRUE),
#'   split = c(FALSE, TRUE, FALSE),
#'   bet_amount = c(10, 20, 15),
#'   initial_coins = c(1000, 1500, 800)
#' )
#' @export
simulate_multiplayer_blackjack <- function(
    num_rounds = 10000,
    num_players = 2,
    bet_amount = 10,
    initial_coins = 1000,
    threshold = 17,
    buy_insurance = TRUE,
    split = FALSE
) {
  # Validate inputs
  if (!is.numeric(num_rounds) || num_rounds < 1) stop("num_rounds must be a positive integer.")
  if (!is.numeric(num_players) || num_players < 1 || num_players > 6) {
    stop("num_players must be between 1 and 6.")
  }

  # Expand parameters to match number of players
  bet_amount <- rep_len(bet_amount, num_players)
  initial_coins <- rep_len(initial_coins, num_players)
  threshold <- rep_len(threshold, num_players)

  # Handle insurance parameter (can be mixed logical/functions)
  if (length(buy_insurance) == 1) {
    buy_insurance <- rep(list(buy_insurance), num_players)
  } else if (length(buy_insurance) != num_players) {
    buy_insurance <- rep_len(as.list(buy_insurance), num_players)
  } else {
    buy_insurance <- as.list(buy_insurance)
  }

  # Handle split parameter (can be mixed logical/functions)
  if (length(split) == 1) {
    split <- rep(list(split), num_players)
  } else if (length(split) != num_players) {
    split <- rep_len(as.list(split), num_players)
  } else {
    split <- as.list(split)
  }

  # Validate all parameters are positive
  if (any(bet_amount < 1)) stop("All bet_amount values must be positive.")
  if (any(initial_coins < 1)) stop("All initial_coins values must be positive.")
  if (any(threshold < 1)) stop("All threshold values must be positive.")

  # Initialize players
  players <- list()
  for (i in 1:num_players) {
    players[[paste0("Player", i)]] <- list(
      is_computer = TRUE,
      coins = initial_coins[i],
      bet = bet_amount[i],
      threshold = threshold[i],
      insurance_func = NULL,
      split_func = NULL
    )
  }

  # Convert strategies to functions for each player
  for (i in 1:num_players) {
    player_name <- paste0("Player", i)

    # Split strategy
    players[[player_name]]$split_func <- if (is.logical(split[[i]])) {
      function(hand) split[[i]]
    } else if (is.function(split[[i]])) {
      split[[i]]
    } else {
      stop(paste("split parameter for player", i, "must be TRUE/FALSE or a function(hand)"))
    }

    # Insurance strategy
    players[[player_name]]$insurance_func <- if (is.logical(buy_insurance[[i]])) {
      function(player, dealer_hand) buy_insurance[[i]]
    } else if (is.function(buy_insurance[[i]])) {
      buy_insurance[[i]]
    } else {
      stop(paste("buy_insurance parameter for player", i, "must be TRUE/FALSE or a function(player, dealer_hand)"))
    }
  }

  # Initialize statistics for each player
  player_stats <- list()
  for (i in 1:num_players) {
    player_name <- paste0("Player", i)
    player_stats[[player_name]] <- list(
      win = 0, lose = 0, tie = 0,
      split_count = 0, split_win = 0, split_lose = 0,
      insurance_wins = 0, insurance_loses = 0,
      bankroll_history = numeric(num_rounds + 1)
    )
    player_stats[[player_name]]$bankroll_history[1] <- initial_coins[i]
  }

  # Main simulation loop
  for (round in seq_len(num_rounds)) {
    # Create and shuffle deck
    ranks <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)
    suits <- rep(c("\u2660", "\u2665", "\u2666", "\u2663"), each = 13)
    deck_char <- sample(paste0(ranks, suits))
    deck <- vctrs::vec_cast(deck_char, to = card(rank = character(), suit = character()))

    # Deal initial cards
    deal_result <- deal_cards(deck, num_players = num_players)
    all_player_hands <- deal_result$player_hands
    dealer_hand <- deal_result$dealer_hand
    deck <- deal_result$deck

    # Process each player
    for (i in 1:num_players) {
      player_name <- paste0("Player", i)
      player <- players[[player_name]]
      stats <- player_stats[[player_name]]
      player_hands <- list(all_player_hands[[i]]) # Start with one hand

      # Insurance phase
      insurance_bet <- 0; insurance_paid <- 0
      if (card_rank(dealer_hand$cards[1]) == "A" &&
          player$insurance_func(list(hand = player_hands[[1]], coins = player$coins), dealer_hand)) {
        insurance_bet <- player$bet / 2
        if (player$coins >= insurance_bet) {
          player$coins <- player$coins - insurance_bet
          if (is_blackjack(dealer_hand)) {
            insurance_paid <- insurance_bet * 2
            player$coins <- player$coins + insurance_paid
            stats$insurance_wins <- stats$insurance_wins + 1
          } else {
            stats$insurance_loses <- stats$insurance_loses + 1
          }
        }
      }

      # Skip if dealer has blackjack (insurance already handled)
      if (is_blackjack(dealer_hand)) {
        stats$lose <- stats$lose + 1
        player$coins <- player$coins - player$bet
        stats$bankroll_history[round + 1] <- player$coins
        players[[player_name]] <- player
        player_stats[[player_name]] <- stats
        next
      }

      # Handle splitting and playing hands
      split_done <- FALSE
      hand_queue <- player_hands
      hand_results <- character(0)

      while (length(hand_queue) > 0) {
        hand <- hand_queue[[1]]
        hand_queue <- hand_queue[-1]

        # Check for split
        if (!split_done && length(hand$cards) == 2 && player$split_func(hand)) {
          stats$split_count <- stats$split_count + 1
          cards <- hand$cards
          if (length(deck) >= 2) {
            hand1 <- new_blackjack_hand(c(cards[1], deck[1]))
            hand2 <- new_blackjack_hand(c(cards[2], deck[2]))
            deck <- deck[-c(1,2)]
            hand_queue <- c(list(hand1, hand2), hand_queue)
            split_done <- TRUE
            next
          }
        }

        # Player hits/stands based on threshold
        repeat {
          score <- calculate_score(hand)
          if (length(hand$cards) >= 5 && score <= 21) break
          if (score < player$threshold) {
            if (length(deck) < 1) break
            hand <- new_blackjack_hand(c(hand$cards, deck[1]))
            deck <- deck[-1]
          } else {
            break
          }
        }

        # Dealer plays (same for all players in this round)
        if (length(hand_results) == 0) {  # Only play dealer once per round
          dealer_res <- dealer_action(dealer_hand, deck)
          dealer_hand <- dealer_res$hand
          deck <- dealer_res$deck
        }

        # Determine winner for this hand
        result <- determine_winner(hand, dealer_hand)
        hand_results <- c(hand_results, result)

        # Track split results
        if (stats$split_count > 0) {
          if (grepl("^Player wins", result)) stats$split_win <- stats$split_win + 1
          if (grepl("^Dealer wins", result)) stats$split_lose <- stats$split_lose + 1
        }
      }

      # Settle the round for this player
      if (any(grepl("^Player wins", hand_results))) {
        stats$win <- stats$win + 1
        player$coins <- player$coins + player$bet
      } else if (all(grepl("^Dealer wins", hand_results))) {
        stats$lose <- stats$lose + 1
        player$coins <- player$coins - player$bet
      } else {
        stats$tie <- stats$tie + 1
      }

      stats$bankroll_history[round + 1] <- player$coins

      # Update player and stats
      players[[player_name]] <- player
      player_stats[[player_name]] <- stats
    }
  }

  # Compile results
  results <- list(
    num_players = num_players,
    num_rounds = num_rounds,
    players = list()
  )

  for (i in 1:num_players) {
    player_name <- paste0("Player", i)
    stats <- player_stats[[player_name]]

    results$players[[player_name]] <- list(
      win_rate = stats$win / num_rounds,
      lose_rate = stats$lose / num_rounds,
      tie_rate = stats$tie / num_rounds,
      insurance_win_rate = stats$insurance_wins / (stats$insurance_wins + stats$insurance_loses + 1e-9),
      split_rate = stats$split_count / num_rounds,
      split_win = stats$split_win,
      split_lose = stats$split_lose,
      final_coins = players[[player_name]]$coins,
      bankroll_history = stats$bankroll_history,
      initial_coins = initial_coins[i],
      bet_amount = bet_amount[i],
      threshold = threshold[i]
    )
  }

  # Add overall statistics
  total_wins <- sum(sapply(player_stats, function(x) x$win))
  total_losses <- sum(sapply(player_stats, function(x) x$lose))
  total_ties <- sum(sapply(player_stats, function(x) x$tie))
  total_games <- num_players * num_rounds

  results$overall <- list(
    total_games = total_games,
    overall_win_rate = total_wins / total_games,
    overall_lose_rate = total_losses / total_games,
    overall_tie_rate = total_ties / total_games
  )

  # Create bankroll history in format compatible with plot_bankroll_history
  # Convert from individual player histories to round-by-round snapshots
  bankroll_history_formatted <- list()
  for (round in 0:num_rounds) {
    round_data <- numeric(num_players)
    names(round_data) <- paste0("Player", 1:num_players)

    for (i in 1:num_players) {
      player_name <- paste0("Player", i)
      round_data[player_name] <- player_stats[[player_name]]$bankroll_history[round + 1]
    }
    bankroll_history_formatted[[round + 1]] <- round_data
  }

  results$bankroll_history <- bankroll_history_formatted

  return(results)
}

#' Helper function to analyze multi-player results
#'
#' @param results Results from simulate_multiplayer_blackjack
#' @export
analyze_multiplayer_results <- function(results) {
  cat("=== MULTI-PLAYER BLACKJACK SIMULATION RESULTS ===\n")
  cat("Number of players:", results$num_players, "\n")
  cat("Number of rounds:", results$num_rounds, "\n\n")

  for (i in 1:results$num_players) {
    player_name <- paste0("Player", i)
    player_results <- results$players[[player_name]]

    cat("--- ", player_name, " ---\n")
    cat("Strategy: Hit <", player_results$threshold, ", Stand >=", player_results$threshold, "\n")
    cat("Bet Amount: $", player_results$bet_amount, "\n")
    cat("Win Rate: ", sprintf("%.2f%%", player_results$win_rate * 100), "\n")
    cat("Lose Rate: ", sprintf("%.2f%%", player_results$lose_rate * 100), "\n")
    cat("Tie Rate: ", sprintf("%.2f%%", player_results$tie_rate * 100), "\n")
    cat("Starting Coins: $", player_results$initial_coins, "\n")
    cat("Final Coins: $", player_results$final_coins, "\n")
    cat("Net Profit/Loss: $", player_results$final_coins - player_results$initial_coins, "\n")
    if (player_results$split_rate > 0) {
      cat("Split Rate: ", sprintf("%.2f%%", player_results$split_rate * 100), "\n")
    }
    cat("\n")
  }

  cat("--- OVERALL STATISTICS ---\n")
  cat("Total Games Played: ", results$overall$total_games, "\n")
  cat("Overall Win Rate: ", sprintf("%.2f%%", results$overall$overall_win_rate * 100), "\n")
  cat("Overall Lose Rate: ", sprintf("%.2f%%", results$overall$overall_lose_rate * 100), "\n")
  cat("Overall Tie Rate: ", sprintf("%.2f%%", results$overall$overall_tie_rate * 100), "\n")
}
