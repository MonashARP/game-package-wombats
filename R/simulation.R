# R/simulation.R
# Strategy check using simulation

#' Simulate Blackjack games with customizable hit, insurance, and split strategies
#'
#' @description
#' Simulate multiple rounds of Blackjack using fully automated AI logic.
#' Allows user to specify hit/stand threshold, whether to buy insurance
#' (logical or custom function), and whether/when to split pairs (logical or function).
#'
#' @param num_rounds Number of games to simulate
#' @param bet_amount Fixed bet per round
#' @param initial_coins Starting coins for the player
#' @param threshold Hit if < threshold, stand if >= threshold
#' @param buy_insurance Logical or function; if TRUE, always buy insurance when dealer shows A.
#'   If function, should return TRUE/FALSE given (player, dealer_hand).
#' @param split Logical or function; if TRUE, always split when possible;
#'   if function, custom logic (function(hand) returns TRUE/FALSE).
#'
#' @return A list: win/lose/tie/insurance/split stats and bankroll history
#' @examples
#' # Always split pairs, always buy insurance, hit <17 stand >=17
#' res <- simulate_blackjack(num_rounds = 10000, threshold = 17, buy_insurance = TRUE, split = TRUE)
#' print(res$win_rate)
#'
#' # Never split, never buy insurance, hit <18 stand >=18
#' res <- simulate_blackjack(num_rounds = 10000, threshold = 18, buy_insurance = FALSE, split = FALSE)
#'
#' # Split only A or 8, only buy insurance if own hand is blackjack
#' res <- simulate_blackjack(
#'   num_rounds = 10000,
#'   threshold = 17,
#'   buy_insurance = function(player, dealer_hand) {
#'     # Only buy insurance if player has blackjack
#'     is_blackjack(player$hand)
#'   },
#'   split = function(hand) {
#'     ranks <- card_rank(hand$cards)
#'     length(ranks) == 2 && ranks[1] == ranks[2] && ranks[1] %in% c("8", "A")
#'   }
#' )
#' @export
simulate_blackjack <- function(
    num_rounds = 10000,
    bet_amount = 10,
    initial_coins = 1000,
    threshold = 17,
    buy_insurance = TRUE,
    split = FALSE
) {
  # Validate inputs
  if (!is.numeric(num_rounds) || num_rounds < 1) stop("num_rounds must be a positive integer.")
  if (!is.numeric(bet_amount) || bet_amount < 1) stop("bet_amount must be a positive number.")
  if (!is.numeric(initial_coins) || initial_coins < 1) stop("initial_coins must be a positive number.")
  if (!is.numeric(threshold) || threshold < 1) stop("threshold must be a positive number.")

  # Convert split strategy into a function
  split_func <- if (is.logical(split)) {
    function(hand) split
  } else if (is.function(split)) {
    split
  } else {
    stop("split must be TRUE/FALSE or a function(hand)")
  }
  # Convert insurance strategy into a function
  insurance_func <- if (is.logical(buy_insurance)) {
    function(player, dealer_hand) buy_insurance
  } else if (is.function(buy_insurance)) {
    buy_insurance
  } else {
    stop("buy_insurance must be TRUE/FALSE or a function(player, dealer_hand)")
  }

  players <- list(
    Player1 = list(
      is_computer = TRUE,
      coins = initial_coins,
      bets = bet_amount
    )
  )
  win <- 0; lose <- 0; tie <- 0
  split_count <- 0; split_win <- 0; split_lose <- 0
  insurance_wins <- 0; insurance_loses <- 0
  bankroll_history <- numeric(num_rounds + 1)
  bankroll_history[1] <- initial_coins

  for (i in seq_len(num_rounds)) {

    # deal card
    ranks <- rep(c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"), 4)
    suits <- rep(c("\u2660", "\u2665", "\u2666", "\u2663"), each = 13)
    deck_char <- sample(paste0(ranks, suits))
    deck <- vctrs::vec_cast(deck_char, to = card(rank = character(), suit = character()))
    deal_result <- deal_cards(deck, num_players = 1)
    player_hands <- list(deal_result$player_hands[[1]]) # list of hands
    dealer_hand <- deal_result$dealer_hand
    deck <- deal_result$deck

    # Insurance
    insurance_bet <- 0; insurance_paid <- 0
    if (card_rank(dealer_hand$cards[1]) == "A" && insurance_func(list(hand = player_hands[[1]], coins = players$Player1$coins), dealer_hand)) {
      insurance_bet <- bet_amount / 2
      if (players$Player1$coins >= insurance_bet) {
        players$Player1$coins <- players$Player1$coins - insurance_bet
        if (is_blackjack(dealer_hand)) {
          insurance_paid <- insurance_bet * 2
          players$Player1$coins <- players$Player1$coins + insurance_paid
          insurance_wins <- insurance_wins + 1
        } else {
          insurance_loses <- insurance_loses + 1
        }
      }
    }

    # dealer blackjack, judging winner (insurance already handled)
    if (is_blackjack(dealer_hand)) {
      lose <- lose + 1
      players$Player1$coins <- players$Player1$coins - bet_amount
      bankroll_history[i + 1] <- players$Player1$coins
      next
    }

    # handling split(Only supports one first-hand split, recursively scalable)
    split_done <- FALSE
    hand_queue <- player_hands
    hand_results <- character(0)
    while (length(hand_queue) > 0) {
      hand <- hand_queue[[1]]
      hand_queue <- hand_queue[-1]

      # Determine whether it can be split
      if (!split_done && length(hand$cards) == 2 && split_func(hand)) {
        split_count <- split_count + 1
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

      # automatically hit/stand process
      repeat {
        score <- calculate_score(hand)
        if (length(hand$cards) >= 5 && score <= 21) break
        if (score < threshold) {
          if (length(deck) < 1) break
          hand <- new_blackjack_hand(c(hand$cards, deck[1]))
          deck <- deck[-1]
        } else {
          break
        }
      }

      # dealer action
      dealer_res <- dealer_action(dealer_hand, deck)
      dealer_hand <- dealer_res$hand

      # judging
      result <- determine_winner(hand, dealer_hand)
      hand_results <- c(hand_results, result)

      if (split_count > 0) {
        if (grepl("^Player wins", result)) split_win <- split_win + 1
        if (grepl("^Dealer wins", result)) split_lose <- split_lose + 1
      }
    }

    # single round settlement(win/lose/tie)
    if (any(grepl("^Player wins", hand_results))) {
      win <- win + 1
      players$Player1$coins <- players$Player1$coins + bet_amount
    } else if (all(grepl("^Dealer wins", hand_results))) {
      lose <- lose + 1
      players$Player1$coins <- players$Player1$coins - bet_amount
    } else {
      tie <- tie + 1
    }
    bankroll_history[i + 1] <- players$Player1$coins
  }

  list(
    win_rate = win / num_rounds,
    lose_rate = lose / num_rounds,
    tie_rate = tie / num_rounds,
    insurance_win_rate = insurance_wins / (insurance_wins + insurance_loses + 1e-9),
    split_rate = split_count / num_rounds,
    split_win = split_win,
    split_lose = split_lose,
    final_coins = players$Player1$coins,
    bankroll_history = bankroll_history
  )
}
