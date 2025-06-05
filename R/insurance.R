# R/Insurance.R

#' Whether the insurance needs to be triggered (dealer's clear card is A)
#' @noRd
needs_insurance <- function(dealer_hand) {
  upcard <- dealer_hand$cards[1]
  card_rank(upcard) == "A"
}

# AI insurance decision(adjustable probability, fixed return during testing)
#' @noRd
ai_decide_insurance <- function(coins, insurance_bet, prob_buy = 0.5) {
  if (coins < insurance_bet) return(FALSE)
  runif(1) < prob_buy
}

#' Human player input (input_fun mockable)
#' @noRd
ask_human_insurance <- function(name, coins, insurance_bet, input_fun = readline) {
  repeat {
    ins <- tolower(input_fun(paste0("Dealer shows A. ", name, ", buy insurance? (yes/no): ")))
    if (ins %in% c("yes", "no")) return(ins == "yes")
    cat("Invalid input. Please enter 'yes' or 'no'.\n")
  }
}

#' Insurance Settlement Logic
#' @noRd
resolve_insurance <- function(coins, insurance_bet, dealer_has_blackjack) {
  coins_new <- coins - insurance_bet
  payout <- if (dealer_has_blackjack) insurance_bet * 2 else 0
  coins_new <- coins_new + payout
  list(coins = coins_new, payout = payout)
}

#' @title Handle insurance and dealer blackjack check
#'
#' @description
#' Offers insurance to all players if the dealer's upcard is an Ace,
#' and settles insurance bets depending on whether the dealer has a Blackjack.
#' Also reveals the dealer's hand if the upcard is a 10-value card and the dealer has Blackjack.
#'
#' @param dealer_hand A \code{blackjack_hand} object. Should be a list containing a \code{cards} character vector,
#'   e.g., \code{cards = c("A♠", "10♦")}.
#' @param players A named list of player info. Each element should be a list that includes at least:
#'   \itemize{
#'     \item{\code{is_computer}: Logical, whether the player is controlled by the computer}
#'     \item{\code{coins}: Numeric, the current coins}
#'     \item{\code{bets}: Numeric, the current round's bet amount}
#'   }
#' @param input_fun Function to handle user input for insurance. Defaults to \code{readline}. Can be replaced for testing.
#' @param ai_prob Probability (between 0 and 1) that the computer player will buy insurance if eligible. Default is 0.5.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{dealer_blackjack}{Logical indicating if the dealer had Blackjack}
#'     \item{players}{Updated named list of players, with insurance bet results applied}
#'   }
#'
#' @details
#' - When the dealer's upcard is Ace (\code{"A♠"}, \code{"A♥"}, etc.), all players are prompted (or computers decide)
#'   whether to buy insurance for half their original bet. If the dealer does have Blackjack, the insurance pays out 2:1.
#' - If the upcard is a 10-value card (\code{"10"}, \code{"J"}, \code{"Q"}, \code{"K"}), and the dealer has Blackjack,
#'   the dealer's full hand is revealed.
#'
#' @export
handle_insurance <- function(dealer_hand, players, input_fun = readline, ai_prob = 0.5) {
  dealer_has_blackjack <- is_blackjack(dealer_hand)

  if (needs_insurance(dealer_hand)) {
    for (name in names(players)) {
      player <- players[[name]]
      insurance_bet <- player$bets / 2

      if (isTRUE(player$is_computer)) {
        buy_insurance <- ai_decide_insurance(player$coins, insurance_bet, prob_buy = ai_prob)
        if (buy_insurance) {
          res <- resolve_insurance(player$coins, insurance_bet, dealer_has_blackjack)
          players[[name]]$coins <- res$coins
          players[[name]]$insurance_bet <- insurance_bet
          if (dealer_has_blackjack) {
            cat("\u2714\ufe0f", name, " (computer): Dealer has Blackjack. Insurance bet wins ", res$payout, " coins!\n", sep = "")
          } else {
            cat("\u274c", name, " (computer): Dealer does not have Blackjack. Insurance bet lost.\n", sep = "")
          }
        } else {
          cat(name, " (computer) chooses NOT to buy insurance.\n")
        }
      } else {
        buy_insurance <- ask_human_insurance(name, player$coins, insurance_bet, input_fun)
        if (buy_insurance) {
          if (player$coins < insurance_bet) {
            cat("You do not have enough coins for insurance bet.\n")
            next
          }
          res <- resolve_insurance(player$coins, insurance_bet, dealer_has_blackjack)
          players[[name]]$coins <- res$coins
          players[[name]]$insurance_bet <- insurance_bet
          if (dealer_has_blackjack) {
            cat("\u2714\ufe0f", name, ": Dealer has Blackjack. Insurance bet wins ", res$payout, " coins!\n", sep = "")
          } else {
            cat("\u274c", name, ": Dealer does not have Blackjack. Insurance bet lost.\n", sep = "")
          }
        }
      }
    }
  }

  # If the dealer's open hand is 10/J/Q/K and it is truly a Blackjack, reveal the hand
  dealer_upcard_rank <- card_rank(dealer_hand$cards[1])
  if (dealer_upcard_rank %in% c("10", "J", "Q", "K") && dealer_has_blackjack) {
    cat("\u26a0\ufe0f Dealer has Blackjack with a 10-value upcard!\n")
    all_cards <- paste0(
      card_rank(dealer_hand$cards),
      card_suit(dealer_hand$cards)
    )
    cat("Dealer hand: [", paste(all_cards, collapse = ", "), "]\n")

  }

  return(list(dealer_blackjack = dealer_has_blackjack, players = players))
}


# Whether the insurance process needs to be triggered
#' @noRd
needs_insurance <- function(dealer_hand) {
  # dealer_hand: A card vector (with a length of at least 1)
  upcard_rank <- card_rank(dealer_hand$cards)[1]
  identical(upcard_rank, "A")
}

# AI decision function: Deciding whether to purchase insurance for the computer
#' @noRd
ai_decide_insurance <- function(coins, insurance_bet, prob_buy = 0.5) {
  if (coins < insurance_bet) {
    return(FALSE)
  }
  runif(1) < prob_buy
}

# Handle the settlement logic of an insurance bet
#' @noRd
resolve_insurance <- function(coins, insurance_bet, dealer_has_blackjack) {
  # Deduct insurance bets
  coins_new <- coins - insurance_bet
  message <- NULL

  if (dealer_has_blackjack) {
    # The insurance payout is 2:1
    payout <- insurance_bet * 2L
    coins_new <- coins_new + payout
    message <- "Insurance wins"
  } else {
    message <- "Insurance loses"
  }
  list(coins = coins_new, message = message)
}

# Whether human players purchase insurance: Separate input reading
#' @noRd
ask_human_insurance <- function(name, coins, insurance_bet, input_fun = readline) {
  if (coins < insurance_bet) {
    return(FALSE)  # bankruptcy
  }
  repeat {
    prompt <- paste0(
      "Dealer shows A. ", name,", buy insurance for ", insurance_bet, " coins? (yes/no): "
    )
    ans <- tolower(input_fun(prompt))
    if (ans %in% c("yes", "no")) {
      return(ans == "yes")
    }
    cat("Invalid input. Please enter 'yes' or 'no'.\n")
  }
}
