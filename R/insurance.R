#' @title Handle insurance and dealer blackjack check
#' @description Offers insurance if the dealer shows an Ace, and checks if the dealer has Blackjack when showing a 10-value card.
#' @param dealer_hand A blackjack_hand object.
#' @param players A named list of player info (must include player name and coins).
#' @return A list with two elements:
#'   \describe{
#'     \item{dealer_blackjack}{Logical indicating if the dealer had Blackjack}
#'     \item{players}{Updated list with possible insurance changes}
#'   }
#' @export
handle_insurance <- function(dealer_hand, players, ai_prob = 0.5, input_fun = readline) {

  dealer_has_blackjack <- is_blackjack(dealer_hand)

  if (!needs_insurance(dealer_hand)) {
    return(list(dealer_blackjack = dealer_has_blackjack, players = players)) # No insurance needed
  }

    for (name in names(players)) {
      player <- players[[name]]
      insurance_bet <- player$bets / 2

      if (isTRUE(player$is_computer)) {
        buy <- ai_decide_insurance(
          coins         = player$coins,
          insurance_bet = insurance_bet,
          prob_buy      = ai_prob
        )
        if (buy) {
          # Deduct bets and settle wins and losses
          res <- resolve_insurance(player$coins, insurance_bet, dealer_has_blackjack)
          players[[name]]$coins        <- res$coins
          players[[name]]$insurance_bet <- insurance_bet
          if (dealer_has_blackjack) {
            cat("\u2714\ufe0f", name, " (computer): Insurance wins ", res$coins - player$coins,
                " coins!\n", sep = "")
          } else {
            cat("\u274c", name, " (computer): Insurance loses.\n", sep = "")
          }
        } else {
          cat(name, " (computer) chooses NOT to buy insurance.\n")
        }
      } else {
        # Human player input
        buy <- ask_human_insurance(
          name          = name,
          coins         = player$coins,
          insurance_bet = insurance_bet,
          input_fun     = input_fun
          )
        if (buy) {
          res <- resolve_insurance(player$coins, insurance_bet, dealer_has_blackjack)
          players[[name]]$coins        <- res$coins
          players[[name]]$insurance_bet <- insurance_bet
          if (dealer_has_blackjack) {
            cat("\u2714\ufe0f", name, ": Insurance wins ", res$coins - player$coins,
                " coins!\n", sep = "")
          } else {
            cat("\u274c", name, ": Insurance loses.\n", sep = "")
          }
        }
        # If the player chooses not to buy insurance, skip the insurance logic
      }
    }

  #  if dealer_upcard fall in 10 point, dealer with Blackjack, show dealer hand
  upcard_rank <- vctrs::field(dealer_hand, "cards")[1]
  if (upcard_rank %in% c("10", "J", "Q", "K") && dealer_has_blackjack) {
    cat("\u26a0\ufe0f Dealer has Blackjack with a 10-value upcard!\n")
    all_cards <- paste0(
      card_suit(dealer_hand$cards),
      card_rank(dealer_hand$cards)
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
