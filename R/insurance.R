# R/Insurance.R
# Core insurance functions for handling insurance logic for human and AI players in a Blackjack game.

#' @title Check if insurance should be offered
#' @description Returns TRUE if the dealer's upcard is an Ace (i.e., insurance should be offered).
#' @param dealer_hand A \code{blackjack_hand} object representing the dealer's hand.
#' @return Logical. TRUE if insurance is offered, otherwise FALSE.
#' @keywords internal
#' @noRd
needs_insurance <- function(dealer_hand) {
  upcard_rank <- card_rank(dealer_hand$cards)[1]
  identical(upcard_rank, "A")
}

#' @title AI insurance decision logic
#' @description Determines if a computer player buys insurance, using available coins and probability.
#' @param coins Numeric. Current coins of the player.
#' @param insurance_bet Numeric. Insurance bet amount.
#' @param prob_buy Numeric, probability that AI will buy insurance (0~1).
#' @return Logical. TRUE if insurance is bought, FALSE otherwise.
#' @keywords internal
#' @noRd
ai_decide_insurance <- function(coins, insurance_bet, prob_buy = 0.5) {
  if (coins < insurance_bet) return(FALSE)
  runif(1) < prob_buy
}

#' @title Prompt human for insurance decision
#' @description Prompts a human player to buy insurance if eligible.
#' @param name Character. The player's name.
#' @param coins Numeric. Player's available coins.
#' @param insurance_bet Numeric. Insurance bet amount.
#' @param input_fun Function for input (default: readline); useful for testing/mocking.
#' @return Logical. TRUE if the player buys insurance, FALSE otherwise.
#' @keywords internal
#' @noRd
ask_human_insurance <- function(name, coins, insurance_bet, input_fun = readline) {
  if (coins < insurance_bet) {
    cat(name, "does not have enough coins for insurance bet.\n")
    return(FALSE)
  }
  repeat {
    prompt <- paste0("Dealer shows A. ", name, ", buy insurance for ", insurance_bet, " coins? (yes/no): ")
    ans <- tolower(input_fun(prompt))
    if (ans %in% c("yes", "no")) return(ans == "yes")
    cat("Invalid input. Please enter 'yes' or 'no'.\n")
  }
}

#' @title Insurance settlement logic
#' @description Computes new coins and payout after an insurance bet is resolved.
#' @param coins Numeric. Original coins before insurance bet.
#' @param insurance_bet Numeric. Insurance bet size.
#' @param dealer_has_blackjack Logical. TRUE if dealer has blackjack, else FALSE.
#' @return A list with two elements:
#'   \describe{
#'     \item{coins}{Player's coins after settlement.}
#'     \item{payout}{Insurance payout amount (0 or 2×bet).}
#'   }
#' @keywords internal
#' @noRd
resolve_insurance <- function(coins, insurance_bet, dealer_has_blackjack) {
coins_new <- coins - insurance_bet
payout <- if (dealer_has_blackjack) insurance_bet * 2 else 0
coins_new <- coins_new + payout
list(coins = coins_new, payout = payout)
}

#' @title Handle insurance offers and dealer blackjack check
#' @description
#' Offers insurance to all players if the dealer's upcard is an Ace,
#' and settles insurance bets depending on whether the dealer has a Blackjack.
#' Also reveals the dealer's hand if the upcard is a 10-value card and the dealer has Blackjack.
#' @param dealer_hand A \code{blackjack_hand} object for the dealer.
#' @param players Named list of players (each with is_computer, coins, bets, etc.).
#' @param input_fun Function for user input (default: readline).
#' @param ai_prob Numeric, probability of AI buying insurance (default 0.5).
#' @return A list with:
#'   \describe{
#'     \item{dealer_blackjack}{Logical: whether the dealer has Blackjack.}
#'     \item{players}{Updated player list after insurance resolution.}
#'   }
#' @examples
#' \dontrun{
#' # Example 1: Dealer shows Ace, both human and computer players
#' players <- list(
#'   Alice = list(is_computer = FALSE, coins = 1000, bets = 100),
#'   Bot = list(is_computer = TRUE, coins = 1000, bets = 100)
#' )
#' dealer_hand <- new_blackjack_hand(c("A♠", "10♦"))  # Dealer has Blackjack
#' # Both players will be offered insurance. Try typing "yes" or "no" when prompted.
#' res <- handle_insurance(dealer_hand, players)
#' print(res)
#'
#' # Example 2: Dealer shows Ace, no Blackjack
#' dealer_hand <- new_blackjack_hand(c("A♠", "5♦"))
#' # Insurance will be offered, but dealer does not have Blackjack.
#' res <- handle_insurance(dealer_hand, players)
#' print(res)
#'
#' # Example 3: Dealer does NOT show Ace, insurance is not offered
#' dealer_hand <- new_blackjack_hand(c("9♣", "A♦"))
#' # No insurance prompt; function just returns dealer_blackjack and updated players
#' res <- handle_insurance(dealer_hand, players)
#' print(res)
#'
#' # Example 4: Use a custom AI insurance probability
#' res <- handle_insurance(dealer_hand, players, ai_prob = 0.9)
#'
#' # The output list has two elements:
#' # $dealer_blackjack - TRUE if dealer has blackjack, FALSE otherwise
#' # $players          - player info after insurance (coins may change)
#' }
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

  # If the dealer's open hand is 10/J/Q/K and it is truly a Blackjack, show the full hand of cards
  dealer_upcard_rank <- card_rank(dealer_hand$cards)[1]
  if (dealer_upcard_rank %in% c("10", "J", "Q", "K") && dealer_has_blackjack) {
    cat("\u26a0\ufe0f Dealer has Blackjack with a 10-value upcard!\n")
    all_cards <- paste0(card_rank(dealer_hand$cards), card_suit(dealer_hand$cards))
    cat("Dealer hand: [", paste(all_cards, collapse = ", "), "]\n")
  }

  return(list(dealer_blackjack = dealer_has_blackjack, players = players))
}
