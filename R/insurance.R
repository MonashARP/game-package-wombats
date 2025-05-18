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
handle_insurance <- function(dealer_hand, players) {
  dealer_upcard <- vctrs::field(dealer_hand, "cards")[1]
  dealer_has_blackjack <- is_blackjack(dealer_hand)

  if (dealer_upcard == "A") {
    for (name in names(players)) {
      player <- players[[name]]
      insurance_bet <- player$bets / 2

      if (isTRUE(player$is_computer)) {
        # Simple AI decision: buy insurance if coins enough and 50% chance
        buy_insurance <- FALSE
        if (player$coins >= insurance_bet) {
          buy_insurance <- sample(c(TRUE, FALSE), 1)
        }

        if (buy_insurance) {
          players[[name]]$coins <- players[[name]]$coins - insurance_bet
          players[[name]]$insurance_bet <- insurance_bet
          if (dealer_has_blackjack) {
            payout <- insurance_bet * 2
            players[[name]]$coins <- players[[name]]$coins + payout
            cat("\u2714\ufe0f", name, " (computer): Dealer has Blackjack. Insurance bet wins ", payout, " coins!\n", sep = "")
          } else {
            cat("\u274c", name, " (computer): Dealer does not have Blackjack. Insurance bet lost.\n", sep = "")
          }
        } else {
          cat(name, " (computer) chooses NOT to buy insurance.\n")
        }
      } else {
        # Human player input
        repeat {
          ins <- tolower(readline(paste0("Dealer shows A. ", name, ", buy insurance? (yes/no): ")))
          if (ins %in% c("yes", "no")) break
          cat("Invalid input. Please enter 'yes' or 'no'.\n")
        }

        if (ins == "yes") {
          if (player$coins < insurance_bet) {
            cat("You do not have enough coins for insurance bet.\n")
            next
          }
          players[[name]]$coins <- players[[name]]$coins - insurance_bet
          players[[name]]$insurance_bet <- insurance_bet

          if (dealer_has_blackjack) {
            payout <- insurance_bet * 2
            players[[name]]$coins <- players[[name]]$coins + payout
            cat("\u2714\ufe0f", name, ": Dealer has Blackjack. Insurance bet wins ", payout, " coins!\n", sep = "")
          } else {
            cat("\u274c", name, ": Dealer does not have Blackjack. Insurance bet lost.\n", sep = "")
          }
        }
      }
    }
  }

  if (dealer_upcard %in% c("10", "J", "Q", "K")) {
    if (dealer_has_blackjack) {
      cat("\u26a0\ufe0f Dealer has Blackjack with a 10-value upcard!\n")
      cat("Dealer hand: [", paste(vctrs::field(dealer_hand, "cards"), collapse = ", "), "]\n")
    }
  }

  return(list(dealer_blackjack = dealer_has_blackjack, players = players))
}
