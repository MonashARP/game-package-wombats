#' @title Handle insurance for each player
#' @description Offers insurance to each player if the dealer shows an Ace and evaluates the outcome.
#' @param dealer_hand A blackjack_hand object.
#' @param players A named list of player info (must include player name and money).
#' @return A list with two elements:
#'   \describe{
#'     \item{dealer_blackjack}{Logical indicating if the dealer had Blackjack}
#'     \item{players}{Updated list with possible insurance changes (if implemented)}
#'   }
#' @export
handle_insurance <- function(dealer_hand, players) {
  dealer_upcard <- vctrs::field(dealer_hand, "cards")[1]

  if (dealer_upcard != "A") {
    return(list(dealer_blackjack = is_blackjack(dealer_hand), players = players))
  }

  dealer_has_blackjack <- is_blackjack(dealer_hand)

  for (name in names(players)) {
    repeat {
      ins <- tolower(readline(paste0("Dealer shows A. ", name, ", buy insurance? (yes/no): ")))
      if (ins %in% c("yes", "no")) break
      cat("Invalid input. Please enter 'yes' or 'no'.\n")
    }

    if (ins == "yes") {
      if (dealer_has_blackjack) {
        cat("✔️ ", name, ": Dealer has Blackjack. Insurance bet wins!\n", sep = "")
        # Optionally award insurance payout here
      } else {
        cat("❌ ", name, ": Dealer does not have Blackjack. Insurance bet lost.\n", sep = "")
        # Optionally deduct insurance bet here
      }
    }
  }

  if (dealer_has_blackjack) {
    cat("⚠️ Dealer has Blackjack!\n")
    cat("Dealer hand: [", paste(vctrs::field(dealer_hand, "cards"), collapse = ", "), "]\n")
  }

  return(list(dealer_blackjack = dealer_has_blackjack, players = players))
}
