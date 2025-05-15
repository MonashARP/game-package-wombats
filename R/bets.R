#' @title Get Bets from Players
#' @description Prompts each player to enter a bet amount at the start of a round,
#' ensuring the bet is a positive integer and does not exceed their available money.
#' @param players A named list of player objects, where each player has a `money` field indicating available coins.
#' @return A named list of numeric bets keyed by player names.
#' @export

get_bets <- function(players) {
  bets <- list()
  for (name in names(players)) {
    repeat {
      prompt <- paste0(name, ", enter your bet (available: ", players[[name]]$money, "): ")
      bet <- suppressWarnings(as.integer(readline(prompt)))
      if (!is.na(bet) && bet > 0 && bet <= players[[name]]$money) {
        bets[[name]] <- bet
        break
      }
      cat("Invalid bet. Please enter a positive number no greater than your current balance.\n")
    }
  }
  return(bets)
}
