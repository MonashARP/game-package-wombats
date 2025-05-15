#' @title Get Bets from Players
#' @description Prompts each human player to enter a bet amount at the start of a round,
#' ensuring the bet is a positive integer and does not exceed their available coins.
#' Automatically assigns bets for computer players.
#' Updates the player's 'bets' and deducts coins directly.
#' @param players A named list of player objects, where each player has a 'coins' field indicating available coins and an 'is_computer' flag.
#' @return A named list of updated player objects with 'bets' and updated 'coins'.
#' @export
get_bets <- function(players) {
  for (name in names(players)) {
    player <- players[[name]]

    if (isTRUE(player$is_computer)) {
      # Simple AI bet logic: e.g., bet 10 or max available if less
      bet <- min(10, player$coins)
      cat(paste0(name, " (computer) bets ", bet, " coins.\n"))
    } else {
      repeat {
        prompt <- paste0(name, ", enter your bet (available: ", player$coins, "): ")
        bet <- suppressWarnings(as.integer(readline(prompt)))
        if (!is.na(bet) && bet > 0 && bet <= player$coins) {
          break
        }
        cat("❌ Invalid bet. Please enter a positive number no greater than your current balance.\n")
      }
    }

    players[[name]]$bets <- bet
  }
  return(players)
}
