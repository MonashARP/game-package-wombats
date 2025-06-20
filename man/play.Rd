% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/game_loop.R
\name{play}
\alias{play}
\title{Play a Full Interactive Game of Blackjack}
\usage{
play()
}
\description{
Launches an interactive multiplayer game of Blackjack with full game mechanics.
The game supports up to six players (human or computer), betting, doubling down,
splitting hands, insurance, and dealer logic. Player data (names and coin balances)
is saved between sessions.

Features include:
- Computer-controlled dealer and players
- Coin betting system with balance persistence
- Support for Blackjack rules: insurance, double down, and split
- Persistent storage of player profiles and bankroll history
}
\details{
This is the main function to run the full-featured Blackjack game. The game is played
in rounds. After each round, players are asked if they wish to continue. The game
loop will end if no players wish to continue or a player exits early.
#' @return No return value. This function is run for its side effects: interactive gameplay,
screen output, and updates to stored player data.
}
\examples{
\dontrun{
# Start a new interactive Blackjack session
play()

# Example: Playing with custom initial player names and coin balances
# (Modify player list before starting)
players_db <- list(
  Alice = list(coins = 1500, is_computer = FALSE),
  Bob   = list(coins = 1200, is_computer = FALSE),
  Dealer = list(is_computer = TRUE)
)
bankroll_history <- list()
save_players(players_db, bankroll_history)
play()  # Players will be loaded with these balances

# Example: Add computer opponents for more challenge
# When prompted, set number of computer players (e.g., 2)
play()

# The game will guide you through betting, turns, splitting, doubling down, and insurance.
# Play multiple rounds; balances and player info are saved automatically.
}

}
