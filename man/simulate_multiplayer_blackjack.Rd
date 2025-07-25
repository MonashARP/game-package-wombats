% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{simulate_multiplayer_blackjack}
\alias{simulate_multiplayer_blackjack}
\title{Simulate Blackjack games with multiple players (up to 6)}
\usage{
simulate_multiplayer_blackjack(
  num_rounds = 10000,
  num_players = 2,
  bet_amount = 10,
  initial_coins = 1000,
  threshold = 17,
  buy_insurance = TRUE,
  split = FALSE
)
}
\arguments{
\item{num_rounds}{Number of games to simulate}

\item{num_players}{Number of players (1-6)}

\item{bet_amount}{Fixed bet per round (can be vector for different bets per player)}

\item{initial_coins}{Starting coins for players (can be vector for different amounts)}

\item{threshold}{Hit/stand threshold for each player (can be vector)}

\item{buy_insurance}{Insurance strategy for each player (can be vector of logical/functions)}

\item{split}{Split strategy for each player (can be vector of logical/functions)}
}
\value{
A list containing results for each player and overall game statistics
}
\description{
Simulate multiple rounds of Blackjack with 1-6 players using fully automated AI logic.
Each player can have different strategies for hit/stand, insurance, and splitting.
}
\examples{
# 3 players with same strategy
res <- simulate_multiplayer_blackjack(
  num_rounds = 1000,
  num_players = 3,
  threshold = 17,
  buy_insurance = TRUE,
  split = FALSE
)

# 3 players with different strategies
res <- simulate_multiplayer_blackjack(
  num_rounds = 1000,
  num_players = 3,
  threshold = c(17, 18, 16),
  buy_insurance = c(TRUE, FALSE, TRUE),
  split = c(FALSE, TRUE, FALSE),
  bet_amount = c(10, 20, 15),
  initial_coins = c(1000, 1500, 800)
)
}
