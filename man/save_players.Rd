% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{save_players}
\alias{save_players}
\title{Save players and bankroll history to disk}
\usage{
save_players(players, bankroll_history = NULL)
}
\arguments{
\item{players}{Named list of players.}

\item{bankroll_history}{Optional. A list; each element is a named numeric vector of player coins per round.}
}
\value{
Invisibly returns NULL.
}
\description{
Saves the player list and optional bankroll history to a file in the user's home directory.
}
\examples{
save_players(list(Alice = list(coins = 1000)), list())
}
