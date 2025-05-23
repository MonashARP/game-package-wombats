% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deal.R
\name{deal_cards}
\alias{deal_cards}
\title{Deal initial cards in Blackjack}
\usage{
deal_cards(deck = NULL, num_players = 1)
}
\arguments{
\item{deck}{A character vector representing the deck. If NULL, a fresh 52-card deck is used.}

\item{num_players}{An integer (default = 1). Number of players to deal cards to.}
}
\value{
A list with `player_hands` (a list of blackjack_hand objects) and `dealer_hand`.
}
\description{
Deal two cards to the player and dealer each, from a shuffled deck.
}
