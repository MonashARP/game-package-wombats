% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results.R
\name{determine_winner}
\alias{determine_winner}
\title{Determine winner(s) of a Blackjack game}
\usage{
determine_winner(player_hand, dealer_hand)
}
\arguments{
\item{player_hand}{A \code{blackjack_hand} or a list of them.}

\item{dealer_hand}{Dealer's \code{blackjack_hand} object.}
}
\value{
A character outcome ("Player wins", ...) or a named vector if multiple hands.
}
\description{
Determines outcome ("Player wins", "Dealer wins", "Push", etc.) for a hand or list of hands.
}
\examples{
hand <- new_blackjack_hand(c("A♠", "K♦"))
dealer <- new_blackjack_hand(c("10♣", "8♥"))
determine_winner(hand, dealer)
}
