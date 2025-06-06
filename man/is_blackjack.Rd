% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hand_type.R
\name{is_blackjack}
\alias{is_blackjack}
\title{Check if a hand is Blackjack}
\usage{
is_blackjack(hand)
}
\arguments{
\item{hand}{A `blackjack_hand` object}
}
\value{
TRUE if Blackjack, else FALSE
}
\description{
Returns TRUE if the hand has exactly 2 cards and total is 21
}
\examples{
# Example: Blackjack (Ace + King)
hand <- new_blackjack_hand(c("A♠", "K♦"))
is_blackjack(hand)
}
