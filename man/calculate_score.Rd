% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_cal.R
\name{calculate_score}
\alias{calculate_score}
\title{Calculate Blackjack Score}
\usage{
calculate_score(hand)
}
\arguments{
\item{hand}{A \code{blackjack_hand} object.}
}
\value{
Integer. Total blackjack score for the hand.
}
\description{
Calculates the total point value of a blackjack hand using a fast C++ backend.
}
\examples{
hand <- new_blackjack_hand(c("A♠", "10♦"))
calculate_score(hand)   # 21
hand2 <- new_blackjack_hand(c("7♣", "8♦", "6♥"))
calculate_score(hand2)  # 21
}
