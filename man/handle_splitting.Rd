% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split.R
\name{handle_splitting}
\alias{handle_splitting}
\title{Handle splitting of pairs}
\usage{
handle_splitting(player_hands, deck, players)
}
\arguments{
\item{player_hands}{A named list of lists of \code{blackjack_hand} objects (hands for each player).}

\item{deck}{Character vector: the remaining deck.}

\item{players}{Named list of player objects (must include player name and is_computer).}
}
\value{
A list with:
  \describe{
    \item{player_hands}{Updated list of player hands (split if chosen).}
    \item{deck_index}{Updated index for next available card in the deck.}
  }
}
\description{
Allows eligible players (pair of same rank) to split their hand. Handles human/AI decision and draws new cards for each split.
}
\examples{
\dontrun{
# Example 1: Human and AI, both eligible for splitting
players <- list(
  Alice = list(is_computer = FALSE),
  Bot = list(is_computer = TRUE)
)
hands <- list(
  Alice = list(new_blackjack_hand(c("8♠", "8♦"))),   # Pair of 8s, human
  Bot   = list(new_blackjack_hand(c("A♣", "A♥")))    # Pair of Aces, AI
)
deck <- c("10♣", "7♠", "6♥", "5♦", "4♠", "9♣")

# Will prompt Alice whether to split, Bot will decide by AI logic
split_res <- handle_splitting(hands, deck, players)

# Check how many hands each player has after splitting
lapply(split_res$player_hands, length)

# Example 2: Only AI is eligible for splitting
players2 <- list(
  Carol = list(is_computer = FALSE),
  Bot2  = list(is_computer = TRUE)
)
hands2 <- list(
  Carol = list(new_blackjack_hand(c("10♦", "8♣"))),    # Not a pair
  Bot2  = list(new_blackjack_hand(c("8♥", "8♣")))      # Pair, AI
)
deck2 <- c("J♦", "5♠", "2♥", "4♣")
split_res2 <- handle_splitting(hands2, deck2, players2)

# Example 3: Hands not eligible for splitting (function returns unchanged hands)
players3 <- list(Dan = list(is_computer = FALSE))
hands3 <- list(Dan = list(new_blackjack_hand(c("7♦", "8♠"))))
deck3 <- c("9♦", "Q♣")
split_res3 <- handle_splitting(hands3, deck3, players3)

# Output: list with updated player_hands and deck_index after splitting
print(split_res)
}
}
