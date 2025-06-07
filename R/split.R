# R/split.R

#' @importFrom stats runif
#' @importFrom utils modifyList
NULL

#' @title Handle splitting of pairs
#' @description
#' Allows eligible players (pair of same rank) to split their hand. Handles human/AI decision and draws new cards for each split.
#' @param player_hands A named list of lists of \code{blackjack_hand} objects (hands for each player).
#' @param deck Character vector: the remaining deck.
#' @param players Named list of player objects (must include player name and is_computer).
#' @return A list with:
#'   \describe{
#'     \item{player_hands}{Updated list of player hands (split if chosen).}
#'     \item{deck_index}{Updated index for next available card in the deck.}
#'   }
#' @examples
#' \dontrun{
#' players <- list(
#'   Alice = list(is_computer = FALSE),
#'   Bot = list(is_computer = TRUE)
#' )
#' hands <- list(
#'   Alice = list(new_blackjack_hand(c("8♠", "8♦"))),
#'   Bot = list(new_blackjack_hand(c("A♣", "A♥")))
#' )
#' deck <- c("10♣", "7♠", "6♥", "5♦")
#' handle_splitting(hands, deck, players)
#' }
#' @export
handle_splitting <- function(player_hands, deck, players) {

  if (!is.list(player_hands) || length(player_hands) == 0) {
    stop("player_hands must be a non-empty named list.")
  }

  if (length(deck) == 0) {
    stop("Deck must not be empty.")
  }

  if (!is.list(players) || length(players) == 0) {
    stop("players must be a non-empty named list.")
  }

  updated_player_hands <- player_hands
  deck_index <- 1

  for (player_name in names(updated_player_hands)) {
    hand_list <- updated_player_hands[[player_name]]

    # Force wrap hands as blackjack_hand if needed

    hand_list <- lapply(hand_list, ensure_blackjack_hand)
    hand <- hand_list[[1]]  # only check the first (main) hand
    is_cp <- players[[player_name]]$is_computer

    if (needs_split(hand)) {
      do_split <- FALSE
      rank <- card_rank(hand$cards)

      if (is_cp) {
        do_split <- ai_decide_split(hand)
        cat(player_name, if (do_split) "decides to split" else "decides NOT to split",
            "a pair of ", rank, "s.\n", sep = " ")
      } else {
        do_split <- ask_human_split(player_name, hand)
      }

      if (do_split) {
        res <- perform_split(hand, deck, deck_index)
        updated_player_hands[[player_name]] <- res$split_hands
        deck_index <- res$new_deck_index

        cat(player_name, "split into:\n")
        for (k in seq_along(res$split_hands)) {
          f <- res$split_hands[[k]]
          cat("  Hand ", k, ": [", paste(f$cards, collapse = " "), "]\n", sep = "")
        }
      }
    }
  }

  list(
    player_hands = updated_player_hands,
    deck_index   = deck_index
  )
}

#' @title Check if a hand can be split
#' @description Returns TRUE if the hand contains exactly two cards of the same rank.
#' @param hand A \code{blackjack_hand} object (or card vector of length 2).
#' @return Logical: TRUE if eligible for split, otherwise FALSE.
#' @keywords internal
#' @noRd
needs_split <- function(hand) {
  ranks <- card_rank(hand$cards)
  length(ranks) == 2 && ranks[1] == ranks[2]
}

#' @title AI split decision logic
#' @description Returns TRUE if AI should split a pair (e.g., only for 8 or A).
#' @param hand A \code{blackjack_hand} or card vector.
#' @param coins Optional. Player coins (not always used).
#' @return Logical: TRUE to split, FALSE otherwise.
#' @keywords internal
#' @noRd
ai_decide_split <- function(hand) {
  ranks <- card_rank(hand$cards)
  ranks[1] %in% c("8", "A")
}

#' @title Prompt human for split decision
#' @description Asks a human player if they want to split a pair, via console input.
#' @param name Character. Player name.
#' @param hand A \code{blackjack_hand} or card vector.
#' @return Logical: TRUE if player chooses to split, otherwise FALSE.
#' @keywords internal
#' @noRd
ask_human_split <- function(player_name, hand) {
  rank <- card_rank(hand$cards)[1]
  repeat {
    prompt <- paste0(
      player_name, ", you have a pair of ", rank, "s. Split hand? (yes/no): "
    )
    ans <- tolower(readline(prompt))
    if (ans %in% c("yes", "no")) return(ans == "yes")
    cat("Invalid input. Please enter 'yes' or 'no'.\n")
  }
}

#' @title Perform a split operation
#' @description
#' Given a pair, splits it into two new hands and draws a card for each.
#' @param hand A \code{blackjack_hand} or card vector of length 2.
#' @param deck Character vector: the deck to draw from.
#' @param deck_index Integer: next card(s) to draw.
#' @return List with:
#'   \describe{
#'     \item{split_hands}{List of two new \code{blackjack_hand} objects.}
#'     \item{new_deck_index}{Integer, index after the split.}
#'   }
#' @keywords internal
#' @noRd
perform_split <- function(hand, deck, deck_index) {
  # hand: blackjack_hand of two cards
  cards <- hand$cards
  new1 <- c(cards[1], deck[deck_index])
  new2 <- c(cards[2], deck[deck_index + 1])
  hand1 <- new_blackjack_hand(new1)
  hand2 <- new_blackjack_hand(new2)
  list(
    split_hands = list(hand1, hand2),
    new_deck_index = deck_index + 2
  )
}
