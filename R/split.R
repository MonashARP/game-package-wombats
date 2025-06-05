# R/split.R

#' @noRd
#' Check if a hand can be split (must have exactly two cards of the same rank)
needs_split <- function(hand) {
  ranks <- get_rank(hand$cards)
  length(ranks) == 2 && ranks[1] == ranks[2]
}

#' @noRd
#' AI decision to split: split only if pair is 8 or A
ai_decide_split <- function(hand) {
  ranks <- get_rank(hand$cards)
  ranks[1] %in% c("8", "A")
}

#' @noRd
#' Ask human player whether to split
ask_human_split <- function(player_name, hand) {
  rank <- get_rank(hand$cards)[1]
  repeat {
    prompt <- paste0(
      player_name, ", you have a pair of ", rank, "s. Split hand? (yes/no): "
    )
    ans <- tolower(readline(prompt))
    if (ans %in% c("yes", "no")) return(ans == "yes")
    cat("Invalid input. Please enter 'yes' or 'no'.\n")
  }
}

#' @noRd
#' Perform the actual split operation and draw one card for each new hand
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

#' @title Handle splitting
#' @description Allows players with pairs to split their hand.
#' @param player_hands A list of lists of blackjack_hand objects.
#' @param deck A character vector representing the remaining deck.
#' @param players A named list of player objects (including names and is_computer flag).
#' @return An updated list of lists of blackjack_hand objects after potential splits.
#' @export

#' @noRd
#' Handle all splitting logic for this round (for both human and AI)
#' Each hand is assumed to be a blackjack_hand object with "point+symbol" cards.
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
      rank <- get_rank(hand$cards)

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



# Check split condition: exactly two cards in the hand with the same point value
#' @noRd
needs_split <- function(hand) {
  if (length(hand) != 2) {
    return(FALSE)  # Can't split non-pairs or already-played hands
  }
  ranks <- card_rank(hand)
  if (length(ranks) != 2) {
    warning("Unexpected rank extraction in needs_split")
    return(FALSE)
  }
  ranks[1] == ranks[2]
}

# AI decision: split if 8 or A in hand, otherwise no split
#' @noRd
ai_decide_split <- function(hand, coins = NULL) {
  ranks <- card_rank(hand)
  rank <- ranks[1]
  rank %in% c("8", "A")
}

# ask human whether to split
#' @noRd
ask_human_split <- function(name, hand) {
  ranks <- card_rank(hand)
  rank <- ranks[1]
  repeat {
    prompt <- paste0(
      name, ", you have a pair of ", rank,
      "s. Split hand? (yes/no): "
    )
    ans <- tolower(readline(prompt))
    if (ans %in% c("yes", "no")) {
      return(ans == "yes")
    }
    cat("Invalid input. Please enter 'yes' or 'no'.\n")
  }
}

# Perform the split operation
#' @noRd
perform_split <- function(hand, deck, deck_index) {
  # hand: card vsrector of length 2
  # deck_index: next two cards to draw from the deck

  if (deck_index + 1 > length(deck)) {
    stop("Not enough cards remaining in deck to complete split")
  }
  first_card  <- hand[1]
  second_card <- hand[2]
  new1 <- vctrs::vec_c(first_card, deck[deck_index])
  new2 <- vctrs::vec_c(second_card, deck[deck_index + 1])
  hand1 <- new_blackjack_hand(new1)
  hand2 <- new_blackjack_hand(new2)
  list(
    split_hands = list(hand1, hand2),
    new_deck_index = deck_index + 2
  )
}

