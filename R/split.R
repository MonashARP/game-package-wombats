#' @title Handle splitting
#' @description Allows players with pairs to split their hand.
#' @param player_hands A list of lists of blackjack_hand objects.
#' @param deck A character vector representing the remaining deck.
#' @param players A named list of player objects (including names and is_computer flag).
#' @return An updated list of lists of blackjack_hand objects after potential splits.
#' @export

handle_splitting <- function8(player_hands, deck, players) {
  updated_player_hands <- player_hands
  deck_index <- 1
  player_names <- names(players)

  for (i in seq_along(updated_player_hands)) {
    hand_list <- updated_player_hands[[i]]
    hand      <- hand_list[[1]]  # assume first hand is the main one
    player_name <- names_list[i]
    is_cp <- players[[player_name]]$is_computer

    if (needs_split(hand)) {
      do_split <- FALSE
      if (is_cp) {
        do_split <- ai_decide_split(hand) # AI logic to decide split
        if (do_split) {
          cat(player_name, "decides to split a pair of ",
              vctrs::field(hand, "rank")[1], "s.\n", sep = "")
        } else {
          cat(player_name, "decides NOT to split a pair of ",
              vctrs::field(hand, "rank")[1], "s.\n", sep = "")
         }
        } else {
        do_split <- ask_human_split(player_name, hand) # Human decision
      }

      if (do_split) {
        res <- perform_split(hand, deck, deck_index)
        updated_player_hands[[i]] <- res$split_hands
        deck_index <- res$new_deck_index

        cat(player_name, "split into:\n")
        # Hand 1
        f1 <- res$split_hands[[1]]
        f1_r <- vctrs::field(f1, "rank")
        f1_s <- vctrs::field(f1, "suit")
        f1_disp <- paste0(f1_s, f1_r)
        cat("  Hand 1: [", paste(f1_disp, collapse = " "), "]\n", sep = "")
        # Hand 2
        f2 <- res$split_hands[[2]]
        f2_r <- vctrs::field(f2, "rank")
        f2_s <- vctrs::field(f2, "suit")
        f2_disp <- paste0(f2_s, f2_r)
        cat("  Hand 2: [", paste(f2_disp, collapse = " "), "]\n", sep = "")
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
  ranks <- vctrs::field(hand, "rank")
  length(ranks) == 2 && ranks[1] == ranks[2]
}

# AI decision: split if 8 or A in hand, otherwise no split
#' @noRd
ai_decide_split <- function(hand, coins = NULL) {
  ranks <- vctrs::field(hand, "rank")
  rank <- ranks[1]
  rank %in% c("8", "A")
}

# ask human whether to split
#' @noRd
ask_human_split <- function(name, hand) {
  ranks <- vctrs::field(hand, "rank")
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
