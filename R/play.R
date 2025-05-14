#' @title Play interactive Blackjack (full version)
#' @description Supports insurance, split with both hands played, double down, and win logic
#' @export
play <- function() {
  repeat {
    deck <- sample(rep(c("A","2","3","4","5","6","7","8","9","10","J","Q","K"), 4))
    deal <- deal_cards(deck)
    player_hand <- deal$player_hand
    dealer_hand <- deal$dealer_hand

    used_cards <- c(vctrs::field(player_hand, "cards"), vctrs::field(dealer_hand, "cards"))
    deck <- setdiff(deck, used_cards)

    cat("\n=== Blackjack ===\n")
    cat("Your hand: [", paste(vctrs::field(player_hand, "cards"), collapse = ", "), "]\n")
    cat("Dealer shows: [", vctrs::field(dealer_hand, "cards")[1], ", ? ]\n")

    #️ Insurance
    if (vctrs::field(dealer_hand, "cards")[1] == "A") {
      ins <- tolower(readline("Dealer shows A. Buy insurance? (yes/no): "))
      if (ins == "yes") {
        if (is_blackjack(dealer_hand)) {
          cat("✔️ Dealer has Blackjack. Your insurance bet wins!\n")
        } else {
          cat("❌ Dealer does not have Blackjack. Insurance bet lost.\n")
        }
      }
    }

    #  Split check
    player_hands <- list()
    ph <- vctrs::field(player_hand, "cards")
    if (length(ph) == 2 && ph[1] == ph[2]) {
      sp <- tolower(readline("You have a pair. Split hand? (yes/no): "))
      if (sp == "yes") {
        first <- new_blackjack_hand(c(ph[1], deck[1]))
        second <- new_blackjack_hand(c(ph[2], deck[2]))
        deck <- deck[-c(1,2)]
        player_hands <- list(first, second)
        cat("=== Split ===\n")
        cat("First hand: [", paste(vctrs::field(first, "cards"), collapse = ", "), "]\n")
        cat("Second hand: [", paste(vctrs::field(second, "cards"), collapse = ", "), "]\n")
      }
    }

    # if not split
    if (length(player_hands) == 0) {
      player_hands <- list(player_hand)
    }

    #  Play each hand separately
    for (i in seq_along(player_hands)) {
      cat(paste0("\n--- Playing Hand ", i, " ---\n"))
      hand <- player_hands[[i]]
      first_move <- TRUE

      repeat {
        prompt_msg <- if (first_move) {
          "Hit, Stand or Double? (hit/stand/double/exit): "
        } else {
          "Hit or Stand? (hit/stand/exit): "
        }
        action <- tolower(readline(prompt = prompt_msg))

        if (action == "exit") {
          cat("Goodbye!\n")
          return(invisible(NULL))
        }

        valid_actions <- if (first_move) c("hit", "stand", "double") else c("hit", "stand")
        if (!action %in% valid_actions) {
          cat("Invalid input.\n")
          next
        }

        res <- player_action(hand, deck, action)
        hand <- res$hand
        deck <- res$deck
        score <- calculate_score(hand)

        cat("You now have: [", paste(vctrs::field(hand, "cards"), collapse = ", "), "]\n")
        cat("Score:", score, "\n")

        if (action == "double") {
          cat("You chose to double down: one card only.\n")
          break
        }
        if (action == "stand" || score > 21) break

        first_move <- FALSE
      }

      player_hands[[i]] <- hand
    }

    # Dealer plays once (only if no player busts)
    all_scores <- purrr::map_dbl(player_hands, calculate_score)
    if (any(all_scores <= 21)) {
      dealer_res <- dealer_action(dealer_hand, deck)
      dealer_hand <- dealer_res$hand
    }

    # Final results
    outcome <- determine_winner(player_hands, dealer_hand)

    cat("\n=== Final Results ===\n")
    for (i in seq_along(player_hands)) {
      ph <- player_hands[[i]]
      p_score <- calculate_score(ph)
      hand_label <- paste0("Hand ", i)
      cat(hand_label, ": [", paste(vctrs::field(ph, "cards"), collapse = ", "), "]  Score:", p_score, "\n")
      if (is_blackjack(ph)) cat(" 🎉 Blackjack!\n")
      if (is_five_card_charlie(ph)) cat(" 💫 5-card Charlie!\n")
      cat(" → ", outcome[i], "\n\n")
    }

    # Dealer info
    cat("Dealer hand: [", paste(vctrs::field(dealer_hand, "cards"), collapse = ", "), "], Score:", calculate_score(dealer_hand), "\n")

    # Play again?
    repeat {
      again <- tolower(readline(prompt = "\nType 'again' to play another game, or 'exit' to quit: "))
      if (again == "again") break
      if (again == "exit") {
        cat("Goodbye!\n")
        return(invisible(NULL))
      }
      cat("Invalid input. Please type 'again' or 'exit'.\n")
    }
  }
}

