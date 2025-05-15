#' @title Input and validate human and computer players
#' @description Allows input of human player names, creates computer players, loads or creates player data with coins.
#' @return A named list of players (human + computer), where each element is a list with player info (name, money, is_computer).
#' @export

input_players <- function(players_db) {

  # Input total number of players
  repeat {
    total_players <- suppressWarnings(as.integer(readline("Enter total number of players (1–6): ")))
    if (!is.na(total_players) && total_players >= 1 && total_players <= 6) break
    cat("Invalid input. Please enter a number between 1 and 6.\n")
  }

  # Input number of computer players
  repeat {
    num_computers <- suppressWarnings(as.integer(readline(paste0("Enter number of computer players (0–", total_players, "): "))))
    if (!is.na(num_computers) && num_computers >= 0 && num_computers <= total_players) break
    cat("Invalid input. Please enter a valid number of computer players.\n")
  }

  num_humans <- total_players - num_computers
  session_players <- list()

  # Input human player names
  if (num_humans > 0) {
    for (i in seq_len(num_humans)) {
      repeat {
        name <- readline(paste0("Enter unique name for Human Player ", i, ": "))
        name <- trimws(name)

        if (name == "") {
          cat("Name cannot be empty.\n")
          next
        }
        if (name %in% names(session_players)) {
          cat("This name is already used for another player in this session. Please choose a different name.\n")
          next
        }

        if (name %in% names(players_db)) {
          repeat {
            cont <- tolower(readline(paste0("Player '", name, "' found. Continue as this player? (yes/no): ")))
            if (cont %in% c("yes", "no")) break
            cat("Invalid input. Please enter 'yes' or 'no'.\n")
          }

          if (cont == "yes") {
            cat("Welcome back, ", name, "! You have ", players_db[[name]]$coins, " coins remaining.\n", sep = "")
            session_players[[name]] <- players_db[[name]]
            session_players[[name]]$is_computer <- FALSE
            break
          } else {
            cat("Please enter a different name.\n")
            next
          }
        } else {
          # New human player with default coins
          players_db[[name]] <- list(name = name, coins = 1000, is_computer = FALSE)
          cat("New player created: ", name, " with 1000 coins.\n", sep = "")
          session_players[[name]] <- players_db[[name]]
          break
        }
      }
    }
  }

  # Create computer players with default names and coins
  if (num_computers > 0) {
    for (i in seq_len(num_computers)) {
      comp_name <- paste0("Computer_", i)
      # Avoid name conflicts
      while (comp_name %in% names(session_players)) {
        i <- i + 1
        comp_name <- paste0("Computer_", i)
      }

      if (comp_name %in% names(players_db)) {
        session_players[[comp_name]] <- players_db[[comp_name]]
      } else {
        players_db[[comp_name]] <- list(name = comp_name, coins = 1000, is_computer = TRUE)
        cat("New computer player created: ", comp_name, " with 1000 coins.\n", sep = "")
        session_players[[comp_name]] <- players_db[[comp_name]]
      }
    }
  }

  # Save updated player database
  save_players(players_db)

  return(list(session_players = session_players, players_db = players_db))
}
