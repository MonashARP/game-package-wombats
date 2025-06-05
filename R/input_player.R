#' @title Input and validate human and computer players
#' @description Allows input of human player names, creates computer players, loads or creates player data with coins.
#' @param players_db Player Database in Local Directory
#' @return A named list of players (human + computer), where each element is a list with player info (name, money, is_computer).
#' @export
input_players <- function(players_db) {
  # Input total number of players
  repeat {
    total_players <- suppressWarnings(as.integer(readline("Enter total number of players (1-6): ")))
    if (!is.na(total_players) && total_players >= 1 && total_players <= 6) break
    cat("Invalid input. Please enter a number between 1 and 6.\n")
  }

  # Input number of computer players
  repeat {
    num_computers <- suppressWarnings(as.integer(readline(paste0("Enter number of computer players (0-", total_players, "): "))))
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
          # Existing player
          repeat {
            cont <- tolower(readline(paste0("Player '", name, "' found. Continue as this player? (yes/no): ")))
            if (cont %in% c("yes", "no")) break
            cat("Invalid input. Please enter 'yes' or 'no'.\n")
          }

          if (cont == "yes") {
            if (players_db[[name]]$coins <= 0) {
              cat("Sorry, ", name, ", you have 0 or negative coins and cannot join the game.\n", sep = "")
              repeat {
                new_profile <- tolower(readline("Do you want to create a new profile? (yes/no): "))
                if (new_profile %in% c("yes", "no")) break
                cat("Invalid input. Please enter 'yes' or 'no'.\n")
              }

              if (new_profile == "yes") {
                next  # Re-enter name
              } else {
                next  # Skip this player
              }
            }

            cat("Welcome back, ", name, "! You have ", players_db[[name]]$coins, " coins remaining.\n", sep = "")
            session_players[[name]] <- players_db[[name]]
            session_players[[name]]$is_computer <- FALSE
            break
          } else {
            cat("Please enter a different name.\n")
            next
          }
        } else {
          # New player
          players_db[[name]] <- list(name = name, coins = 1000, is_computer = FALSE)
          session_players[[name]] <- players_db[[name]]
          cat("New player created: ", name, " with 1000 coins.\n", sep = "")
          break
        }
      }
    }
  }

  # Create computer players
  created_computers <- 0
  i <- 1

  while (created_computers < num_computers) {
    comp_name <- paste0("Computer_", i)

    if (comp_name %in% names(players_db)) {
      if (players_db[[comp_name]]$coins > 0) {
        session_players[[comp_name]] <- players_db[[comp_name]]
        created_computers <- created_computers + 1
      } else {
        cat("Computer player ", comp_name, " is bankrupt and will be replaced.\n", sep = "")
      }
    }

    if (!(comp_name %in% names(players_db)) || players_db[[comp_name]]$coins <= 0) {
      players_db[[comp_name]] <- list(name = comp_name, coins = 1000, is_computer = TRUE)
      session_players[[comp_name]] <- players_db[[comp_name]]
      cat("New computer player created: ", comp_name, " with 1000 coins.\n", sep = "")
      created_computers <- created_computers + 1
    }

    i <- i + 1
  }

  return(list(session_players = session_players, players_db = players_db))
}
