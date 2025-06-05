# R/io.R
# Design for reading, writing and persistence of player data and game records.

#' @title Save players to disk
#' @param players named list of players
#' @param bankroll_history A list. Each element is a named numeric vector of player coins per round.
#' @export

save_players <- function(players, bankroll_history = NULL) {
  file_path <- file.path(get_home_dir(), "players.rds")
  save_list <- list(
    players = players,
    bankroll_history = bankroll_history
  )
  saveRDS(save_list, file = file_path)
}

#' @title Load players from disk
#' @return A list containing players and bankroll_history, or default empty lists if none found
#' @export

load_players <- function() {
  file_path <- file.path(get_home_dir(), "players.rds")
  if (file.exists(file_path)) {
    saved_data <- readRDS(file_path)
    return(saved_data)
  } else {
    return(list(players = list(), bankroll_history = list()))
  }
}

#' @title Reset game memory: players_db and/or bankroll_history
#' @param reset_players Logical. Whether to clear players_db
#' @param reset_history Logical. Whether to clear bankroll_history
#' @return A list with (possibly reset) players_db and bankroll_history
#' @export
reset_game_data <- function(players_db, bankroll_history,
                            reset_players = TRUE, reset_history = TRUE) {
  if (reset_players) {
    cat(" players_db has been cleared.\n")
    players_db <- list()
  }

  if (reset_history) {
    cat(" bankroll_history has been cleared.\n")
    bankroll_history <- list()
  }

  save_players(players_db, bankroll_history)

  return(list(players_db = players_db, bankroll_history = bankroll_history))
}
