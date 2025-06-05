# R/io.R
# Design for reading, writing and persistence of player data and game records.

#' @title Save players and bankroll history to disk
#' @description Saves the player list and optional bankroll history to a file in the user's home directory.
#' @param players Named list of players.
#' @param bankroll_history Optional. A list; each element is a named numeric vector of player coins per round.
#' @return Invisibly returns NULL.
#' @examples
#' save_players(list(Alice = list(coins = 1000)), list())
#' @export
save_players <- function(players, bankroll_history = NULL) {
  file_path <- file.path(get_home_dir(), "players.rds")
  save_list <- list(
    players = players,
    bankroll_history = bankroll_history
  )
  saveRDS(save_list, file = file_path)
}

#' @title Load players and bankroll history from disk
#' @description Loads the saved player list and bankroll history from disk, or returns defaults if no save exists.
#' @return A list with two elements:
#'   \describe{
#'     \item{players}{Named list of players.}
#'     \item{bankroll_history}{List of bankroll history (can be empty).}
#'   }
#' @examples
#' res <- load_players()
#' str(res$players)
#' str(res$bankroll_history)
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

#' @title Reset game memory: players and/or bankroll history
#' @description Clears and saves player database and/or bankroll history, depending on arguments.
#' @param players_db Named list of players (before reset).
#' @param bankroll_history List tracking coin balances over rounds (before reset).
#' @param reset_players Logical. Whether to clear \code{players_db} (default TRUE).
#' @param reset_history Logical. Whether to clear \code{bankroll_history} (default TRUE).
#' @return A list with two elements:
#'   \describe{
#'     \item{players_db}{Possibly reset players database (list).}
#'     \item{bankroll_history}{Possibly reset bankroll history (list).}
#'   }
#' @examples
#' players_db <- list(Alice = list(coins = 1000))
#' bankroll_history <- list()
#' reset_game_data(players_db, bankroll_history)
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
