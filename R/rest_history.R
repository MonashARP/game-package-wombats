#' Reset game memory: players_db and/or bankroll_history
#'
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

  return(list(players_db = players_db, bankroll_history = bankroll_history))
}
