#' @title Save players to disk
#' @param players named list of players
#' @param bankroll_history A list. Each element is a named numeric vector of player coins per round.

save_players <- function(players, bankroll_history = NULL) {
  file_path <- file.path(get_home_dir(), "players.rds")
  save_list <- list(
    players = players,
    bankroll_history = bankroll_history
  )
  saveRDS(save_list, file = file_path)
}
