#' @title Load players from disk
#' @return A list containing players and bankroll_history, or default empty lists if none found

load_players <- function() {
  file_path <- file.path(get_home_dir(), "players.rds")
  if (file.exists(file_path)) {
    saved_data <- readRDS(file_path)
    return(saved_data)
  } else {
    return(list(players = list(), bankroll_history = list()))
  }
}
