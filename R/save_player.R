#' @title Save players to disk
#' @param players named list of players
save_players <- function(players) {
  file <- file.path(get_home_dir(), ".blackjack_players.rds")
  saveRDS(players, file)
}
