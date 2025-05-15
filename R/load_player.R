#' @title Load players from disk
#' @return named list of player info or empty list if none found

load_players <- function() {
  file <- file.path(get_home_dir(), ".blackjack_players.rds")
  if (file.exists(file)) {
    players <- readRDS(file)
    if (is.list(players)) return(players)
  }
  list()
}
