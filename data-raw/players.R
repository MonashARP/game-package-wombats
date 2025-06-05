players_data <- readRDS("data-raw/players.rds")

usethis::use_data(players_data, overwrite = TRUE)
