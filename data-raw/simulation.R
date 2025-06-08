simulation_data <- readRDS("data-raw/simulation.rds")

usethis::use_data(simulation_data, overwrite = TRUE)
