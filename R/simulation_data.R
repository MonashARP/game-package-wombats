#' Sample simulation data for vignette examples
#'
#' A list containing example players and bankroll history.
#'
#' @format A list with components:
#' \describe{
#'   \item{num_players}{Number of players in the simulation.}
#'   \item{num_rounds}{Number of rounds simulated}
#'   \item{players}{A data frame containing player statistics, including: win rate, lose rate, tie rate, insurance win rate, split rate, split win, split lose, final #' coins, bankroll history, initial coins, bet amount and threshold}
#'   \item{overall}{A data frame with overall statistics for the simulation}
#'   \item{bankroll_history}{A data frame with the bankroll history of each player}
#'   }
#' @source Internal example data for wombat21
#' @keywords internal
"simulation_data"
