#' @title Ask to play again
#' @description Prompts the user to play another round or exit the game.
#' @return A logical indicating whether the player wants to play again (TRUE) or exit (FALSE).

ask_play_again <- function() {
  repeat {
    cat("\n")
    again <- tolower(readline(prompt = "Type 'again' to play another game, or 'exit' to quit: "))
    if (again == "again") return(TRUE)
    if (again == "exit") return(FALSE)
    cat("Invalid input. Please type 'again' or 'exit'.\n")
  }
}
