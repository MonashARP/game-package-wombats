# R/data_directory.R

#' @title Get the User's Home Directory
#' @description Returns a robust path to the user's home directory, with fallbacks for different operating systems.
#' @return A character string containing the path to the user's home directory. If unavailable, returns a temporary directory path.
#' @examples
#' get_home_dir()
#' # Example output (on Linux): "/home/username"
#' # Example output (on Windows): "C:/Users/username"
#' @export
get_home_dir <- function() {
  home <- Sys.getenv("HOME")
  if (nzchar(home)) {
    return(home)
  }
  userprofile <- Sys.getenv("USERPROFILE")
  if (nzchar(userprofile)) {
    return(userprofile)
  }

  tempdir()
}
