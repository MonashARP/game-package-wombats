# R/bankroll_graph.R

#' @title Plot bankroll history over rounds
#' @description Creates a line chart of bankrolls over time using Plotly.
#' @param bankroll_history A list. Each element is a named numeric vector of player coins per round.
#' @return A plotly line chart object
#' @examples
#' \dontrun{
#' bhist <- list(c(Alice = 1000, Bob = 900))
#' plot_bankroll_history(bhist)
#' }
#' @export
plot_bankroll_history <- function(bankroll_history) {
  if (length(bankroll_history) == 0) {
    cat("No bankroll data to plot.\n")
    return(invisible(NULL))
  }

  df <- data.frame(
    Round = unlist(lapply(seq_along(bankroll_history), function(i) rep(i, length(bankroll_history[[i]])))),
    Player = unlist(lapply(bankroll_history, names)),
    Coins = unlist(bankroll_history),
    stringsAsFactors = FALSE
  )

  if (nrow(df) > 0) {
    # Create custom text for the tooltip
    df$Tooltip <- paste0("Player: ", df$Player, "<br>Coins: ", df$Coins)

    p <- plotly::plot_ly(
      df,
      x = ~Round,
      y = ~Coins,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~Tooltip,
      hoverinfo = 'text',
      color = ~Player,
      showlegend = FALSE
    )

    p <- plotly::layout(p,
                        title = "\U0001f4b0 Bankroll Over Time",
                        xaxis = list(title = "Round"),
                        yaxis = list(title = "Coins"),
                        showlegend = FALSE  # double-check legend is hidden in layout
    )
    return(p)
  } else {
    return(invisible(NULL))
  }
}
list(c(You = 1100, Computer_1 = 900), c(You = 1200, Computer_1 = 800))

