#' @title Plot current player ranking
#' @description Creates a bar chart showing player coin rankings, with a ranking number added to the chart.
#' @param players A named list of player objects with 'coins'.
#' @return A plotly bar chart object
#' @export

plot_player_ranking <- function(players) {
  df <- data.frame(
    player = names(players),
    coins = sapply(players, function(p) p$coins),
    stringsAsFactors = FALSE
  )

  df <- df[order(-df$coins), ]
  df$rank <- seq_len(nrow(df))
  df$player <- factor(df$player, levels = df$player)

  # Tooltip only
  df$Tooltip <- paste0("Rank: ", df$rank,
                       "<br>Player: ", df$player,
                       "<br>Coins: ", df$coins)

  p <- plotly::plot_ly(
    data = df,
    x = ~player,
    y = ~coins,
    type = 'bar',
    marker = list(color = 'skyblue'),
    hovertext = ~Tooltip,
    hoverinfo = 'text',
    textinfo = 'none',
    showlegend = FALSE
  )

  p <- plotly::layout(
    p,
    title = "\U0001f4ca Player Coin Ranking",
    xaxis = list(title = "Player"),
    yaxis = list(title = "Coins"),
    margin = list(b = 100),
    showlegend = FALSE
  )

  return(p)
}
