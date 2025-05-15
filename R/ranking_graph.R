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

  p <- plotly::plot_ly(
    data = df,
    x = ~player,
    y = ~coins,
    type = 'bar',
    text = ~coins,
    textposition = "inside",  # safer inside
    marker = list(color = 'skyblue')
  )

  p <- plotly::add_trace(
    p,
    x = ~player,
    y = ~coins + max(df$coins) * 0.15,  # push rank label higher
    text = ~paste0("Rank ", rank),
    type = "scatter",
    mode = "text",
    marker = NULL,  # prevent marker warning
    textfont = list(color = "black", size = 12),
    showlegend = FALSE
  )

  p <- plotly::layout(
    p,
    title = "📊 Player Coin Ranking",
    xaxis = list(title = "Player"),
    yaxis = list(title = "Coins"),
    margin = list(b = 100)
  )

  return(p)
}
