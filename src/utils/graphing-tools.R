hist <- function(
    data,
    colname = "NUMPREC",
    title = "",
    xmax = NA,
    ymax = NA # If specified, max y-value on chart
) {
  raw_data <- data |> pull({{ colname }})

  # Cap values at xmax if specified
  if (!is.na(xmax)) {
    raw_data <- pmin(raw_data, xmax)
  } else {
    xmax = max(raw_data)
  }
  
  plot_df <- data.frame(raw_data)
  
  # x-axis breaks based on graph
  x_breaks <- seq(from = min(raw_data), to = xmax, by = 1)
  
  # Base plot
  p <- 
    ggplot(
      plot_df, 
      aes(x = raw_data, y = after_stat(density))) +
      geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(limits = if (!is.na(ymax)) c(0, ymax) else NULL) +
    labs(
      title = title,
      x = colname,
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  return(p)
}