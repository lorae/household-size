hist <- function(
    data,
    colname = "NUMPREC",
    title = "",
    xtitle = "Number of people in HH", # X-axis title
    ytitle = "Proportion", # Y-axis title
    xmax = NA,
    ymax = NA # If specified, max y-value on chart
) {
  raw_data <- data |> pull({{ colname }}) 
  
  # Cap values at xmax
  if (!is.na(xmax)) {
    raw_data <- pmin(raw_data, xmax)
    x_breaks <- seq(from = min(raw_data), to = xmax, by = 1)
    x_labels <- as.character(x_breaks)
    x_labels[length(x_labels)] <- paste0(xmax, "+")
  } else {
    xmax = max(raw_data)
    x_breaks <- seq(from = min(raw_data), to = xmax, by = 1)
    x_labels <- as.character(x_breaks)
  }
  
  # Warning: ymax
  # TODO
  
  plot_df <- data.frame(raw_data)
  
  # Base plot
  p <- 
    ggplot(
      plot_df, 
      aes(x = raw_data, y = after_stat(density))) +
      geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels
      ) +
    scale_y_continuous(limits = if (!is.na(ymax)) c(0, ymax) else NULL) +
    labs(
      title = title,
      x = xtitle,
      y = ytitle
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  return(p)
}