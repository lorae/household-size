# This function prepares frequency tables that can be used as inputs by the 
# `hist` function.
prepare_hist_data <- function(
    data, 
    data_col = "NUMPREC", 
    weight_col = "PERWT", 
    xmax = NA
    ) {
  # Produce a frequency table with the first column as the `data_col` and the
  # second column as the sum of the weights in `weight_col`
  freq_raw <- data |> 
    select(all_of(c(data_col, weight_col))) |>
    group_by(.data[[data_col]]) |>
    summarise(freq = sum(.data[[weight_col]], na.rm = TRUE), .groups = "drop") |>
    arrange(.data[[data_col]]) |>
    collect()
  
  # Maximum data_col value observed in the data
  xmax_observed <- freq_raw[[data_col]] |> max()
  
  # Fill in the frequency table with all whole numbers spanning from 1 to the 
  # maximum observed value of `data_col` (i.e. `xmax_observed). Fill in these extra 
  # entries as 0
  filler_values <- tibble(!!data_col := 1:xmax_observed)
  freq_filled <- filler_values |>
    left_join(freq_raw, by = data_col) |>
    mutate(freq = replace_na(freq, 0)) # fill remaining NAs with 0
  
  freq_filled_label <- freq_filled |>
    mutate(
      # If xmax is specified, assign `label` to read as "`xmax`+"
      # Else, assign the label to just be the character version of `data_col`
      label = case_when(
        !is.na(xmax) & .data[[data_col]] >= xmax ~ paste0(xmax, "+"),
        TRUE ~ as.character(.data[[data_col]])
      )
    )
  
  x_axis_factors <- freq_filled_label |> pull(label) |> unique()
  
  # Now sum all values with the same `label` (i.e. all those `xmax`+ entries)
  # Assign a factor label so that the bars remain in numerical order
  freq <- freq_filled_label |>
    mutate(label = factor(label, levels = x_axis_factors)) |>
    group_by(label) |>
    summarize(freq = sum(freq), .groups = "drop") |>
    mutate(proportion = freq / sum(freq)) 
  
  return(freq)
}


# This function prepares frequency tables that can be used as inputs by the XXX
# function. It prepares two frequency tables for two specified years so that the plots
# can be overlaid.
# This function prepares frequency tables that can be used as inputs by the 
# `hist` function.
prepare_hist_data_multiper <- function(
    data, 
    data_col = "NUMPREC", 
    weight_col = "PERWT", 
    period_col = YEAR,
    xmax = NA,
    per1 = 2000,
    per2 = 2019
) {
  # If no xmax is manually assigned, then the max value of `data_col` in the 
  # two periods is selected
  if(is.na(xmax)) {
    xmax <- data |>
      filter({{ period_col }} %in% c(per1, per2)) |>
      summarise(xmax = max(.data[[data_col]], na.rm = TRUE)) |>
      pull(xmax)
  }
  
  freq_per1 <- prepare_hist_data(
    data = data |> filter({{ period_col }} == per1),
    data_col,
    weight_col,
    xmax
  ) |> rename_with(~ paste0(.x, "_", per1), -label)
  
  freq_per2 <- prepare_hist_data(
    data = data |> filter({{ period_col }} == per2),
    data_col,
    weight_col,
    xmax
  ) |> rename_with(~ paste0(.x, "_", per2), -label)
  
  out <- full_join(freq_per1, freq_per2, by = "label")
  
  return(out)
}


hist <- function(
    freq_data, # `output from prepare_hist_data`
    title = "",
    xtitle = "Number of people in HH", # X-axis title
    ytitle = "Proportion", # Y-axis title
    ymax = NA, # If specified, max y-value on chart
    bar_fill = list(color = "skyblue", alpha = 0.5)
) {
  # Plot it!
  p <- ggplot(freq_data, aes(x = label, y = proportion)) +
    geom_col(
      width = 1,
      fill = bar_fill$color,
      alpha = bar_fill$alpha,
      color = "black"
    ) +
    scale_y_continuous(limits = if (!is.na(ymax)) c(0, ymax) else NULL) +
    labs(
      title = title,
      x = xtitle,
      y = ytitle
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 6),
      plot.title = element_text(size = 8),
      axis.title = element_text(size = 7),
      axis.text = element_text(size = 6),
      panel.grid = element_blank()
      )
  
  return(p)
}