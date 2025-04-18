# Set defaults for the four functions below
default_data_col <- "NUMPREC" # a string
default_weight_col <- "PERWT" # a string
default_period_col <- "YEAR" # a string
default_per1 <- 2000
default_per2 <- 2019



# This function prepares frequency tables that can be used as inputs by the 
# `hist` function.
prepare_hist_data <- function(
    data, 
    data_col = default_data_col, # a string
    weight_col = default_weight_col, # a string
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


# This function prepares frequency tables that can be used as inputs by the `double_hist`
# function. It prepares two frequency tables for two specified years so that the plots
# can be overlaid.
prepare_double_hist_data <- function(
    data, 
    data_col = default_data_col, # a string
    weight_col = default_weight_col, # a string
    period_col = default_period_col, # no string: just the name
    xmax = NA,
    per1 = default_per1,
    per2 = default_per2
) {
  # If no xmax is manually assigned, then the max value of `data_col` in the 
  # two periods is selected
  if(is.na(xmax)) {
    xmax <- data |>
      filter(.data[[period_col]] %in% c(per1, per2)) |>
      summarise(xmax = max(.data[[data_col]], na.rm = TRUE)) |>
      pull(xmax)
  }
  
  freq_per1 <- prepare_hist_data(
    data = data |> filter(.data[[period_col]] == per1),
    data_col,
    weight_col,
    xmax
  ) |> rename_with(~ paste0(.x, "_", per1), -label)
  
  freq_per2 <- prepare_hist_data(
    data = data |> filter(.data[[period_col]] == per2),
    data_col,
    weight_col,
    xmax
  ) |> rename_with(~ paste0(.x, "_", per2), -label)
  
  out <- full_join(freq_per1, freq_per2, by = "label")
  
  return(out)
}

# This function creates a simple histogram using the output of the `prepare_hist_data`
# function
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

# This function prepares a histogram with overlaid period bars based on the output
# of prepare_double_hist_data
double_hist <- function(
    freq_data,
    per1 = default_per1,
    per2 = default_per2,
    title = "",
    xtitle = "Number of people in HH",
    ytitle = "Proportion",
    ymax = NA,
    bar_fills = list(
      per1 = list(color = "skyblue", alpha = 0.4, line_color = "skyblue", line_type = "dashed"),
      per2 = list(color = "forestgreen", alpha = 0.2, line_color = "forestgreen", line_type = "solid")
    )
) {
  ggplot(freq_data, aes(x = label)) +
    geom_col(
      aes(y = .data[[paste0("proportion_", per1)]]),
      fill = bar_fills$per1$color,
      alpha = bar_fills$per1$alpha,
      width = 1,
      color = bar_fills$per1$line_color,
      linetype = bar_fills$per1$line_type
    ) +
    geom_col(
      aes(y = .data[[paste0("proportion_", per2)]]),
      fill = bar_fills$per2$color,
      alpha = bar_fills$per2$alpha,
      width = 1,
      color = bar_fills$per2$line_color,
      linetype = bar_fills$per2$line_type
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
}

# ----- Wrapper functions ----- #
make_histogram <- function(
    data,
    data_col = default_data_col,
    weight_col = default_weight_col,
    xmax = NA,
    title = "",
    xtitle = "Number of people in HH",
    ytitle = "Proportion",
    ymax = NA,
    bar_fill = list(color = "skyblue", alpha = 0.5)
) {
  freq_data <- prepare_hist_data(
    data = data,
    data_col = data_col,
    weight_col = weight_col,
    xmax = xmax
  )
  
  hist(
    freq_data = freq_data,
    title = title,
    xtitle = xtitle,
    ytitle = ytitle,
    ymax = ymax,
    bar_fill = bar_fill
  )
}


make_double_histogram <- function(
    data,
    data_col = default_data_col,
    weight_col = default_weight_col,
    period_col = default_period_col,
    per1 = default_per1,
    per2 = default_per2,
    xmax = NA,
    title = "",
    xtitle = "Number of people in HH",
    ytitle = "Proportion",
    ymax = NA,
    bar_fills = list(
      per1 = list(color = "skyblue", alpha = 0.4, line_color = "skyblue", line_type = "dashed"),
      per2 = list(color = "forestgreen", alpha = 0.2, line_color = "forestgreen", line_type = "solid")
    )
) {
  freq_data <- prepare_double_hist_data(
    data = data,
    data_col = data_col,
    weight_col = weight_col,
    period_col = period_col,
    xmax = xmax,
    per1 = per1,
    per2 = per2
  )
  
  double_hist(
    freq_data = freq_data,
    per1 = per1,
    per2 = per2,
    title = title,
    xtitle = xtitle,
    ytitle = ytitle,
    ymax = ymax,
    bar_fills = bar_fills
  )
}

