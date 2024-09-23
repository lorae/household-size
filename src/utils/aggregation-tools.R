weighted_mean <- function(data, value_column, weight_column, group_by_columns) {
  # Use quasiquotation to handle column names passed as strings
  value_col <- sym(value_column)
  weight_col <- sym(weight_column)
  
  # Dynamically reference grouping columns
  group_by_cols <- syms(group_by_columns)
  
  # Calculate the weighted mean, sum of weights, and count of observations
  data %>%
    group_by(!!!group_by_cols) |>
    summarize(
      total_value_weighted = sum(!!value_col * !!weight_col, na.rm = TRUE),
      sum_weights = sum(!!weight_col, na.rm = TRUE),
      count = n()
      # Add weighted variance. Potential resources:
      # https://stats.stackexchange.com/questions/51442/weighted-variance-one-more-time
      # https://influentialpoints.com/Training/two-sample_t-test-principles-properties-assumptions.htm
    ) |>
    mutate(weighted_mean = total_value_weighted / sum_weights) |>
    select(!!!group_by_cols, count, sum_weights, weighted_mean)
}



difference_means <- function(
    data2000, 
    data2020
    ) {
  
  # Prep the data for merging by renaming columns to indicate year
  data2020 <- data2020 |>
    select(
      CPUMA0010,
      weighted_mean_2020 = weighted_mean,
      count_2020 = count
    )
  
  data2000 <- data2000 |>
    select(
      CPUMA0010,
      weighted_mean_2000 = weighted_mean,
      count_2000 = count
    )
  
  # Merge data_2000 and data_2020 by CPUMA0010
  diff <- data2000 |>
    inner_join(data2020, by = "CPUMA0010") |>
    mutate(diff = weighted_mean_2020 - weighted_mean_2000) |>
    # Arrange column order
    select(
      CPUMA0010,
      diff,
      weighted_mean_2020, 
      weighted_mean_2000, 
      count_2020,
      count_2000
    )
  
  return(diff)
}