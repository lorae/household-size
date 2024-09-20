weighted_mean <- function(data, value_column, weight_column, group_by_columns) {
  # Use quasiquotation to handle column names passed as strings
  value_col <- sym(value_column)
  weight_col <- sym(weight_column)
  
  # Dynamically reference grouping columns
  group_by_cols <- syms(group_by_columns)
  
  # Calculate the weighted mean, sum of weights, and count of observations
  data %>%
    group_by(!!!group_by_cols) %>%
    summarize(
      total_value_weighted = sum(!!value_col * !!weight_col, na.rm = TRUE),
      total_weight = sum(!!weight_col, na.rm = TRUE),
      sum_weights = sum(!!weight_col, na.rm = TRUE),  # Sum of weights
      count = n()
    ) %>%
    mutate(weighted_mean = total_value_weighted / total_weight) %>%
    select(!!!group_by_cols, count, sum_weights, weighted_mean) %>%
    compute(name = "weighted_mean", temporary = FALSE)
}
