# create-categorical-buckets.R
#
# This module contains helper functions meant to create custom buckets of variables

# create-categorical-buckets.R
#
# This module contains helper functions meant to create custom buckets of variables

#' Generate a Bucket Column Based on Specified Ranges for Continuous Values
#'
#' The `generate_bucket_column` function creates a new column in a data frame by categorizing
#' a specified numeric column based on the ranges provided in a lookup table. It handles continuous values
#' by using lower and upper bounds, where the lower bound is exclusive, and the upper bound is inclusive.
#'
#' @param data A data frame containing the data to be categorized.
#' @param lookup_table A data frame with at least three columns: `bucket_name`, `lower_bound`, and `upper_bound`.
#'        Each row defines a bucket with a name and the range of values for that bucket, where `lower_bound` is exclusive,
#'        and `upper_bound` is inclusive.
#' @param column_name The name of the numeric column in `data` to categorize.
#' @param new_column_name The name of the new column to be created. If not provided, it defaults to `bucketed_{column_name}`.
#'
#' @return A modified data frame with a new column containing the categorized buckets.
#' @examples
#' # Define a lookup table
#' lookup_table <- data.frame(
#'   bucket_name = c("LowIncome", "MiddleIncome"),
#'   lower_bound = c(-Inf, 10000),  # Use -Inf to represent the minimum possible value
#'   upper_bound = c(9999.99, 19999.99)
#' )
#'
#' # Example usage with the given data frame 'household_data'
#' household_data <- generate_bucket_column(
#'   data = household_data, 
#'   lookup_table = lookup_table, 
#'   column_name = "INCOME", 
#'   new_column_name = "income_bucket"
#' )
#'
#' @export
generate_bucket_column <- function(
    data, 
    lookup_table, 
    column_name, 
    new_column_name = NULL) {
  
  # Check if the specified column exists in the data frame
  if (!column_name %in% names(data)) {
    stop(paste("Column", column_name, "not found in the data frame."))
  }
  
  # Generate a default name for the new column if not provided
  if (is.null(new_column_name)) {
    new_column_name <- paste0("bucketed_", column_name)
  }
  
  # Check if the new column name already exists in the data frame
  if (new_column_name %in% names(data)) {
    stop(paste("Column", new_column_name, "already exists in the data frame. Please choose a different name."))
  }
  
  # Initialize the new column for the categorized buckets with NA
  data[[new_column_name]] <- NA
  
  # Loop through each row of the lookup table to categorize based on the provided ranges
  for (i in 1:nrow(lookup_table)) {
    bucket_name <- lookup_table$bucket_name[i]
    lower_bound <- lookup_table$lower_bound[i]
    upper_bound <- lookup_table$upper_bound[i]
    
    # Assign bucket name to entries that fall within the range (lower bound exclusive, upper bound inclusive)
    data[[new_column_name]][data[[column_name]] > lower_bound & data[[column_name]] <= upper_bound] <- bucket_name
  }
  
  # Return the modified data frame with the new column
  return(data)
}
