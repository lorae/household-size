# create-categorical-buckets.R
#
# This module contains helper functions meant to create custom buckets of variables

#' Generate a bucket column based on specified ranges for continuous values
#'
#' The `generate_bucket_column` function creates a new column in a data frame by 
#' categorizing a specified numeric column based on the ranges provided in a 
#' lookup table. It handles continuous values by using inclusive `lower_bound` 
#' and exclusive `upper_bound`, where the `lower_bound` is inclusive, and the 
#' `upper_bound` is exclusive.
#'
#' @param data A data frame containing the data to be categorized.
#' @param lookup_table A data frame with at least three columns: `bucket_name`, 
#'   `lower_bound` (inclusive), and `upper_bound` (exclusive). Each row defines 
#'   a bucket with a name and the range of values for that bucket. For example, 
#'   an income bucket with a lower bound of $0 and upper bound of $10,000 would 
#'   include the observations $0.00; $1.00; $9,999.00; and $9,999.99; but would 
#'   NOT include an observation of $10,000.00. For a range of buckets to have 
#'   full coverage of the data, without gaps, it is necessary for the upper 
#'   bound of a previous range to equal the lower bound of the following range. 
#'   If the ranges overlap, an error will be thrown. If the ranges do not overlap 
#'   but this condition does not hold, then the function will produce a warning, 
#'   and unbucketed observations will be assigned NA.
#' @param column_name The name of the numeric column in `data` to categorize.
#' @param new_column_name The name of the new column to be created. If not provided, 
#'   it defaults to `bucketed_{column_name}`.
#'
#' @return A modified data frame with a new column containing the categorized 
#'   buckets.
#' @examples
#' # Define a lookup table
#' lookup_table <- data.frame(
#'   bucket_name = c("LowIncome", "HighIncome"),
#'   lower_bound = c(0, 100000),  
#'   upper_bound = c(100000, Inf) # Use Inf to represent the maximum possible value
#' )
#'
#' # Example usage with the given data frame 'household_data'
#' household_data <- generate_bucket_column(
#'   data = household_data, 
#'   lookup_table = lookup_table, 
#'   column_name = "HHINCOME", 
#'   new_column_name = "hhincome_bucket"
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
    new_column_name <- paste0(column_name, "_bucket")
  }
  
  # Check if the new column name already exists in the data frame
  if (new_column_name %in% names(data)) {
    stop(paste("Column", new_column_name, "already exists in the data frame. Please choose a different name."))
  }
  
  # Initialize the new column for the categorized buckets with NA
  data[[new_column_name]] <- NA
  
  # Sort the lookup table by `lower_bound`
  lookup_table <- lookup_table[order(lookup_table$lower_bound), ]
  
  # Check for overlapping ranges in the lookup table
  if (any(lookup_table$upper_bound[-nrow(lookup_table)] > lookup_table$lower_bound[-1])) {
    stop("Error: Overlapping ranges detected in the lookup table.")
  }
  
  # Check for gaps in the ranges and produce a warning if found
  if (any(lookup_table$upper_bound[-nrow(lookup_table)] != lookup_table$lower_bound[-1])) {
    warning("Warning: Gaps detected between ranges in the lookup table. Observations outside these ranges will be assigned NA.")
  }
  
  # Loop through each row of the lookup table to categorize based on the provided ranges
  for (i in 1:nrow(lookup_table)) {
    bucket_name <- lookup_table$bucket_name[i]
    lower_bound <- lookup_table$lower_bound[i]
    upper_bound <- lookup_table$upper_bound[i]
    
    # Assign bucket name to entries that fall within the range (lower bound inclusive, upper bound exclusive)
    data[[new_column_name]][data[[column_name]] >= lower_bound & data[[column_name]] < upper_bound] <- bucket_name
  }
  
  # Return the modified data frame with the new column
  return(data)
}
