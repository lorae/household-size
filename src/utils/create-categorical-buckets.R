#' Generate a bucket column based on specified ranges and specific values
#'
#' The `generate_bucket_column` function creates a new column in a data frame by 
#' categorizing a specified numeric column based on the ranges provided in a 
#' lookup table or specific values. It handles continuous values by using inclusive 
#' `lower_bound` and exclusive `upper_bound`, and allows for specific values to be 
#' matched directly.
#'
#' @param data A data frame containing the data to be categorized.
#' @param lookup_table A data frame with columns: `bucket_name`, `lower_bound`, 
#'   `upper_bound`, and `specific_value`. The `specific_value` column handles 
#'   cases that match specific values.
#' @param column_name The name of the numeric column in `data` to categorize.
#' @param new_column_name The name of the new column to be created. If not provided, 
#'   it defaults to `bucketed_{column_name}`.
#'
#' @return A modified data frame with a new column containing the categorized 
#'   buckets.
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
  lookup_table <- lookup_table[order(lookup_table$lower_bound, na.last = TRUE), ]
  
  # Handle specific values first
  for (i in 1:nrow(lookup_table)) {
    bucket_name <- lookup_table$bucket_name[i]
    specific_value <- lookup_table$specific_value[i]
    
    # If there's a specific value to match
    if (!is.na(specific_value)) {
      # Create a logical vector to identify rows that match the specific value
      matches_specific_value <- data[[column_name]] == specific_value
      
      # Assign the bucket name to the new column for all rows where the condition is TRUE
      data[[new_column_name]][matches_specific_value] <- bucket_name
    }
  }
  
  # Loop through each row of the lookup table to categorize based on the provided ranges
  for (i in 1:nrow(lookup_table)) {
    bucket_name <- lookup_table$bucket_name[i]
    lower_bound <- lookup_table$lower_bound[i]
    upper_bound <- lookup_table$upper_bound[i]
    
    # Skip if it's a specific value rule
    if (!is.na(lookup_table$specific_value[i])) next
    
    # Create a logical vector to identify rows that fall within the range of bucket `i`
    in_range <- data[[column_name]] >= lower_bound & data[[column_name]] < upper_bound
    
    # Assign the bucket name to the new column for all rows where the condition is TRUE
    data[[new_column_name]][in_range] <- bucket_name
  }
  
  # Return the modified data frame with the new column
  return(data)
}
