# create-categorical-buckets.R
#
# This module contains various helper functions meant to create custom buckets 
# of continuous (such as income, age) and categorical (such as race, ethnicity)
# variables.


#' Generate a bucket column for continuous values based on specified ranges and specific values
#'
#' The `generate_bucket_continuous` function creates a new column in a data frame by 
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
#'   it defaults to `{column_name}_bucket`.
#'
#' @return A modified data frame with a new column containing the categorized 
#'   buckets as a factor.
#'
#' @export
generate_bucket_continuous <- function(
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
  
  # Convert the new column to a factor with levels in the order they appear in the lookup table
  data[[new_column_name]] <- factor(data[[new_column_name]], levels = lookup_table$bucket_name)
  
  # Return the modified data frame
  return(data)
}



#' Generate a bucket column for categorical values based on a lookup table
#'
#' The `generate_bucket_categorical` function creates a new column in a data frame by 
#' categorizing a specified categorical column based on a lookup table. It maps old 
#' values to new values as defined in the lookup table.
#'
#' @param data A data frame containing the data to be categorized.
#' @param lookup_table A data frame with columns: `old_val` and `new_val`. The `old_val` 
#'   column represents the current values in the data, and the `new_val` column represents 
#'   the new values to which `old_val` should be mapped.
#' @param column_name The name of the column in `data` to categorize.
#' @param new_column_name The name of the new column to be created. If not provided, 
#'   it defaults to `{column_name}_bucket`.
#'
#' @return A modified data frame with a new column containing the categorized values as a factor.
#'
#' @export
generate_bucket_categorical <- function(
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
  
  # Initialize the new column for the categorized values with NA
  data[[new_column_name]] <- NA
  
  # Loop through each row of the lookup table to categorize based on the provided values
  for (i in 1:nrow(lookup_table)) {
    old_val <- lookup_table$old_val[i]
    new_val <- lookup_table$new_val[i]
    
    # Create a logical vector to identify rows that match the old value
    matches_old_val <- data[[column_name]] == old_val
    
    # Assign the new value to the new column for all rows where the condition is TRUE
    data[[new_column_name]][matches_old_val] <- new_val
  }
  
  # Convert the new column to a factor with levels in the order they appear in the lookup table
  data[[new_column_name]] <- factor(data[[new_column_name]], levels = unique(lookup_table$new_val))
  
  # Return the modified data frame
  return(data)
}



# The purpose of this function is to take inputs from the RACE_bucket and
# HISPAN_bucket columns as defined in the race_buckets00 and hispan_buckets00
# lookup files and use them to generate a joint race/ethnicity column.
create_race_eth_bucket <- function(data) {
  
  # Check if the RACE_bucket and HISPAN_bucket columns exist in the data frame
  if (!"RACE_bucket" %in% names(data)) {
    stop("Column RACE_bucket not found in the data frame.")
  }
  if (!"HISPAN_bucket" %in% names(data)) {
    stop("Column HISPAN_bucket not found in the data frame.")
  }
  
  # Check that the RACE_ETH_bucket column does not already exist in the data frame
  if ("RACE_ETH_bucket" %in% names(data)) {
    warning("Column RACE_ETH_bucket already exists in the data frame and will be overwritten.")
  }
  
  # Initialize the new column with NA
  data[["RACE_ETH_bucket"]] <- NA
  
  # Assign "hispanic" to all observations where HISPAN_bucket is "hispanic"
  data[["RACE_ETH_bucket"]][data[["HISPAN_bucket"]] == "hispanic"] <- "hispanic"
  
  # Assign "black" to remaining observations where RACE_bucket is "black"
  data[["RACE_ETH_bucket"]][is.na(data[["RACE_ETH_bucket"]]) & data[["RACE_bucket"]] == "black"] <- "black"
  
  # Assign "aapi" to remaining observations where RACE_bucket is "aapi"
  data[["RACE_ETH_bucket"]][is.na(data[["RACE_ETH_bucket"]]) & data[["RACE_bucket"]] == "aapi"] <- "aapi"
  
  # Assign "aian" to remaining observations where RACE_bucket is "aian"
  data[["RACE_ETH_bucket"]][is.na(data[["RACE_ETH_bucket"]]) & data[["RACE_bucket"]] == "aian"] <- "aian"
  
  # Assign "multi" to remaining observations where RACE_bucket is "multi"
  data[["RACE_ETH_bucket"]][is.na(data[["RACE_ETH_bucket"]]) & data[["RACE_bucket"]] == "multi"] <- "multi"
  
  # Assign "white" to remaining observations where RACE_bucket is "white"
  data[["RACE_ETH_bucket"]][is.na(data[["RACE_ETH_bucket"]]) & data[["RACE_bucket"]] == "white"] <- "white"
  
  # Assign "other" to remaining observations where RACE_bucket is "other"
  data[["RACE_ETH_bucket"]][is.na(data[["RACE_ETH_bucket"]]) & data[["RACE_bucket"]] == "other"] <- "other"
  
  # Convert the RACE_ETH_bucket to a factor with levels in the desired order
  race_eth_levels <- c("hispanic", "black", "aapi", "aian", "multi", "white", "other")
  data[["RACE_ETH_bucket"]] <- factor(data[["RACE_ETH_bucket"]], levels = race_eth_levels)
  
  # Return the modified data frame
  return(data)
}

