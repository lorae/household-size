# bucketing-tools.R
#
# This module contains various helper functions meant to create custom buckets 
# of continuous (such as income, age) and categorical (such as race, ethnicity)
# variables.

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


# https://duckdb.org/2024/04/02/duckplyr.html


# Modified write_sql_query() function to handle multiple columns
write_sql_query_multi <- function(
    data,          # STRING: Name of the database table in the connection
    transformations # LIST: List of transformations with lookup and column_name
) {
  # Start building the SELECT statement
  select_columns <- "data.*"
  left_joins <- ""
  
  # Iterate over each transformation to build the SELECT columns and JOINs
  for (transformation in transformations) {
    column_name <- transformation$column_name
    lookup <- transformation$lookup
    bucket_column_name <- paste0(column_name, "_bucket")
    
    # Build the COALESCE expression for the bucketed column
    select_columns <- glue::glue(
      "{select_columns},
      COALESCE({bucket_column_name}_specific.bucket_name, {bucket_column_name}_range.bucket_name) AS {bucket_column_name}"
    )
    
    # Build the LEFT JOINs for specific and range lookups
    left_joins <- glue::glue(
      "{left_joins}
      LEFT JOIN (
          SELECT *
          FROM {lookup}
          WHERE specific_value IS NOT NULL
      ) AS {bucket_column_name}_specific
          ON data.{column_name} = {bucket_column_name}_specific.specific_value
      LEFT JOIN (
          SELECT *
          FROM {lookup}
          WHERE specific_value IS NULL
      ) AS {bucket_column_name}_range
          ON data.{column_name} >= {bucket_column_name}_range.lower_bound
          AND data.{column_name} < {bucket_column_name}_range.upper_bound
      "
    )
  }
  
  # Combine everything into the final SQL query
  sql_query <- glue::glue(
    "
    SELECT
        {select_columns}
    FROM
        {data} AS data
    {left_joins}
    "
  )
  
  return(sql_query)
}


# Read CSV data tables and write their contents to the database connection
read_csv_into_db <- function(
    con, # Name of the DuckDB connection
    data_title, # STRING: Name that will be assigned to the data table in the connection
    file_path # STRING: File path to the data (must be CSV)
    ) {
  # Read the CSV data
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Write the lookup table to DuckDB
  copy_to(con, data, data_title, overwrite = TRUE)
}

# bucket_with_lookup <- function(
#     con, # name of connection
#     data, # name of database in connection - string 
#     lookup_table, # name of lookup table in connection - string
#     column_name # name of column being bucketed
# ) {
#   # Write a SQL query that will be applied to `data` and `lookup_table`
#   sql_query <- write_sql_query(column_name = column_name)
#   cat(paste0("The SQL query being run is: \n", sql_query, " \n"))
#   
#   # Create a name for the new column being added to `data`
#   new_column_name <- paste0(column_name, "_bucket")
#   print(paste0("Column ", new_column_name, " is being created."))
#   
#   # Join the main data to the lookup table
#   result <- tbl(con, data) %>%
#     left_join(
#       tbl(con, lookup_table), 
#       by = character(), 
#       sql_on = sql_query # Using custom sql query, defined above
#     ) %>%
#     select(-lower_bound, -upper_bound, -specific_value) %>%
#     rename(!!new_column_name := bucket_name) %>% # Rename the new column to `new_column_name`
#     head(n = 50) %>%
#     collect()
#   
#   return(result)
# }
