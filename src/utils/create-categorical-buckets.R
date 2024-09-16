# create-categorical-buckets.R
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


# Create a query that assigns the buckets within the database
write_sql_query <- function(
    data, # STRING: Name of the database containing data in the connection
    lookup, # STRING: Name of the database containing the lookup table in the connection
    column_name # STRING: Name of the column being transformed
) {
  # Create a dynamic name for the bucket column
  bucket_column_name <- paste0(column_name, "_bucket")
  
  # Build the SQL query using glue
  sql_query <- glue::glue(
    "
    SELECT
        {data}.*,
        COALESCE(specific.bucket_name, range.bucket_name) AS {bucket_column_name}
    FROM
        {data}
    LEFT JOIN (
        SELECT *
        FROM {lookup}
        WHERE specific_value IS NOT NULL
    ) AS specific
        ON {data}.{column_name} = specific.specific_value
    LEFT JOIN (
        SELECT *
        FROM {lookup}
        WHERE specific_value IS NULL
    ) AS range
        ON {data}.{column_name} >= range.lower_bound
        AND {data}.{column_name} < range.upper_bound
  "
  )
  
  return(sql_query)  
}

