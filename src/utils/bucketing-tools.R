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


# Proof-of-concept matching ranges in a lookup table using non-database data
# https://stackoverflow.com/questions/75629990/lookup-table-in-r-by-matching-ranges

# TODO: Roxygen documentation
# A function for bucketing data based on a simple range-based lookup table.
# Returns the input data with an appended column named `output_column`.
# Ranges are inclusive on the bottom end and exclusive on the top end.
# The lookup table must have colnames (bucket_name, lower_bound, upper_bound)
range_match_lookup <- function(
    data, # A dataframe, tibble, or db object containing the data
    lookup, # A dataframe, tibble, or db object containing the lookup table
    input_column, # The name of the column from `data` to be bucketed
    output_column = NULL # optional: the name of the output column. Default: {input_column}_bucket
) {
  # TODO: build in data check on input object types being consistent w/ one another (db,db) or (df, df)
  # TODO: build in check verifying that lookup table ranges don't overlap.
  # That will be fun math problem to solve.
  # TODO: build in check that colnames match. Add warning if any extra columns in lookup table and say
  # that they will be unused, listing the colnames.
  
  # Rename the output_column  to default, if set to null
  if(is.null(output_column)) {
    output_column <- paste0(input_column, "_bucket")
  }
  
  result <- data |>
    # For every unique row of data, a new row is generated combining it with the lookup table
    cross_join(lookup) |>
    # Then only the rows of the lookup table that match the specified data are kept.
    # Note that this logic means that if the lookup table has overlapping ranges 
    # that both match the data, it will produce duplicate entries for the same individual.
    filter(!!sym(input_column) >= lower_bound & !!sym(input_column) < upper_bound) |>
    select(-lower_bound, -upper_bound) |> # Clean up extra columns
    rename(!!sym(output_column) := bucket_name)
  
  return(result)
}


join_columns <- function(
    data1,   # The deprioritized dataset
    data2,   # The prioritized dataset
    column,  # The name of the data column being joined (as a string)
    id       # The name of the column uniquely identifying observations (as a string)
){
  # Join the two datasets on the id column
  result <- data1 |>
    left_join(data2, by = id, suffix = c("_data1", "_data2")) |>
    mutate(
      # Use the value from data2 if it's not NA; otherwise, use the value from data1
      !!sym(column) := coalesce(!!sym(paste0(column, "_data2")), !!sym(paste0(column, "_data1")))
    ) |>
    # Remove the two versions of `column` used for the join, so that only
    # the merged column remains
    select(-!!sym(paste0(column, "_data1")), -!!sym(paste0(column, "_data2"))) |>
    # Remove all `_data2` suffixed columns to delete all other duplicates
    # Note: It would have been equally fine to remove all `_data1` columns instead,
    # since we expect that these two input datasets were exactly identical except
    # for the column called `column`
    select(-ends_with("_data2")) |>
    # Rename the columns ending in "_data1" back to their original names
    rename_with(~ gsub("_data1$", "", .), ends_with("_data1"))
    
  
  return(result)
}


value_match_lookup <- function(
    data, # A dataframe, tibble, or db object containing the data
    lookup, # A dataframe, tibble, or db object containing the lookup table
    input_column, # The name of the column from `data` to be bucketed
    output_column = NULL # optional: the name of the output column. Default: {input_column}_bucket
) {
  # TODO: build in data check on input object types being consistent w/ one another (db,db) or (df, df)
  # TODO: keep only the "specific_value" and "bucket_name"
  
  # Rename the output_column  to default, if set to null
  if(is.null(output_column)) {
    output_column <- paste0(input_column, "_bucket")
  }
  
  result <- data |>
    # For every unique row of data, a new row is generated combining it with the lookup table
    left_join(lookup, by = setNames("specific_value", input_column)) |>
    rename(!!sym(output_column) := bucket_name)
  # Then only the rows of the lookup table that match the specified data are kept.
  # Note that this logic means that if the lookup table has overlapping ranges 
  # that both match the data, it will produce duplicate entries for the same individual.
  
  return(result)
}


# A function that reads a lookup table from a csv file path and splits it into 
# a list with two attributes: 
# $value - a tibble containing a value lookup table. This table can be fed directly
# as the `lookup` argument in value_match_lookup()
# $range - a tibble containing the range lookup table. This table can be fed directly
# as the `lookup` argument in range_match_lookup()
# The source lookup table must have four columns:
# bucket_name
# lower_bound
# upper_bound
# specific_value
split_lookup_table <- function(
    filepath # Path to the .csv file
) {
  # Read the CSV file into a tibble
  lookup_raw <- read_csv(filepath, show_col_types = FALSE)
  
  # Separate rows into range-based and value-based lookups
  range_lookup <- lookup_raw %>%
    filter(!is.na(lower_bound) & !is.na(upper_bound)) %>%
    select(bucket_name, lower_bound, upper_bound)
  
  value_lookup <- lookup_raw %>%
    filter(!is.na(specific_value)) %>%
    select(bucket_name, specific_value)
  
  # Create the processed lookup as a list with two component tibbles: value and range
  lookup_processed <- list(
    value = value_lookup,
    range = range_lookup
  )
  
  # Return the result 
  return(lookup_processed)
}


# Function adding a bucketed column to a database (referenced, optionally,
# through a lazy database reference) using value and range lookup rules specified
# in a lookup table CSV.
append_bucket_column <- function(
    con,             # Database connection
    filepath,        # Path to the lookup table CSV file
    data,            # Lazy database reference (input data)
    input_column,    # The column to be bucketed/matched
    id_column        # The unique ID column
) {
  # Step 1: Split the lookup table into range and value components
  lookup_tb <- split_lookup_table(filepath)
  
  # Step 2: Write the lookup tables to the database
  dbWriteTable(con, paste0(input_column, "_lookup_range"), lookup_tb$range, overwrite = TRUE)
  dbWriteTable(con, paste0(input_column, "_lookup_value"), lookup_tb$value, overwrite = TRUE)
  
  # Step 3: Create lazy references to these tables
  lookup_range_db <- tbl(con, paste0(input_column, "_lookup_range"))
  lookup_value_db <- tbl(con, paste0(input_column, "_lookup_value"))
  
  # Step 4: Perform range and value lookups
  data1_db <- range_match_lookup(
    data = data,
    lookup = lookup_range_db,
    input_column = input_column
  )
  
  data2_db <- value_match_lookup(
    data = data,
    lookup = lookup_value_db,
    input_column = input_column
  )
  
  # Step 5: Join the results
  result <- join_columns(
    data1 = data1_db,
    data2 = data2_db,
    column = paste0(input_column, "_bucket"),
    id = id_column
  )
  
  # Return lazy database reference (result)
  return(result)
}

