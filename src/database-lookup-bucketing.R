# Sample Script for Sarah
#
# Using SQL queries for DuckDBs! Woah!

# ----- Step 0: Load required packages ----- #
library("magrittr")
library("dplyr")
library("duckdb")
# library("duckplyr")
library("dbplyr")
library("glue")
library("readr")
library("purrr")

# ----- Step 1: Helper functions ----- #

source("src/utils/create-synthetic-data.R")
source("src/utils/bucketing-tools.R")

# ---- Step 2: Generate synthetic IPUMS and save to DB ----- #

set.seed(42) # Pseudorandom seed for replicability
synth_ipums <- generate_household_data() # Generate synthetic ipums data

con <- dbConnect(duckdb::duckdb(), ":memory:")
dbWriteTable(con, "synth_ipums", synth_ipums)
synth_ipums_db <- tbl(con, "synth_ipums") |>
  # Create a column of unique person-level ids
  mutate(id = paste(SERIAL, PERNUM, sep = "_")) |>
  # Make `id` the first column
  select(id, everything())

# ----- Append a column with bucketed information ---- #

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


# Try out the function
result <- append_bucket_column(
  con = con,
  filepath = "lookup_tables/age/age_buckets00.csv", 
  data = synth_ipums_db, 
  input_column = "AGE", 
  id_column = "id"
)

# ----- Step 4: Bucket the data ----- #


# Proof of concept: Apply the range_match_lookup function to a tibble
range_match_lookup(
  data = synth_ipums,
  lookup = age_lookup,
  input_column = "AGE"
)

# Assign names to connections to database tables
age_lookup_db <- tbl(con, "age_lookup")
raw_db <- tbl(con, "synth_ipums")

# Proof of concept: Apply the range_match_lookup function to a table in a database
range_match_lookup(
  data = raw_db,
  lookup = age_lookup_db,
  input_column = "AGE"
)

# Proof of concept: Assign the results of the range_match_lookup function to a table
# in the database connection, and export back to the R environment.
processed_db <- range_match_lookup(
  data = raw_db,
  lookup = age_lookup_db,
  input_column = "AGE"
) |>
  compute("synth_ipums_bucket", temporary = FALSE) 

processed_tibble <- processed_db |>
  collect()

# Disconnect from DuckDB
DBI::dbDisconnect(con)

