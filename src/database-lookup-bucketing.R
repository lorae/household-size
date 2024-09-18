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

# ----- Step 1: Helper functions ----- #

source("src/utils/create-synthetic-data.R")

# ---- Step 2: Generate synthetic IPUMS and save to DB ----- #

set.seed(42) # Pseudorandom seed for replicability
synth_ipums <- generate_household_data() # Generate synthetic ipums data

con <- dbConnect(duckdb::duckdb(), ":memory:")
dbWriteTable(con, "synth_ipums", synth_ipums) 

# ----- Step 3: Create a lookup table and save to DB ----- #
age_lookup <- tibble::tibble(
  bucket_name = c("Under 18", "18-34", "35-49", "Over 50"),
  lower_bound = c(-Inf, 18, 35, 50),
  upper_bound = c(18, 35, 50, Inf)
)

dbWriteTable(con, "age_lookup", age_lookup, overwrite = TRUE)

# ----- Step 4: Bucket the data ----- #

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
    left_join(lookup, by = character()) |>
    # Then only the rows of the lookup table that match the specified data are kept.
    # Note that this logic means that if the lookup table has overlapping ranges 
    # that both match the data, it will produce duplicate entries for the same individual.
    filter(!!sym(input_column) >= lower_bound & !!sym(input_column) < upper_bound) |>
    select(-lower_bound, -upper_bound) |> # Clean up extra columns
    rename(!!sym(output_column) := bucket_name)
  
  return(result)
}

# Apply the function to some data (simple data in the workspace)
range_match_lookup(
  data = synth_ipums,
  lookup = age_lookup,
  input_column = "AGE"
)

# Apply the function to a database
age_lookup_db <- tbl(con, "age_lookup")
raw_db <- tbl(con, "synth_ipums")

range_match_lookup(
  data = raw_db,
  lookup = age_lookup_db,
  input_column = "AGE"
)

# Load the table from the connection
raw_db <- tbl(con, "synth_ipums")

# Add and categorize the age_bucket column
processed_db <- raw_db %>%
  mutate(
    AGE_bucket = case_when(
      AGE >= 0 & AGE < 18 ~ 'Under 18',
      AGE >= 18 & AGE < 35 ~ '18-34',
      AGE >= 35 & AGE < 50 ~ '35-49',
      AGE >= 50 & AGE < 200 ~ 'Over 50',
      TRUE ~ 'Unknown'
    )
  )

# Write the result as a new table using compute()
processed_db %>%
  compute("synth_ipums_bucket", temporary = FALSE)
