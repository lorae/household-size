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
source("src/utils/bucketing-tools.R")

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

