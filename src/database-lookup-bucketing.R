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

# ----- Step 3: Bucket the data ---- #

# Try out the function
synth_ipums_bucketed_db <- append_bucket_column(
  con = con,
  filepath = "lookup_tables/age/age_buckets00.csv", 
  data = synth_ipums_db, 
  input_column = "AGE", 
  id_column = "id"
)

# ----- Step 4: Clean up ----- #

# Disconnect from DuckDB
DBI::dbDisconnect(con)

