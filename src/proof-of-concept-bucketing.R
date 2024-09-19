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
dbWriteTable(
  con, 
  "synth_ipums", 
  synth_ipums, 
  overwrite = TRUE
)
synth_ipums_db <- tbl(con, "synth_ipums") |>
  # Create a column of unique person-level ids
  mutate(id = paste(SERIAL, PERNUM, sep = "_")) |>
  # Make `id` the first column
  select(id, everything())

# ----- Step 3: Bucket the data ---- #

# Append AGE_bucketed according to the lookup table
synth_ipums_bucketed_db <- synth_ipums_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/age/age_buckets00.csv", 
    data = _, 
    input_column = "AGE", 
    id_column = "id"
  ) |>
  compute(name = "synth_ipums_age_bucketed", temporary = FALSE)
print("Ages bucketed successfully.")

# Append HHINCOME_bucketed according to the lookup table
synth_ipums_bucketed_db <- synth_ipums_bucketed_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/hhincome/hhincome_buckets00.csv",
    data = _,
    input_column = "HHINCOME",
    id_column = "id"
  ) |> 
  compute(name = "synth_ipums_hhincome_bucketed", temporary = FALSE)
print("Household incomes bucketed successfully.")

# Append RACE_bucketed according to the lookup table
synth_ipums_bucketed_db <- synth_ipums_bucketed_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/race/race_buckets00.csv",
    data = _,
    input_column = "RACE",
    id_column = "id"
  ) |> 
  compute(name = "synth_ipums_race_bucketed", temporary = FALSE)
print("Race bucketed successfully.")

synth_ipums_bucketed_db |> collect() |> View()

# ----- Step 4: Clean up ----- #

# Disconnect from DuckDB
DBI::dbDisconnect(con)

