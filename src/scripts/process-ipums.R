# process-ipums.R
#
# This script adds bucket columns to raw data based on specifications outlined in
# CSV files in the `lookup_tables/` directory.
# It reads data from the "ipums" table in `/db/ipums-raw.duckdb` and writes processed
# data to the "ipums-bucketed" table in `/db/ipums-processed.duckdb`.
#
# According to the Census Bureau: "A combination of SAMPLE and SERIAL provides a unique 
# identifier for every household in the IPUMS; the combination of SAMPLE, SERIAL, 
# and PERNUM uniquely identifies every person in the database."
#
# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("readr")

# ----- Step 1: Source helper functions ----- #

source("src/utils/bucketing-tools.R")
source("src/utils/data-validation.R")

# ----- Step 2: Prepare the new database ----- #

# Read in raw data
con_raw <- dbConnect(duckdb::duckdb(), "db/ipums-raw.duckdb")

# Create database for processed data
con_processed <- dbConnect(duckdb::duckdb(), "db/ipums-processed.duckdb")

copy_to(
  dest = con_processed, 
  df = tbl(con_raw, "ipums"), 
  name = "ipums_raw", 
  temporary = TRUE, 
  overwrite = TRUE
  )

ipums_db <- tbl(con_processed, "ipums_raw") |>
  mutate(id = paste(SAMPLE, SERIAL, PERNUM, sep = "_")) |>
  select(id, everything())

# ----- Step 3: Create bucket columns in the new database ----- #

# For data validation: ensure no rows are dropped
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()

# Append AGE_bucketed according to the lookup table
ipums_bucketed_db <- ipums_db |>
  append_bucket_column(
    con = con_processed,
    filepath = "lookup_tables/age/age_buckets01.csv", 
    data = _, 
    input_column = "AGE", 
    id_column = "id"
  ) |>
  compute(
    name = "ipums_age_bucketed", 
    temporary = TRUE
  )

print("Ages bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by age group"
)

# Append HHINCOME_bucketed according to the lookup table
# TODO: Must deflate HH income from 2020 to 2000 levels.
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con_processed,
    filepath = "lookup_tables/hhincome/hhincome_buckets00.csv",
    data = _,
    input_column = "HHINCOME",
    id_column = "id"
  ) |> 
  compute(
    name = "ipums_hhincome_bucketed", 
    temporary = TRUE
  )

print("Household incomes bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by household-level income group"
)

# Append HISPAN_bucketed according to the lookup table
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con_processed,
    filepath = "lookup_tables/hispan/hispan_buckets00.csv",
    data = _,
    input_column = "HISPAN",
    id_column = "id"
  ) |> 
  compute(
    name = "ipums_hispan_bucketed", 
    temporary = TRUE
  )

print("Ethnicity bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by ethnicity"
)

# Append RACE_bucketed according to the lookup table
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con_processed,
    filepath = "lookup_tables/race/race_buckets00.csv",
    data = _,
    input_column = "RACE",
    id_column = "id"
  ) |> 
  compute(
    name = "ipums_race_bucketed", 
    temporary = TRUE
  )

print("Race bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by race"
)

# Use the HISPAN_bucket and RACE_bucket to produce a RACE_ETH_bucket column
ipums_bucketed_db <- ipums_bucketed_db |>
  race_eth_bucket(
    data = _
  ) |> 
  compute(
    name = "ipums_race_eth_bucketed", 
    temporary = TRUE
  )

print("Ethnicity/Race coded into a single bucket successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed into a combined race-ethnicity column"
)

# Save this bucketed db to the database
ipums_bucketed_db <- ipums_bucketed_db |>
  compute(
    name = "ipums_bucketed", 
    temporary = FALSE
    )


# ----- Step 4: Clean up ----- #

DBI::dbDisconnect(con_raw)
DBI::dbDisconnect(con_processed)
