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
  lower_bound = c(0, 18, 35, 50),
  upper_bound = c(18, 35, 50, 200)
)

dbWriteTable(con, "age_lookup", age_lookup, overwrite = TRUE)

# ----- Step 4: Categorize the data ----- #

# https://stackoverflow.com/questions/75629990/lookup-table-in-r-by-matching-ranges
synth_ipums |>
  # For every unique row of data, a new row is generated combining it with the lookup table
  left_join(age_lookup, by = character()) |>
  # Then only the rows of the lookup table that match the specified data are kept.
  # Note that if the lookup table has overlapping ranges that both match the data, it will
  # produce duplicate entries for the same individual.
  filter(AGE >= lower_bound & AGE < upper_bound) |>
  select(-lower_bound, -upper_bound) |> # Clean up extra columns
  rename(AGE_bucket = bucket_name)
  


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
