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

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Write the IPUMS microdata table to the connection
dbWriteTable(con, "synth_ipums", synth_ipums) 

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
