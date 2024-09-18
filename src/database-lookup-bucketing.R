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
