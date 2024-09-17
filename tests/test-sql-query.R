# tests/testthat/test-bucketing.R
library("testthat")
library("DBI")
library("duckdb")
library("dplyr")
library("glue")
library("rprojroot")

# Define the project root
root <- rprojroot::find_rstudio_root_file()
setwd(root)

# Source the function to be tested
source("src/utils/bucketing-tools.R")

# Test write_sql_query function with mock data and lookup table
# This test is meant to verify whether the SQL query produced by the write_sql_query()
# function in src/utils/bucket_utils.R produces the expected output in categorizing
# data into buckets in certain edge cases.
test_that("write_sql_query outputs the correct database results with mock data", {
  
  # Step 1: Create a temporary DuckDB in-memory connection
  test_con <- dbConnect(duckdb::duckdb(), ":memory:")
  
  # Step 2: Read in mock data tables
  mock_data <- read_csv_into_db(
    test_con, 
    "mock_data", 
    "tests/test-data/mock_data.csv"
    )
  
  # Step 3: Read in mock lookup tables
  mock_age_lookup <- read_csv_into_db(
    test_con, 
    "mock_age_lookup", 
    "tests/test-data/mock_age_lookup.csv"
  )
  
  mock_hhincome_lookup <- read_csv_into_db(
    test_con, 
    "mock_hhincome_lookup", 
    "tests/test-data/mock_hhincome_lookup.csv"
  )
  
  # Step 4: Read in expected output
  expected_age_output <- read.csv("tests/test-data/expected_age_output.csv", stringsAsFactors = FALSE) |>
    as_tibble() |>
    arrange(ID) # Sort result by ID column for comparability

  expected_hhincome_output <- read.csv("tests/test-data/expected_hhincome_output.csv", stringsAsFactors = FALSE)  |>
    as_tibble() |>
    arrange(ID) # Sort result by ID column for comparability

  # Step 4: Run the SQL queries 
  age_output <- write_sql_query( 
    data = "mock_data", 
    lookup = "mock_age_lookup", 
    column_name = "AGE"
  ) |>
    sql() |>                 # Convert the SQL string to a SQL object
    tbl(test_con, from = _) |>    # Create a reference to the database table with the SQL query
    collect() |>             # Collect the results into a data frame
    arrange(ID) # Sort result by ID column for comparability
  
  hhincome_output <- write_sql_query( 
    data = "mock_data", 
    lookup = "mock_hhincome_lookup", 
    column_name = "HHINCOME"
  ) |>
    sql() |>                 # Convert the SQL string to a SQL object
    tbl(test_con, from = _) |>    # Create a reference to the database table with the SQL query
    collect() |>             # Collect the results into a data frame
    arrange(ID) # Sort result by ID column for comparability
  
  # Step 5: Test if the output matches the expected output
  expect_equal(
    object = age_output, 
    expected = expected_age_output
    )
  
  expect_equal(
    object = hhincome_output, 
    expected = expected_hhincome_output
  )
  
  # Step 6: Disconnect from DuckDB
  dbDisconnect(test_con, shutdown = TRUE)
})
