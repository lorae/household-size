# tests/testthat/test-bucketing.R
library("testthat")
library("DBI")
library("duckdb")
library("dplyr")
library("glue")

# Source the function to be tested
source("src/utils/bucketing_tools.R")

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
  
  mock_income_lookup <- read_csv_into_db(
    test_con, 
    "mock_income_lookup", 
    "tests/test-data/mock_income_lookup.csv"
  )
  
  # Step 4: Read in expected output
  expected_age_output <- read_csv_into_db(
    test_con, 
    "expected_age_output", 
    "tests/test-data/expected_age_output.csv"
  )
  
  expected_income_output <- read_csv_into_db(
    test_con, 
    "expected_income_output", 
    "tests/test-data/expected_income_output.csv"
  )

  # Step 4: Run the SQL queries 
  age_output <- write_sql_query( 
    data = "mock_data", 
    lookup = "mock_age_lookup", 
    column_name = "AGE"
  ) |>
    sql() |>                 # Convert the SQL string to a SQL object
    tbl(test_con, from = _) |>    # Create a reference to the database table with the SQL query
    collect()                # Collect the results into a data frame
  
  income_output <- write_sql_query( 
    data = "mock_data", 
    lookup = "mock_income_lookup", 
    column_name = "INCOME"
  ) |>
    sql() |>                 # Convert the SQL string to a SQL object
    tbl(test_con, from = _) |>    # Create a reference to the database table with the SQL query
    collect()                # Collect the results into a data frame
  
  # # Step 5: Run the SQL query for income bucketing
  # query_income <- write_sql_query("mock_data", "hhincome_lookup", "HHINCOME")
  # result_income <- tbl(con, sql(query_income)) |> collect()
  # 
  # # Step 6: Define the expected output for age bucketing
  # expected_age <- data.frame(
  #   ID = 1:5,
  #   AGE = c(10, 30, 60, 45, 70),
  #   HHINCOME = c(50000, 150000, 20000, 100000, 250000),
  #   AGE_bucket = c("r00_49", "r00_49", "r50plus", "r00_49", "r50plus")
  # )
  # 
  # # Step 7: Define the expected output for income bucketing
  # expected_income <- data.frame(
  #   ID = 1:5,
  #   AGE = c(10, 30, 60, 45, 70),
  #   HHINCOME = c(50000, 150000, 20000, 100000, 250000),
  #   HHINCOME_bucket = c("r000_100k", "r100kplus", "r000_100k", "r100kplus", "r100kplus")
  # )
  # 
  # # Step 8: Test if the results match the expected output for age bucketing
  # expect_equal(result_age |> select(ID, AGE, HHINCOME, AGE_bucket), expected_age)
  # 
  # # Step 9: Test if the results match the expected output for income bucketing
  # expect_equal(result_income |> select(ID, AGE, HHINCOME, HHINCOME_bucket), expected_income)
  # 
  # # Step 10: Disconnect from DuckDB
  # dbDisconnect(con, shutdown = TRUE)
})
