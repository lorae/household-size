library("testthat")
library("dplyr")
library("duckdb")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function being tested
source("src/utils/aggregation-tools.R")

# Unit test for difference_means function
test_that("difference_means produces correct results", {
  
  # Create sample data for 2000 and 2020 as tibbles
  data2000_tb <- tibble(
    CPUMA0010 = c(1001, 1002, 1003),
    weighted_mean = c(2.5, 3.1, 2.9),
    count = c(100, 200, 150)
  )
  
  data2020_tb <- tibble(
    CPUMA0010 = c(1001, 1002, 1003),
    weighted_mean = c(2.7, 2.8, 3.0),
    count = c(110, 210, 140)
  )
  
  # Expected output tibble
  expected_tb <- tibble(
    CPUMA0010 = c(1001, 1002, 1003),
    diff = c(0.2, -0.3, 0.1),
    weighted_mean_2020 = c(2.7, 2.8, 3.0),
    weighted_mean_2000 = c(2.5, 3.1, 2.9),
    count_2020 = c(110, 210, 140),
    count_2000 = c(100, 200, 150)
  )
  
  # Create DuckDB connection
  test_con <- dbConnect(duckdb::duckdb(), ":memory:")
  
  # Copy tibbles into DuckDB
  data2000_db <- copy_to(test_con, data2000_tb, name = "data2000_db", temporary = TRUE)
  data2020_db <- copy_to(test_con, data2020_tb, name = "data2020_db", temporary = TRUE)
  
  # Run the difference_means function
  output_db <- difference_means(data2000 = data2000_db, data2020 = data2020_db) |> compute()
  
  # Collect output back to R memory
  output_tb <- output_db |> collect()
  
  # Compare the actual output to the expected output
  expect_equal(output_tb, expected_tb)
  
  # Disconnect DuckDB connection
  dbDisconnect(test_con, shutdown = TRUE)
})

