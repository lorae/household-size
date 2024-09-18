library("testthat")
library("dplyr")
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function being tested
source("src/utils/bucketing-tools.R")

test_that("range_match_lookup works with a tibble in R memory", {
  
  # Create synthetic data
  raw_df <- tibble(
    AGE = c(15, 18, 25, 40, 60)
  )

  # Create the lookup table
  age_lookup_df <- tibble(
    bucket_name = c("Under 18", "18-34", "35-49", "Over 50"),
    lower_bound = c(0, 18, 35, 50),
    upper_bound = c(18, 35, 50, 200)
  )

  # Apply the range_match_lookup function
  result_df <- range_match_lookup(
    data = raw_df,
    lookup = age_lookup_df,
    input_column = "AGE"
  )
  
  # Expected result
  expected_df <- tibble(
    AGE = c(15, 18, 25, 40, 60),
    AGE_bucket = c("Under 18", "18-34", "18-34", "35-49", "Over 50")
  )
  
  # Assert that the output matches the expected result
  expect_equal(result_df, expected_df)
})
