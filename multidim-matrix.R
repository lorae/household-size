# multidim-matrix.R
#
# This script produces a multidimensional aggregate matrix on synthetic data
# as a proof-of-concept for aggregation methods in the IPUMS data.

# ----- Step 0: Source helper functions and packages
library("data.table")
library("magrittr")
source("src/utils/create-synthetic-data.R")
source("src/utils/create-categorical-buckets.R")

# ----- Step 1: Generate synthetic data

# Set pseudorandom seed for replicability
set.seed(42)

# Generate synthetic household data
household_data <- generate_household_data()
View(household_data)

# ----- Step 2: Bucket the data

# Step 2A: Define lookup tables for bucketing specific columns
# Define a lookup table for ages
age_lookup_table <- data.frame(
  bucket_name = c("r00_49", "r50plus"),
  lower_bound = c(0, 50),
  upper_bound = c(50, 200)
)

# Define a lookup table for household income
hhinc_lookup_table <- data.frame(
  bucket_name = c("LowIncome", "HighIncome"),
  lower_bound = c(0, 100000),  
  upper_bound = c(100000, Inf)
)

# Step 2B: Bucket the data using lookup tables
household_data_bucketed <- household_data %>%
  # Bucket the age categories
  generate_bucket_column(
    data = ., 
    lookup_table = age_lookup_table, 
    column_name = "AGE"
  ) %>%
  # Bucket the income categories
  generate_bucket_column(
    data = ., 
    lookup_table = hhinc_lookup_table, 
    column_name = "HHINCOME"
  ) 

View(household_data_bucketed)

# ----- Step 3: Produce aggregate household sizes in data table

# Convert your large data frame to data.table for faster processing
household_dt <- as.data.table(household_data_bucketed)

# Compute weighted mean directly using data.table's grouping and add counts
# TODO: replace HHWT with the correct person-level weight.
aggregate_dt <- household_dt[, .(
  weighted_mean = weighted.mean(PERNUM, w = HHWT, na.rm = FALSE),  # Weighted mean calculation
  count = .N  # Count of observations in each group
), by = .(AGE_bucket, HHINCOME_bucket, SEX)]

# Print the resulting data table
print(aggregate_dt)


