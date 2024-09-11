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
# Buckets are defined in lookup tables that are stored as .csv files in the /lookup_tables/
# directory. There are several bucketing schemes saved. Here we explicitly choose
# each .csv file.
age_lookup_table <- read.csv("lookup_tables/age/age_buckets00.csv", stringsAsFactors = FALSE)
hhincome_lookup_table <- read.csv("lookup_tables/hhincome/hhincome_buckets00.csv", stringsAsFactors = FALSE)

# Use the lookup tables to add bucket columns to the data frame.
household_data_bucketed <- household_data %>%
  # Age buckets
  generate_bucket_column(
    data = ., 
    lookup_table = age_lookup_table,
    column_name = "AGE"
  ) %>%
  # Household income buckets
  generate_bucket_column(
    data = ., 
    lookup_table = hhincome_lookup_table,
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


