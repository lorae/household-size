# multidim-matrix.R
#
# This script produces a multidimensional aggregate matrix on synthetic data
# as a proof-of-concept for aggregation methods in the IPUMS data.

# ----- Step 0: Source helper functions
source("src/utils/create-synthetic-data.R")
source("src/utils/create-categorical-buckets.R")

# Set pseudorandom seed for replicability
set.seed(42)

# Generate synthetic household data
household_data <- generate_household_data()
print(household_data)


# Add age buckets

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

# Example usage with the given data frame 'household_data'
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

# Print the first few rows of the updated data frame to see the new column
View(household_data_bucketed)
