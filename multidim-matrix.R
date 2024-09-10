# multidim-matrix.R
#
# This script produces a multidimensional aggregate matrix on synthetic data
# as a proof-of-concept for aggregation methods in the IPUMS data.

# ----- Step 0: Source helper functions
source("src/utils/create-synthetic-data.R")

# Set pseudorandom seed for replicability
set.seed(42)

# Generate synthetic household data
household_data <- generate_household_data()
print(household_data)


# Add age buckets

# Define a lookup table for ages
age_lookup_table <- data.frame(
  bucket_name = c("r00_49", "r50plus"),
  min_value = c(0, 50),
  max_value = c(49, 200)
)

# Example usage with the given data frame 'household_data'
household_data_bucketed <- categorize_column_buckets(
  data = household_data, 
  lookup_table = lookup_table, 
  column_name = "AGE", 
  new_column_name = "age_bucket"
)

# Print the first few rows of the updated data frame to see the new column
print(head(household_data_bucketed))
