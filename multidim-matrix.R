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
