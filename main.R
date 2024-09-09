# main.R
#
# Process 2000 IPUMS data into two multidimensional demographic matrices.
# In both matrices, each each cell represents a unique combination of variables 
# (age, sex, race, and PUMA).
# In output matrix `nobs`, the value within each cell represents the number
# of observations.
# In output matrix `hhsize`, the value within each cell represents the size of 
# the household
#
# TODO: two matrices not necessary? Each cell has two dimensions of values,
# nobs and hhsize?

# Step 0: Initialize libraries and modules
library("ipumsr")

# Step 1: Load data


# Pseudocode
# Create list of attributes and dimensions of each:
attributes <- list(
  sex = 
    c(
      "M", 
      "F"
      ),
  # I'll do 2 buckets for now for simplicity
  age = 
    c(
      "r00-49",
      "r5plus"
    )
)