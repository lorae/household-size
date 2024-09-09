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
# Current code assumes data is saved to computer. 
# TODO: build in an API call instead to make the code more easily replicable.
# Helpful information on IPUMS and ipumsr from: 
# https://www.youtube.com/watch?v=OT6upQ1dBgU

ddi <- read_ipums_ddi("usa_00002.xml")
# Note: This file takes about 3 minutes to read
data <- read_ipums_micro(ddi)

# Step 2: Explore the data

# Learn higher level info about variables
View(ddi$var_info)

# Learn what the variables are!
names(data) # not very informative

# Variable labels provide additional insight into what a cryptically-labelled
# column represents
ipums_var_label(ddi, PERNUM)

# Value labels decode numeric codes into their descriptions
ipums_val_labels(ddi, SEX)

# A nice static HTML page labelling the data. This can be useful for understanding
# a dataset after importing it.
ipums_view(ddi)

# Step X: Extra pseudocode
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
