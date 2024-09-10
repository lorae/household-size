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

# ----- Step 0: Initialize libraries and modules
library("ipumsr")
library("magrittr")
library("dplyr")

# ----- Step 1: Load data
# Current code assumes data is saved to computer. 
# TODO: build in an API call instead to make the code more easily replicable.
# Helpful information on IPUMS and ipumsr from: 
# https://www.youtube.com/watch?v=OT6upQ1dBgU

ddi <- read_ipums_ddi("usa_00002.xml")
# Note: This file takes about 3 minutes to read
data <- read_ipums_micro(ddi)

# ----- Step 2: Explore the data

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

# ----- Step 3: Actually start analysis
if (any(is.na(data$YEAR))) {
  warning("There are missing values in the YEAR column.")
}

# Data from 2000
data2000 <- data %>%
  filter(YEAR == 2000)

# Data from 2020
data2020 <- data %>%
  filter(YEAR == 2020)

# Number of observations
nrow(data2000)
nrow(data2020)

## Number of households. There are two ways to count the number of households.

# Method 1: Count the number of "person number 1" (within each n-person household, persons 
# are numbered from 1 to n using the PERNUM variable).
data2000 %>% 
  filter(PERNUM == 1) %>%
  nrow()

# Method 2: Count the number of unique serial numbers
length(unique(data2000$SERIAL))

## Average household size

# In 2000
data2000 %>%
  filter(PERNUM == 1) %>%
  weighted.mean(x = .$NUMPREC, w = .$HHWT)

# In 2020
data2020 %>%
  filter(PERNUM == 1) %>%
  weighted.mean(x = .$NUMPREC, w = .$HHWT)

# ----- Step X: Extra pseudocode
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
