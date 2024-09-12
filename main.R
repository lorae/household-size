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
library("data.table")
source("src/utils/create-categorical-buckets.R")

# ----- Step 1: Load data
# Current code assumes data is saved to computer. 
# TODO: build in an API call instead to make the code more easily replicable.
# Helpful information on IPUMS and ipumsr from: 
# https://www.youtube.com/watch?v=OT6upQ1dBgU

# On my computer, the usa_00003 data pull represents just data from 2000. It is 
# currently .gitignored. Later, I will make this code more replicable by using
# an API call.
ddi <- read_ipums_ddi("usa_00003.xml")
# Note: This file takes about 3 minutes to read
data <- read_ipums_micro(ddi)

# ----- Step 2: Bucket the data
# Buckets are defined in lookup tables that are stored as .csv files in the /lookup_tables/
# directory. There are several bucketing schemes saved. Here we explicitly choose
# each .csv file.
age_lookup_table <- read.csv("lookup_tables/age/age_buckets00.csv", stringsAsFactors = FALSE)
hhincome_lookup_table <- read.csv("lookup_tables/hhincome/hhincome_buckets00.csv", stringsAsFactors = FALSE)
hispan_lookup_table <- read.csv("lookup_tables/hispan/hispan_buckets00.csv", stringsAsFactors = FALSE)
race_lookup_table <- read.csv("lookup_tables/race/race_buckets00.csv", stringsAsFactors = FALSE)

# Use the lookup tables to add bucket columns to the data frame.
data_bucketed <- data %>%
  # Age buckets
  generate_bucket_continuous(
    data = .,
    lookup_table = age_lookup_table,
    column_name = "AGE"
  ) %>%
  # Household income buckets
  generate_bucket_continuous(
    data = .,
    lookup_table = hhincome_lookup_table,
    column_name = "HHINCOME"
  ) %>%
  # Ethnicity (hispanic/not hispanic) buckets
  generate_bucket_categorical(
    data = .,
    lookup_table = hispan_lookup_table,
    column_name = "HISPAN"
  ) %>%
  # Race buckets
  generate_bucket_categorical(
    data = .,
    lookup_table = race_lookup_table,
    column_name = "RACE"
  )

#View(data_bucketed)

# ----- Step 3: Produce aggregate household sizes in data table

# Convert your large data frame to data.table for faster processing
dt <- as.data.table(data_bucketed)

# Compute weighted mean directly using data.table's grouping and add counts
# TODO: replace HHWT with the correct person-level weight.
aggregate_dt <- dt[, .(
  weighted_mean = weighted.mean(PERNUM, w = HHWT, na.rm = FALSE),  # Weighted mean calculation
  count = .N  # Count of observations in each group
), by = .(
  AGE_bucket, 
  HHINCOME_bucket, 
  SEX, 
  HISPAN_bucket,
  RACE_bucket
  )
]

# Print the resulting data table
print(aggregate_dt)




# ----- Step EXTRA: Explore the data

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

# Check for missing year
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

