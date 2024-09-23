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

# ----- Step 0: Load required packages ----- #
library("magrittr")
library("dplyr")
library("duckdb")
library("ipumsr")
# library("duckplyr")
library("dbplyr")
library("glue")
library("readr")
library("purrr")
library("sf")
library("ggplot2")

# ----- Step 1: Source helper functions ----- #

source("src/utils/bucketing-tools.R")
source("src/utils/aggregation-tools.R")
source("src/utils/data-validation.R")

# ---- Step 2: Load in IPUMS data and save to DB ----- #

# Current code assumes data is saved to computer. 
# TODO: build in an API call instead to make the code more easily replicable.
# Helpful information on IPUMS and ipumsr from: 
# https://www.youtube.com/watch?v=OT6upQ1dBgU

# On my computer, the usa_00004 data pull represents data from 2000 and 2020. It is 
# currently .gitignored. Later, I will make this code more replicable by using
# an API call.
ddi <- read_ipums_ddi("usa_00004.xml")
# Note: This file takes about 3 minutes to read
print("Reading in IPUMS microdata")
start_time <- Sys.time() # For elapsed time
ipums <- read_ipums_micro(
  ddi,
  var_attrs = c() # Scrub variable attributes so that the data can be read into duckplyr
)
end_time <- Sys.time() # For elapsed time
print(
  paste(
    "Time taken to read in IPUMS microdata:", 
    round(difftime(end_time, start_time, units = "secs"), 3), 
    "seconds")
)

con <- dbConnect(duckdb::duckdb(), ":memory:")
dbWriteTable(con, "ipums", ipums, overwrite = TRUE)

ipums_db <- tbl(con, "ipums") |>
  # Create a column of unique person-level ids
  # Census documentation: "A combination of SAMPLE and SERIAL provides a unique 
  # identifier for every household in the IPUMS; the combination of SAMPLE, SERIAL, 
  # and PERNUM uniquely identifies every person in the database."
  mutate(id = paste(SAMPLE, SERIAL, PERNUM, sep = "_")) |>
  # Make `id` the first column
  select(id, everything())

obs_count <- ipums_db %>%
  summarise(count = n()) %>%
  pull()
print(paste("Number of observations in IPUMS data:", obs_count))

# ----- Step 3: Bucket the data ---- #

# Append AGE_bucketed according to the lookup table
ipums_bucketed_db <- ipums_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/age/age_buckets00.csv", 
    data = _, 
    input_column = "AGE", 
    id_column = "id"
  ) |>
  compute(name = "ipums_age_bucketed", temporary = FALSE)
print("Ages bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by age group"
)

# Append HHINCOME_bucketed according to the lookup table
# TODO: Must deflate HH income from 2020 to 2000 levels.
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/hhincome/hhincome_buckets00.csv",
    data = _,
    input_column = "HHINCOME",
    id_column = "id"
  ) |> 
  compute(name = "ipums_hhincome_bucketed", temporary = FALSE)
print("Household incomes bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by household-level income group"
)

# Append HISPAN_bucketed according to the lookup table
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/hispan/hispan_buckets00.csv",
    data = _,
    input_column = "HISPAN",
    id_column = "id"
  ) |> 
  compute(name = "ipums_hispan_bucketed", temporary = FALSE)
print("Ethnicity bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by ethnicity"
)

# Append RACE_bucketed according to the lookup table
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/race/race_buckets00.csv",
    data = _,
    input_column = "RACE",
    id_column = "id"
  ) |> 
  compute(name = "ipums_race_bucketed", temporary = FALSE)
print("Race bucketed successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by race"
)

# Use the HISPAN_bucket and RACE_bucket to produce a RACE_ETH_bucket column
ipums_bucketed_db <- ipums_bucketed_db |>
  race_eth_bucket(
    data = _
  ) |> 
  compute(name = "ipums_race_eth_bucketed", temporary = FALSE)
print("Ethnicity/Race coded into a single bucket successfully.")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed into a combined race-ethnicity column"
)

# ----- Step 5: Create the individual databases for each year ----- #
ipums_bucketed_2000_db <- ipums_bucketed_db |> # 2000
  filter(YEAR == 2000) |>
  # Force computation now so later aggregation is faster
  compute(name = "ipums_bucketed_2000", temporary = FALSE)

ipums_bucketed_2020_db <- ipums_bucketed_db |> # 2020
  filter(YEAR == 2020) |>
  # Force computation now so later aggregation is faster
  compute(name = "ipums_bucketed_2020", temporary = FALSE)

# Count the number of entries in each year
ipums_bucketed_2000_db |> summarize(count = n()) |> pull() # in 2000
ipums_bucketed_2020_db |> summarize(count = n()) |> pull() # in 2020

# Optional: View a subset of the bucketed data
ipums_bucketed_db |> head(1000) |> collect() |> View()

# ----- Step 6: Calculate weighted mean household size ----- #

weighted_mean_db <- weighted_mean( # Overall
  data = ipums_bucketed_db,
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX")
) |> 
  compute(name = "weighted_mean", temporary = FALSE)

weighted_mean_2000_db <- weighted_mean( # Just 2000
  data = ipums_bucketed_2000_db,
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX")
) |> 
  compute(name = "weighted_mean_2000", temporary = FALSE)

weighted_mean_2020_db <- weighted_mean( # Just 2020
  data = ipums_bucketed_2020_db,
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("HHINCOME_bucket", "AGE_bucket", "RACE_ETH_bucket", "SEX")
) |> 
  compute(name = "weighted_mean_2020", temporary = FALSE)

# To perform the data check I use the raw IPUMS data to calculate average 
# household size. I then use the aggregated weighted_mean tables to 
# calculate what should be an algebraically identical household size. 
# I print both outputs to confirm their similarity.
# 
# Note that these are not valid statistical ways for calculating average
# household size: Average household size should truly be measured by 
# using one observation per household and useing the HHWT, not PERWT, 
# weights. However, these steps serve as another helpful indicator on 
# whether weighted mean tables have gone awry.
# 
# It's the difference between the average household size and the average
# household size a person lives in.
#
# See tests/test-data/weighted-mean-inputs.xlsx, sheet entitled "Mean
#  Household Size" for more information.

mean_hh_size_method1 <- ipums_db |>
  summarize(mean = sum(PERWT * NUMPREC, na.rm = TRUE)/sum(PERWT)) |>
  pull(mean)

mean_hh_size_method2 <- weighted_mean_db |>
  ungroup() |>  # Remove existing groupings
  summarize(mean = sum(weighted_mean * sum_weights, na.rm = TRUE) / sum(sum_weights, na.rm = TRUE)) |>
  pull(mean)

print(mean_hh_size_method1)
print(mean_hh_size_method2)


# ----- Step 7: Calculate aggregates for every PUMA ----- #

puma_mean_db <- weighted_mean( # Overall
  data = ipums_bucketed_db,
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("CPUMA0010")
) |> 
  compute(name = "puma_mean", temporary = FALSE)

puma_mean_2000_db <- weighted_mean( # 2000
  data = ipums_bucketed_2000_db,
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("CPUMA0010")
) |> 
  compute(name = "puma_mean_2000", temporary = FALSE)

puma_mean_2020_db <- weighted_mean( # 2020
  data = ipums_bucketed_2020_db,
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("CPUMA0010")
) |> 
  compute(name = "puma_mean_2020", temporary = FALSE)

puma_diff <- puma_mean_2000_db %>%
  select(   # Rename certain columns to distinguish between years
    CPUMA0010, # The column on which the data are being joined
    weighted_mean_2000 = weighted_mean, 
    count_2000 = count
    ) %>%
  inner_join(
    puma_mean_2020_db %>% 
      select(
        CPUMA0010, 
        weighted_mean_2020 = weighted_mean,
        count_2020 = count
        ),
    by = "CPUMA0010"
  ) %>%
  # Calculate the difference (weighted_mean_2020 - weighted_mean_2000)
  mutate(diff = weighted_mean_2020 - weighted_mean_2000) |>
  # Arrange columns
  select(
    CPUMA0010,
    diff,
    weighted_mean_2020, 
    weighted_mean_2000, 
    count_2020,
    count_2000
  )

puma_diff_tb <- puma_diff |> collect()

# ----- Step 8: Map it ----- #

my_sf <- read_sf("ipums_cpuma0010/ipums_cpuma0010.shp")

# Merge with the data from ipums 
my_sf_merged <- my_sf |>
  left_join(puma_diff_tb, by = "CPUMA0010")

sf_ohio <- my_sf_merged |> 
  filter(STATEFIP == 39)

sf_nj <- my_sf_merged |>
  filter(STATEFIP == 34)

sf_ny <- my_sf_merged |>
  filter(STATEFIP == 36)


# Plot ohio
ggplot(sf_ohio) +
  geom_sf(aes(fill = diff), color = NA) +  # Remove borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +  # Set color scale
  theme_void()  # Keep the void theme


# Plot New Jersey
ggplot(sf_nj) +
  geom_sf(aes(fill = diff), color = NA) +  # Remove borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +  # Set color scale
  theme_void()  # Keep the void theme

# Plot New York
ggplot(sf_ny) +
  geom_sf(aes(fill = diff), color = NA) +  # Remove borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +  # Set color scale
  theme_void()  # Keep the void theme

# # This one takes longer
# ggplot(my_sf_merged) +
#   geom_sf(aes(fill = diff())) +
#   theme_void()

# ----- Step 9: Clean up ----- #

# Disconnect from DuckDB
DBI::dbDisconnect(con)


######################################################################
##### NEARLY DEPRECATED CODE THAT I'M NOT YET READY TO PART WITH #####
######################################################################
# 
# # ----- Step 3: Produce aggregate household sizes in data table
# 
# # Convert your large data frame to data.table for faster processing
# dt <- as.data.table(data_bucketed)
# 
# # Compute weighted mean directly using data.table's grouping and add counts
# # TODO: replace HHWT with the correct person-level weight.
# aggregate_dt <- dt[, .(
#   weighted_mean = weighted.mean(PERNUM, w = HHWT, na.rm = FALSE),  # Weighted mean calculation
#   count = .N  # Count of observations in each group
# ), by = .(
#   AGE_bucket, 
#   HHINCOME_bucket, 
#   SEX, 
#   RACE_ETH_bucket
# )
# ]
# 
# # Print the resulting data table
# print(aggregate_dt)
# 
# # Example lookup tables with desired order
# sex_levels <- c(1, 2)
# 
# # Convert to factor with specified levels
# aggregate_dt[, AGE_bucket]
# aggregate_dt[, HHINCOME_bucket]
# aggregate_dt[, RACE_ETH_bucket]
# aggregate_dt[, SEX := factor(SEX, levels = sex_levels)]
# 
# # Sort by age, income, race/ethnicity, and sex
# setorder(aggregate_dt, AGE_bucket, HHINCOME_bucket, RACE_ETH_bucket, SEX)
# 
# # View the sorted data
# print(aggregate_dt)
# 
# # Export the data.table to a CSV file
# fwrite(aggregate_dt, "output/aggregate_dt_sorted.csv")
# 
# 
# # ----- Step EXTRA: Explore the data
# 
# # Learn higher level info about variables
# View(ddi$var_info)
# 
# # Learn what the variables are!
# names(data) # not very informative
# 
# # Variable labels provide additional insight into what a cryptically-labelled
# # column represents
# ipums_var_label(ddi, PERNUM)
# 
# # Value labels decode numeric codes into their descriptions
# ipums_val_labels(ddi, SEX)
# 
# # A nice static HTML page labelling the data. This can be useful for understanding
# # a dataset after importing it.
# ipums_view(ddi)
# 
# # Check for missing year
# if (any(is.na(data$YEAR))) {
#   warning("There are missing values in the YEAR column.")
# }
# 
# # Data from 2000
# data2000 <- data |>
#   filter(YEAR == 2000)
# 
# # Data from 2020
# data2020 <- data |>
#   filter(YEAR == 2020)
# 
# # Number of observations
# nrow(data2000)
# nrow(data2020)
# 
# ## Number of households. There are two ways to count the number of households.
# 
# # Method 1: Count the number of "person number 1" (within each n-person household, persons 
# # are numbered from 1 to n using the PERNUM variable).
# data2000 |> 
#   filter(PERNUM == 1) |>
#   nrow()
# 
# # Method 2: Count the number of unique serial numbers
# length(unique(data2000$SERIAL))
# 
# ## Average household size
# 
# # In 2000
# data2000 |>
#   filter(PERNUM == 1) |>
#   weighted.mean(x = .$NUMPREC, w = .$HHWT)
# 
# # In 2020
# data2020 |>
#   filter(PERNUM == 1) |>
#   weighted.mean(x = .$NUMPREC, w = .$HHWT)
# 
# 
# 
# 
# 
# 
