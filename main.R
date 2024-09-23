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
library("writexl")

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

# A nice static HTML page labelling the data. This can be useful for understanding
# a dataset after importing it.
ipums_view(ddi)

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

# Difference the tables to determine which CPUMAs saw increases or decreases
# in household size
puma_diff_db <- difference_means(
  data2000 = puma_mean_2000_db,
  data2020 = puma_mean_2020_db
) |> 
  compute(name = "puma_diff", temporary = FALSE)

# Store these results into memory
puma_diff_tb <- puma_diff_db |> collect()

# ----- Step 8: Map it ----- #

sf <- read_sf("ipums_cpuma0010/ipums_cpuma0010.shp")

# Merge shapefile with the differenced IPUMS data 
puma_diff_sf <- sf |>
  left_join(puma_diff_tb, by = "CPUMA0010")

# Summarize population size differences across states
puma_diff_by_states <- puma_diff_sf %>%
  filter(STATEFIP != "72") %>%  # Remove Puerto Rico
  group_by(STATEFIP, State) %>%
  summarize(
    min_diff = min(diff, na.rm = TRUE),
    Q1_diff = quantile(diff, 0.25, na.rm = TRUE),
    median_diff = median(diff, na.rm = TRUE),
    Q3_diff = quantile(diff, 0.75, na.rm = TRUE),
    max_diff = max(diff, na.rm = TRUE),
    min_CPUMA = CPUMA0010[which.min(diff)],
    max_CPUMA = CPUMA0010[which.max(diff)]
  ) %>%
  ungroup()

# Save the summarized results to an Excel file
write_xlsx(puma_diff_by_states, "results/puma_diff_by_states.xlsx")
# Save the unsummarized results, too
write_xlsx(puma_diff_sf, "results/puma_diff.xlsx")

# Ensure 'State' is a factor to maintain order
puma_diff_by_states <- puma_diff_by_states %>%
  mutate(State = factor(State, levels = State))

# Reorder states based on median_diff (comment out if you want states ordered
# alphabetically)
puma_diff_by_states <- puma_diff_by_states %>%
  mutate(State = reorder(State, median_diff))

# Create the box and whisker plot
ggplot(puma_diff_by_states, aes(x = State)) +
  geom_boxplot(
    aes(
      ymin = min_diff,
      lower = Q1_diff,
      middle = median_diff,
      upper = Q3_diff,
      ymax = max_diff
    ),
    stat = "identity"
  ) +
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal() +
  labs(
    title = "Household Size Differences by State",
    x = "State",
    y = "Change in People Per Household, 2000 - 2020"
  )

sf_il <- puma_diff_sf |>
  filter(STATEFIP == 17)

sf_dc <- puma_diff_sf |>
  filter(STATEFIP == 11)

sf_id <- puma_diff_sf |> 
  filter(STATEFIP == 16)

sf_ohio <- puma_diff_sf |> 
  filter(STATEFIP == 39)

sf_nj <- puma_diff_sf |>
  filter(STATEFIP == 34)

sf_ny <- puma_diff_sf |>
  filter(STATEFIP == 36)

# Create a plot of Idaho PUMA to show that it is only one PUMA for the whole state
ggplot(sf_id) +
  geom_sf(fill = "white", color = "black") +  # Set fill to white and borders to black
  theme_void()  # Keep the void theme

# Plot Chicago
ggplot(
  sf_il |> filter(CPUMA0010 > 315)
  ) +
  geom_sf(aes(fill = diff), color = NA) +  # Remove borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +  # Set color scale
  theme_void()  # Keep the void theme

# Plot DC
ggplot(sf_dc) +
  geom_sf(aes(fill = diff), color = NA) +  # Remove borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0) +  # Set color scale
  theme_void()  # Keep the void theme

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

