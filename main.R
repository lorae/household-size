# main.R
#
# Process 2000 IPUMS data into two multidimensional demographic matrices.
# In both matrices, each each cell represents a unique combination of variables 
# (age, sex, race, and PUMA).

# Reconnect
con <- dbConnect(duckdb::duckdb(), "db/ipums.duckdb")



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

# Mean HH size in 2000 and 2020
mean2000 <- ipums_db |> filter(YEAR == 2000) |>
  summarize(mean = sum(PERWT * NUMPREC, na.rm = TRUE)/sum(PERWT)) |>
  pull(mean)

mean2020 <- ipums_db |> filter(YEAR == 2020) |>
  summarize(mean = sum(PERWT * NUMPREC, na.rm = TRUE)/sum(PERWT)) |>
  pull(mean)
mean_hh_size_method1 


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


sf_ohio <- puma_diff_sf |> 
  filter(STATEFIP == 39)

sf_nj <- puma_diff_sf |>
  filter(STATEFIP == 34)

sf_ny <- puma_diff_sf |>
  filter(STATEFIP == 36)

# Create a plot of Idaho PUMA to show that it is only one PUMA for the whole state
map_geographies(puma_diff_sf |> filter(State == "Idaho"))

# Plot differences in various geographies
map_data(puma_diff_sf |> filter(State == "Illinois" & CPUMA0010 > 315)) # Chicago
map_data(puma_diff_sf |> filter(State == "District of Columbia"))
map_data(puma_diff_sf |> filter(State == "Ohio"))
map_data(puma_diff_sf |> filter(State == "New Jersey"))
map_data(puma_diff_sf |> filter(State == "New York"), borders = FALSE) # New York State
map_data(puma_diff_sf |> filter(State == "New York" & CPUMA0010 > 705)) # New York City
map_data(puma_diff_sf |> filter(State == "California" & CPUMA0010 %in% # Bay Area
                                  c(58, 62, 101, 102, 105, 111, 112, 122, 123, 124, 
                                    125, 126, 127, 128, 129, 131, 132, 133, 134, 
                                    135, 136, 138, 139, 140, 141, 142, 143, 144,
                                    145, 147, 148, 149, 150, 151, 152, 153, 158)
                                )) 

# ----- Step 9: Clean up ----- #

# Disconnect from DuckDB
DBI::dbDisconnect(con)

