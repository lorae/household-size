# NOTE to self: This script appears deprecated. I am not sure where puma_diff
# is - the database has been streamlined to only one table. Consider removing.

puma_diff_db <- tbl(con, "puma_diff")
puma_diff_tb <-puma_diff_db |> collect()

library(dplyr)

# Count the number of rows where diff is positive and the total number of rows
positive_diff_count <- puma_diff_db %>%
  filter(diff > 0) %>%
  tally() %>%
  collect() %>%
  pull(n)

# Total number of rows
total_row_count <- puma_diff_db %>%
  tally() %>%
  collect() %>%
  pull(n)

# Calculate the percentage of rows where diff is positive
percent_positive_diff <- (positive_diff_count / total_row_count) * 100

# Print the results
cat("Number of rows where diff is positive:", positive_diff_count, "\n")
cat("Total number of rows:", total_row_count, "\n")
cat("Percentage of rows where diff is positive:", round(percent_positive_diff, 2), "%\n")


# Merge with state information to get percentage of states
# with at least one positive diff
sf <- read_sf("ipums_cpuma0010/ipums_cpuma0010.shp")

# Merge shapefile with the differenced IPUMS data 
puma_diff_sf <- sf |>
  left_join(puma_diff_tb, by = "CPUMA0010")

library(dplyr)

# Count the number of unique STATEFIP where at least one diff is positive
unique_fips_positive_diff <- puma_diff_sf %>%
  filter(STATEFIP != "72") |> # no puerto rico
  filter(diff > 0) %>%       # Filter rows where diff is positive
  distinct(STATEFIP) %>%     # Get distinct STATEFIP values
  tally() %>%                # Count the number of unique STATEFIP
  pull(n)                    # Extract the count value

# Print the result
cat("Number of unique FIPS with at least one positive diff:", unique_fips_positive_diff, "\n")

library(dplyr)

# Get the total number of unique STATEFIP and list the state names
unique_fips_and_states <- puma_diff_sf %>%
  distinct(STATEFIP, State) %>%  # Get unique STATEFIP and corresponding state names
  arrange(STATEFIP)              # Sort by STATEFIP for readability

# Count the total number of unique FIPS
total_unique_fips <- unique_fips_and_states %>%
  tally() %>%
  pull(n)

# Print the total number of unique FIPS and list the states
cat("Total number of unique FIPS:", total_unique_fips, "\n")
cat("List of unique states:\n")
print(unique_fips_and_states)

