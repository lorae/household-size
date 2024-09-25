# fig02.R
#
# This script produces Figure 2. Average Size of a Household by Age, Race/Ethnicity, and Year
# The figure is a facet chart with 7 scatter plots, one per race/ethnicity group
# (AAPI, AIAN, Black, Hispanic, Multiracial, Other, and White). On the x-axis is age,
# and on the y-axis is household size. Each scatter plot has two colors of dots, one
# indicating the values in the 2020 data and another indicating values in the 2000
# data. Each plotted dot represents an averages across a unique combination of
# race/ethnicity, age, and year.
#
# The code at the beginning calculates averages for every age and produces a chart. 
# The code at the end calculates averages for every 5-year age bucket (specified in
# age lookup table "lookup_tables/age/age_buckets01.csv")



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

# Reconnect
con <- dbConnect(duckdb::duckdb(), "db/ipums.duckdb")

race_age_agg_2000_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed_2000"),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("RACE_ETH_bucket", "AGE")
)
race_age_agg_2020_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed_2020"),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("RACE_ETH_bucket", "AGE")
)

race_agg_age_2000_tb <- race_age_agg_2000_db |> mutate(year = 2000) |> collect()
race_agg_age_2020_tb <- race_age_agg_2020_db |> mutate(year = 2020) |> collect()

race_agg_age_tb <- bind_rows(race_agg_age_2000_tb, race_agg_age_2020_tb)

# Create the scatter plot
ggplot(race_agg_age_tb, aes(x = AGE, y = weighted_mean, color = factor(year))) +
  geom_point(alpha = 0.6) +  # Scatter plot points with transparency
  facet_wrap(~RACE_ETH_bucket, nrow = 2, ncol = 4, scales = "free_x") +  # Facet by race, with individual x-axis scales
  labs(x = "Age", y = "Number of People in Household", 
       title = "Number of People in a Household, by Age and Race/Ethnicity (2000 vs 2020)") +
  scale_color_manual(values = c("2000" = "blue", "2020" = "red")) +  # Set blue for 2000 and red for 2020
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),  # Adjust facet label size
    axis.text = element_text(size = 10),   # Adjust axis text size
    plot.title = element_text(hjust = 0.5) # Center the plot title
  )



# Try for different buckets

ipums_db <- tbl(con, "ipums")

obs_count <- ipums_db %>%
  summarise(count = n()) %>%
  pull()
print(paste("Number of observations in IPUMS data:", obs_count))

# Append AGE_bucketed according to the lookup table
age_bucketed_db <- ipums_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/age/age_buckets01.csv", 
    data = _, 
    input_column = "AGE", 
    id_column = "id"
  ) |>
  compute(name = "age_bucketed", temporary = FALSE)
print("Ages bucketed successfully.")

validate_row_counts(
  db = age_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by age group"
)

# Append RACE_bucketed according to the lookup table
age_bucketed_db <- age_bucketed_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/race/race_buckets00.csv",
    data = _,
    input_column = "RACE",
    id_column = "id"
  ) |> 
  compute(name = "race_bucketed", temporary = FALSE)
print("Race bucketed successfully.")

validate_row_counts(
  db = age_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by race"
)

# Append HISPAN_bucketed according to the lookup table
age_bucketed_db <- age_bucketed_db |>
  append_bucket_column(
    con = con,
    filepath = "lookup_tables/hispan/hispan_buckets00.csv",
    data = _,
    input_column = "HISPAN",
    id_column = "id"
  ) |> 
  compute(name = "hispan_bucketed", temporary = FALSE)
print("Ethnicity bucketed successfully.")

validate_row_counts(
  db = age_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by ethnicity"
)

# Use the HISPAN_bucket and RACE_bucket to produce a RACE_ETH_bucket column
age_bucketed_db <- age_bucketed_db |>
  race_eth_bucket(
    data = _
  ) |> 
  compute(name = "race_eth_bucketed", temporary = FALSE)
print("Ethnicity/Race coded into a single bucket successfully.")

validate_row_counts(
  db = age_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed into a combined race-ethnicity column"
)


race_age_agg_2000_db <- weighted_mean(
  data = age_bucketed_db |> filter(YEAR == 2000),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("RACE_ETH_bucket", "AGE_bucket")
)
race_age_agg_2020_db <- weighted_mean(
  data = age_bucketed_db |> filter(YEAR == 2020),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("RACE_ETH_bucket", "AGE_bucket")
)

race_agg_age_2000_tb <- race_age_agg_2000_db |> mutate(year = 2000) |> collect()
race_agg_age_2020_tb <- race_age_agg_2020_db |> mutate(year = 2020) |> collect()

race_agg_age_tb <- bind_rows(race_agg_age_2000_tb, race_agg_age_2020_tb)

library(dplyr)
library(ggplot2)

# Define the correct order for the age buckets
age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                "65-69", "70-74", "75-79", "80-84", "85+")

# Convert AGE_bucket to a factor with the specified order
race_agg_age_tb <- race_agg_age_tb %>%
  mutate(AGE_bucket = factor(AGE_bucket, levels = age_levels),
         year = as.factor(year))  # Ensure 'year' is treated as a factor

fig02 <- ggplot(race_agg_age_tb, aes(x = AGE_bucket, y = weighted_mean, color = year)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter plot points with transparency and size adjustment
  facet_wrap(~RACE_ETH_bucket, nrow = 3, ncol = 3, scales = "free_x") +  # Facet by race, with individual facets for each race and free x-axis
  labs(y = "People per household", color = "Year") +  # Set y-axis label and legend title
  scale_color_manual(values = c("2000" = "#577590", "2020" = "#F94144")) +  # Set blue for 2000 and red for 2020
  scale_y_continuous(breaks = seq(2, 5, by = 1)) +  # Horizontal gridlines only for whole numbers 2, 3, 4, 5
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),  # Adjust facet label size
    axis.text.x = element_text(angle = 60, hjust = 1, size = 6),  # Rotate x-axis labels and reduce size
    axis.title.x = element_blank(),  # Remove the space reserved for the x-axis label
    axis.text.y = element_text(size = 8),  # Reduce y-axis text size
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    panel.grid.major.x = element_line(color = "grey", size = 0.05),  # Customize the vertical gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical gridlines
    panel.grid.major.y = element_line(color = "grey", size = 0.3),  # Customize the horizontal gridlines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal gridlines
    legend.position = c(0.52, 0.13)  # Move the legend into an empty facet spot
  )

# Save the plot as a PNG file
ggsave("results/fig02.png", plot = fig02, width = 6.5, height = 5, dpi = 300)


# Disconnect from DuckDB
DBI::dbDisconnect(con)


