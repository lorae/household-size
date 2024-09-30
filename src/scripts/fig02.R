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
library("dbplyr")
library("glue")
library("readr")
library("purrr")
library("sf")
library("ggplot2")
library("writexl")

# ----- Step 1: Source and define functions ----- #

source("src/utils/bucketing-tools.R")
source("src/utils/aggregation-tools.R")
source("src/utils/data-validation.R")

# This function produces tables containing the data for figure 2 and 2a
tabulate_aggregates <- function(
    group_by = c() # The columns that variables are grouped by
    ) {
  
  table_2000 <- weighted_mean(
    data = tbl(con, "ipums_bucketed") |> filter(YEAR == 2000),
    value_column = "NUMPREC",
    weight_column = "PERWT",
    group_by_columns = group_by
  ) |>
    mutate(year = 2000)
  
  table_2020 <- weighted_mean(
    data = tbl(con, "ipums_bucketed") |> filter(YEAR == 2020),
    value_column = "NUMPREC",
    weight_column = "PERWT",
    group_by_columns = group_by
  ) |>
    mutate(year = 2020)
  
  combined_table <- union_all(table_2000, table_2020)
  
  return(combined_table)
}

# ----- Step 2: Generate tables underlying graphs ----- #
con <- dbConnect(duckdb::duckdb(), "db/ipums-processed.duckdb")

# For Fig 2: Ages are bucketed
# Note, this is brittle, since age buckets may change
age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                "65-69", "70-74", "75-79", "80-84", "85+")

race_age_bucket_tb <- tabulate_aggregates(group_by = c("RACE_ETH_bucket", "AGE_bucket")) |>
  collect() |>
  mutate(AGE_bucket = factor(AGE_bucket, levels = age_levels),
         year = as.factor(year))

# For Fig 2a: Ages are not bucketed (they're aggregated in 1-year increments)
race_age_tb <- tabulate_aggregates(group_by = c("RACE_ETH_bucket", "AGE")) |>
  collect()

# ---- Step 3: Generate graphs ----- #

# Figure 02 (Ages are bucketed)
fig02 <- ggplot(race_age_bucket_tb, aes(x = AGE_bucket, y = weighted_mean, color = year)) +
  geom_point(alpha = 0.6, size = 2) + 
  facet_wrap(~RACE_ETH_bucket, nrow = 3, ncol = 3, scales = "free_x") +  # Facet by race, with individual facets for each race and free x-axis
  labs(y = "People per household", color = "Year") +  # Set y-axis label and legend title
  scale_color_manual(values = c("2000" = "#577590", "2020" = "#F94144")) + 
  scale_y_continuous(breaks = seq(2, 5, by = 1)) +  # Horizontal gridlines only for whole numbers 2, 3, 4, 5
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),  # Adjust facet label size
    axis.text.x = element_text(angle = 60, hjust = 1, size = 6),  # Rotate x-axis labels and reduce size
    axis.title.x = element_blank(),  # Remove the space reserved for the x-axis label
    axis.text.y = element_text(size = 8),  # Reduce y-axis text size
    panel.grid.major.x = element_line(color = "grey", size = 0.05),  # Customize the vertical gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical gridlines
    panel.grid.major.y = element_line(color = "grey", size = 0.3),  # Customize the horizontal gridlines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal gridlines
    legend.position = c(0.52, 0.13)  # Move the legend into an empty facet spot
  )

ggsave("results/fig02.png", plot = fig02, width = 6.5, height = 5, dpi = 300)

# Figure 02a (ages grouped in 1-year increments)
fig02a <- ggplot(race_age_tb, aes(x = AGE, y = weighted_mean, color = factor(year))) +
  geom_point(alpha = 0.6, size = 1) + 
  facet_wrap(~RACE_ETH_bucket, nrow = 2, ncol = 4, scales = "free_x") +  # Facet by race, with individual x-axis scales
  labs(y = "Number of People in Household", color = "Year") +  # Set y-axis label and legend title
  scale_color_manual(values = c("2000" = "#577590", "2020" = "#F94144")) + 
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +  # Horizontal gridlines only for whole numbers
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  # Vert gridlines, decade increments
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),  # Adjust facet label size
    axis.text.x = element_text(angle = 60, hjust = 1, size = 6),
    axis.title.x = element_blank(),  # Remove the space reserved for the x-axis label
    axis.text.y = element_text(size = 8),  # Reduce y-axis text size
    panel.grid.major.x = element_line(color = "grey", size = 0.05),  # Customize the vertical gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical gridlines
    panel.grid.major.y = element_line(color = "grey", size = 0.3),  # Customize the horizontal gridlines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal gridlines
    legend.position = c(0.87, 0.20)  # Move the legend into an empty facet spot
  )

ggsave("results/fig02a.png", plot = fig02a, width = 6.5, height = 5, dpi = 300)

# Disconnect from DuckDB
DBI::dbDisconnect(con)