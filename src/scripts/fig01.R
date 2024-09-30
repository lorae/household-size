# fig01.R
#
# This script produces Figure 1. Average Size of a Household by Race/Ethnicity and Year
# The figure is a bar chart with x-axis categories representing 7 race/ethnicity groups
# (AAPI, AIAN, Black, Hispanic, Multiracial, Other, and White) in 2 years: 2000 and 2020.
# The Y axis is the people per household.

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

# ----- Step 1: Source helper functions ----- #

source("src/utils/bucketing-tools.R")
source("src/utils/aggregation-tools.R")
source("src/utils/data-validation.R")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "db/ipums-processed.duckdb")

race_agg_2000_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed") |> filter(YEAR == 2000),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = "RACE_ETH_bucket"
) |> 
  mutate(year = 2000)

race_agg_2020_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed") |> filter(YEAR == 2020),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = "RACE_ETH_bucket"
) |> 
  mutate(year = 2020)

race_agg_db <- union_all(race_agg_2000_db, race_agg_2020_db) # Row bind the tables
race_agg_tb <- race_agg_db |> collect() # Export into memory

# ----- Step 3: Produce the graph ----- #

# Define a single shade of grey for all bars
grey_color <- "gray40"

# Create the bar plot with side-by-side bars for 2000 and 2020
fig01 <- ggplot(race_agg_tb, aes(x = RACE_ETH_bucket, y = weighted_mean, fill = RACE_ETH_bucket)) +
  geom_bar(stat = "identity", aes(group = year, alpha = factor(year)), 
           position = position_dodge(width = 0.8),  # Adjust bar separation
           width = 0.8,  # Make the bars thinner
           color = "black") +  # Add black border to the bars
  geom_text(aes(label = round(weighted_mean, 2), group = year), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5,  # Adjust label position above the bars
            size = 3) +  # Adjust text size for readability
  scale_alpha_manual(values = c("2000" = 0.4, "2020" = 0.8), guide = guide_legend(title = NULL)) +  # Custom alpha values for years
  scale_fill_manual(values = rep(grey_color, length(unique(race_agg_tb$RACE_ETH_bucket))), guide = "none") +  # Apply grey color, no legend for fill
  labs(y = "People per household") +  # Remove x-axis label
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.border = element_blank(),  # No border around the panel
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",   # Make the legend horizontal
    legend.title = element_blank(),  # Remove the title of the legend
    axis.title.x = element_blank(),  # Remove the space reserved for the x-axis label
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)  # Adjust bottom margin to minimize white space
  ) +
  guides(alpha = guide_legend(override.aes = list(fill = "gray60", color = "black")))  # Custom legend with grey color

# Save the plot as a PNG file
ggsave("results/fig01.png", plot = fig01, width = 6.5, height = 3.5, dpi = 300)


# Disconnect from DuckDB
DBI::dbDisconnect(con)
