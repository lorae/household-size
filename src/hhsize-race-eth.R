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

# See what exists in the connection
dbListTables(conn = con)

ipums_bucketed_2000_db <- tbl(con, "ipums_bucketed_2000")
ipums_bucketed_2000_db |> head(1000) |> collect() |> View()

race_agg_2000_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed_2000"),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = "RACE_ETH_bucket"
)
race_agg_2000_tb <- race_agg_2000_db |> mutate(year = 2000) |> collect()

race_agg_2020_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed_2020"),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = "RACE_ETH_bucket"
)
race_agg_2020_tb <- race_agg_2020_db |> mutate(year = 2020) |> collect()

race_agg_tb <- bind_rows(race_agg_2000_tb, race_agg_2020_tb)


# graph it

# Define a vector of 7 hexadecimal color codes
custom_colors <- c(
  "#F94144",  
  "#F3722C", 
  "#F8961E",  
  "#F9C74F", 
  "#90BE6D", 
  "#43AA8B", 
  "#577590"  
)

# Side by side panels
ggplot(race_agg_tb, aes(x = RACE_ETH_bucket, y = weighted_mean, fill = RACE_ETH_bucket)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year, ncol = 2) +  # Create side-by-side facets
  labs(x = "", y = "People per household", title = "Average Size of a Household A Person Lives in\nby Race/Ethnicity (2000 vs 2020)") +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border around panels
  )

# Create the bar plot with side-by-side bars for 2000 and 2020
ggplot(race_agg_tb, aes(x = RACE_ETH_bucket, y = weighted_mean, fill = RACE_ETH_bucket)) +
  geom_bar(stat = "identity", aes(group = year, alpha = factor(year)), 
           position = position_dodge(width = 0.8),  # Adjust bar separation
           width = 0.8,  # Make the bars thinner
           color = "black") +  # Add black border to the bars
  scale_alpha_manual(values = c("2000" = 0.4, "2020" = 0.8), guide = guide_legend(title = NULL)) +  # Custom alpha values for years
  scale_fill_manual(values = custom_colors, guide = "none") +  # Apply custom colors, no legend for fill
  labs(x = "", y = "People per household", 
       title = "Average Size of a Household A Person Lives in\nby Race/Ethnicity (2000 vs 2020)") +  # Break title into two lines
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),  # No border around the panel
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal",   # Make the legend horizontal
    legend.title = element_blank()  # Remove the title of the legend
  ) +
  guides(alpha = guide_legend(override.aes = list(fill = c("gray40", "gray40"), color = "black")))  # Custom legend with color boxes

