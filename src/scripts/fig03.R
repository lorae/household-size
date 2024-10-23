# fig03.R
#
# This script produces Figure 3: Changes in Average Size of a Household between 
# 2000 and 2020, by State

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")
library("glue")
library("readr")
library("purrr")
library("sf")
library("ggplot2")
library("tigris")
library("rlang")
library("DBI")

# ----- Step 1: Source and define helper functions ----- #
source("src/utils/aggregation-tools.R")

# Creates a 2x2 matrix representing the rotation transformation relative to angle
# a
rot <- function(a) {
  c(cos(a), sin(a), -sin(a), cos(a)) |>
    matrix(_, 2, 2)
}

# Moves a state according to custom specifications
transform_state <- function(
    df, 
    state_fp, 
    rotation_angle, 
    scale_factor, 
    shift_coords
    ) {
  state <- df %>% filter(STATEFIP == state_fp)
  state_geom <- st_geometry(state)
  state_centroid <- st_centroid(st_union(state_geom))
  rotated_geom <- (state_geom - state_centroid) * rot(rotation_angle * pi / 180) / scale_factor + state_centroid + shift_coords
  state %>% st_set_geometry(rotated_geom) %>% st_set_crs(st_crs(df))
}

# ----- Step 3: Load in data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums-processed.duckdb")

# Calculate means by state and year
state_mean_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed"),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("STATEFIP", "YEAR")
)

# Calculate differences in weighted means between 2000 and 2020
state_mean_diff_tb <- difference_means(
  data2000 = state_mean_db |> filter(YEAR == 2000), 
  data2020 = state_mean_db |> filter(YEAR == 2020),
  match_by = "STATEFIP",
  diff_by = "weighted_mean",
  keep = c("count", "sum_weights")
) |> collect()

# ----- Step 4: Load and process state shapefile ----- #

# Load shapefiles from tigris
state_sf <- tigris::states(cb = TRUE, class = 'sf') |>
  rename(STATEFIP = STATEFP) |> # For consistency with household size data
  filter(!STATEFIP %in% c('60', '66', '69', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

# Rotate and move Alaska and Hawaii to fit on map
alaska <- transform_state(state_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii <- transform_state(state_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
state_sf_final <- state_sf %>%
  filter(!STATEFIP %in% c("02", "15")) %>%
  bind_rows(alaska, hawaii)

# Join the state data with household size differences
state_sf_final_diff <- state_sf_final %>%
  left_join(state_mean_diff_tb, by = "STATEFIP")

# ----- Step 6: Generate Figures ----- #

# Choropleth map (color version)
fig03 <- ggplot(state_sf_final_diff) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "2000 to 2020 \ndifference",
    low = "#577590", mid = "white", high = "#F94144", midpoint = 0,
    breaks = seq(from = -0.2, to = 0.05, by = 0.05)
  ) +
  theme_void()

# Save the plot as a PNG file
ggsave("results/fig03.png", plot = fig03, width = 6.5, height = 5, dpi = 300)

# Choropleth map (black and white version)
fig03a <- ggplot(state_sf_final_diff) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = "black", size = 0.5) +
  scale_fill_gradient(
    name = "2000 to 2020 \ndifference",
    low = "black", high = "white", limits = c(-0.2, 0.05),
    oob = scales::squish
  ) +
  theme_void()

# Save the black and white plot as fig03a
ggsave("results/fig03a.png", plot = fig03a, width = 6.5, height = 5, dpi = 300)

# ----- Step 7: Clean up ----- #
dbDisconnect(con)