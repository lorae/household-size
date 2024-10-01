# fig03.R
#
# This script produces Figure 3: Changes in Average Size of a Household between 
# 2000 and 2020, by State

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
library("tigris")
library("DBI")

# ----- Step 1: Source helper functions ----- #

source("src/utils/bucketing-tools.R")
source("src/utils/aggregation-tools.R")
source("src/utils/data-validation.R")


# ----- Step 2: Load in data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums-processed.duckdb")

# ----- Step 3: Calculate means by state and year

state_mean_db <- weighted_mean(
  data = tbl(con, "ipums_bucketed"),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("STATEFIP", "YEAR")
)

# Now subtract the 2020 values from the 2000 values
state_mean_2000_db <- state_mean_db |>
  filter(YEAR == 2000)
state_mean_2020_db <- state_mean_db |>
  filter(YEAR == 2020)

# TODO: universalize the difference_means function and use it
# instead
state_mean_diff_tb <- state_mean_2000_db |>
  inner_join(state_mean_2020_db, by = "STATEFIP", suffix = c("_2000", "_2020")) |>
  mutate(diff = weighted_mean_2020 - weighted_mean_2000) |>
  select(
    STATEFIP, 
    diff, 
    weighted_mean_2000, 
    count_2000, 
    sum_weights_2000, 
    weighted_mean_2020, 
    count_2020,
    sum_weights_2020
    ) |>
  collect()


df <- tigris::states(cb = T, class = 'sf')

df <- df %>% filter(!STATEFP %in% c('60', '66', '69', '72', '78'))

crs_lambert <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

df <- df %>%
  st_transform(crs = crs_lambert)


alaska <- df %>% filter(STATEFP == "02")
alaska_g <- st_geometry(alaska)
alaska_centroid <- st_centroid(st_union(alaska_g))

rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

alaska_trans <- (alaska_g - alaska_centroid) * rot(-39 * pi/180) / 2.3 + alaska_centroid + c(1000000, -5000000)

alaska <- alaska %>% st_set_geometry(alaska_trans) %>% st_set_crs(st_crs(df))


hawaii <- df %>% filter(STATEFP == "15")

hawaii_g <- st_geometry(hawaii)
hawaii_centroid <- st_centroid(st_union(hawaii_g))

hawaii_trans <- (hawaii_g - hawaii_centroid) * rot(-35 * pi/180) + hawaii_centroid + c(5200000, -1400000)
hawaii <- hawaii %>% st_set_geometry(hawaii_trans) %>% st_set_crs(st_crs(df))

tnc_map_final <- df %>%
  filter(!STATEFP %in% c("02", "15")) %>%
  rbind(alaska) %>%
  rbind(hawaii)

tnc_map_final_diff <- tnc_map_final %>%
  left_join(state_mean_diff_tb, by = c("STATEFP" = "STATEFIP"))

# Adjust the choropleth map with additional break
fig03 <- ggplot(tnc_map_final_diff) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "2000 to 2020 \ndifference",
    low = "#577590",
    mid = "white",
    high = "#F94144",
    midpoint = 0,
    breaks = seq(from =-0.2, to = 0.05, by = 0.05) # Add the desired breaks, including a positive number
  ) +
  theme_void()

# Save the plot as a PNG file
ggsave("results/fig03.png", plot = fig03, width = 6.5, height = 5, dpi = 300)


##### Alternate
# color scale is Black and White

# Create the black and white version of the choropleth map with gradual gray shades
fig03a <- ggplot(tnc_map_final_diff) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = "black", size = 0.5) +
  scale_fill_gradient(
    name = "2000 to 2020 \ndifference",
    low = "black",
    high = "white",
    limits = c(-0.2, 0.05),  # Set the limits to control the range
    oob = scales::squish  # Squish values outside the range into the limit
  ) +
  theme_void()

# Save the black and white plot as fig03a
ggsave("results/fig03a.png", plot = fig03a, width = 6.5, height = 5, dpi = 300)

# Disconnect from DuckDB
DBI::dbDisconnect(con)
