# trying to debug
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")
library(base64enc)
library("sf")
options(scipen = 999)

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation
load("data/helpers/cpuma-state-cross.rda") # Crosswalks CPUMA0010 to state
load("data/helpers/state-cpuma-shapefiles.rda") # All shapefiles

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")


# ----- other stuff ----- #

# Generate data for all scenarios
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) 
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) 

# Calculate CPUMA-level fully-controlled diffs
hhsize_contributions_old <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample, 
  p1_data = p1_sample,
  outcome = "NUMPREC"
)$contributions  |>
  group_by(CPUMA0010) |>
  summarize(contribution_diff = sum(contribution_diff, na.rm = TRUE),
            prop_2019 = sum(percent_2019) / 100, .groups = "drop",
            pop_2019 = sum(weighted_count_2019)) |>
  mutate(diff = contribution_diff / prop_2019) |>
  left_join(
    cpuma_state_cross,
    by = "CPUMA0010"
  )
is.na(hhsize_contributions_old$State) |> sum() # No NA values! Great!
hhsize_state_summary_old <- hhsize_contributions_old |>
  group_by(State, STATEFIP) |>
  summarize(
    median = median(diff, na.rm = TRUE),
    weighted_median = rep(diff, times = pop_2019) |> median(),
    weighted_mean = weighted.mean(diff, w = pop_2019, na.rm = TRUE),
    .groups = "drop"
  )

hhsize_contributions_new <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample, 
  p1_data = p1_sample,
  outcome = "NUMPREC"
)$contributions  |>
  group_by(CPUMA0010) |>
  summarize(contribution_diff = sum(contribution_diff, na.rm = TRUE),
            prop_2019 = sum(percent_2019) / 100, .groups = "drop",
            pop_2019 = sum(weighted_count_2019)) |>
  mutate(diff = contribution_diff / prop_2019) |>
  left_join(
    cpuma_state_cross,
    by = "CPUMA0010"
  )
is.na(hhsize_contributions_new$State) |> sum() # No NA values! Great!
hhsize_state_summary_new <- hhsize_contributions_new |>
  group_by(State, STATEFIP) |>
  summarize(
    median = median(diff, na.rm = TRUE),
    weighted_median = rep(diff, times = pop_2019) |> median(),
    weighted_mean = weighted.mean(diff, w = pop_2019, na.rm = TRUE),
    .groups = "drop"
  )


# --- graphs

# Join the state data with household size differences
state_sf_hhsize_old <- state_sf |>
  left_join(hhsize_state_summary_old, by = "STATEFIP")

# Choropleth map (color version)
ggplot(state_sf_hhsize_old) + 
  geom_sf(aes(geometry = geometry, fill = weighted_mean), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "#577590", mid = "white", high = "#F94144", midpoint = 0,
    breaks = seq(from = -0.1, to = 0.2, by = 0.05)
  ) +
  theme_void()

# Join the state data with household size differences
state_sf_hhsize_new <- state_sf |>
  left_join(hhsize_state_summary_new, by = "STATEFIP")

# Choropleth map (color version)
ggplot(state_sf_hhsize_new) + 
  geom_sf(aes(geometry = geometry, fill = weighted_mean), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "#577590", mid = "white", high = "#F94144", midpoint = 0,
    breaks = seq(from = -0.1, to = 0.2, by = 0.05)
  ) +
  theme_void()

# --- cpuma

cpuma_sf_hhsize_old <- cpuma_sf |>
  left_join(hhsize_contributions_old, by = "CPUMA0010") |>
  mutate(hhsize_unexplained = contribution_diff / prop_2019) 

# Choropleth map (color version)
ggplot(cpuma_sf_hhsize_old) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()

cpuma_sf_hhsize_new <- cpuma_sf |>
  left_join(hhsize_contributions_new, by = "CPUMA0010") |>
  mutate(hhsize_unexplained = contribution_diff / prop_2019) 

# Choropleth map (color version)
ggplot(cpuma_sf_hhsize_new) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()




ggplot(cpuma_sf_hhsize_old |> filter(STATEFIP.x == "04")) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf |> filter(STATE == "AZ"), aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()
ggplot(cpuma_sf_hhsize_new |> filter(STATEFIP.x == "04")) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf |> filter(STATE == "AZ"), aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()

st <- "PA"
fips <- "42"
ggplot(cpuma_sf_hhsize_old |> filter(STATEFIP.x == fips)) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf |> filter(STATE == st), aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()
ggplot(cpuma_sf_hhsize_new |> filter(STATEFIP.x == fips)) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf |> filter(STATE == st), aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()