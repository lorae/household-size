# counterfactual-regional.R
#
# The purpose of this script is to calculate what - after controlling for demographic 
# factors - average person-level household size would be in 2019 compared to 2000 
# values. This script takes on the counterfactual question with a geographic focus.
# It calculates CPUMA0010-level and state-level counterfactuals.
# 
# The script is closely related to counterfactual-multiscenario.R. This script produces
# the data underlying:
# - Table 3.3
# - Table 3.4
# in the Shiny app, while counterfactual-multiscenario.R produces the data underlying
# Tables 3.1 and 3.2 in the shiny app.
#
# This script will also hopefully eventually be used to create choloropleth maps.
#
# Inputs:
#   - data/db/ipums.duckdb
#   - draws from function defined in src/utils/counterfactual-tools.R
# Outputs:
#   - shiny-app/data/diffs-by-geography.rda
# TODO: Do a shiny-app wide audit of data names: right now, the RDA files are
# sillily named and it's basically the Wild West out here.
# 
# TODO: step 2, 2a, and 2b are identical between this script and counterfactual-regional.R.
# Figure out how potentially to move this data wrangling upstream.

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

# ----- Step 2: Import and wrangle data ----- #

load("data/helpers/state-pop-growth.rda") # For Figure 3.2
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

# ----- Step 2a: Import CPI-U data ----- #
# TODO: Transfer this code to process-ipums.R: import CPI-U series (and bucketing,
# too - see below)
cpiu <- read_excel(
  path = "data/helpers/CPI-U.xlsx",
  sheet = "BLS Data Series",
  range = "A12:N36",
  col_names = TRUE
) |>
  select(Year, Annual) |>
  rename(
    YEAR = Year,
    cpiu = Annual
  )

# Get the 2010 value of cpi_u
cpiu_2010_value <- cpiu |>
  filter(YEAR == 2010) |>
  pull(cpiu)

# Add a new column cpi_u_2010
cpiu <- cpiu |>
  mutate(cpiu_2010_deflator = cpiu / cpiu_2010_value)

# ----- Step 2b: Add columns to ipums_db data ----- #

ipums_db <- tbl(con, "ipums_processed")

# TODO: introduce this process into process-ipums.R rather than being here
# Adjust incomes for CPI-U
ipums_db <- ipums_db |>
  left_join(cpiu, by = "YEAR", copy = TRUE) |>
  mutate(
    INCTOT_cpiu_2010 = if_else(
      INCTOT %in% c(9999999, 9999998), 
      NA_real_, 
      INCTOT / cpiu_2010_deflator
    )
  ) |>
  mutate(
    INCTOT_cpiu_2010 = if_else(
      AGE < 15,
      0,
      INCTOT_cpiu_2010 # Keep the existing value if AGE >= 15
    )
  ) |>
  mutate(
    INCTOT_cpiu_2010_bucket = case_when(
      INCTOT_cpiu_2010 < 0 ~ "neg",
      INCTOT_cpiu_2010 == 0 ~ "0",
      INCTOT_cpiu_2010 < 10000 ~ "under 10k",
      INCTOT_cpiu_2010 >= 10000 & INCTOT_cpiu_2010 < 30000 ~ "10 to 30k",
      INCTOT_cpiu_2010 >= 30000 & INCTOT_cpiu_2010 < 100000 ~ "30k to 100k",
      INCTOT_cpiu_2010 >= 100000 ~ "over 100k",
      TRUE ~ NA_character_ # Handles unexpected cases
    )
  )


# We're adding some simplified/binary variables for counterfactual calculations
ipums_db <- ipums_db |>
  mutate(
    us_born = BPL <= 120 # TRUE if person born in US or US territories
  )

# TODO: eventually write a function in dataduck that when buckets are created,
# the code automatically writes a list of vectors containing factor
# labels. For now, I'm just generating factor labels directly from the lookup
# table here, but this code is more brittle since it relies on me remembering
# which lookup table I used.
age_factor_levels <- extract_factor_label(
  lookup_table = read.csv("lookup_tables/age/age_buckets01.csv"),
  colname = "bucket_name"
)

# Introduce a variable for number of people per bedroom
ipums_db <- ipums_db |>
  mutate(
    persons_per_bedroom = NUMPREC / BEDROOMS
  )

# ----- Step 3: Create mappable diff data by CPUMA0010 ----- #

# Crosswalks CPUMA0010 to state
load("data/helpers/cpuma-state-cross.rda")

# Create a list of states to loop through later
list_of_states <- cpuma_state_cross |>
  select(State) |>
  unique()

# Generate data for all scenarios
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) 
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) 


# Calculate CPUMA-level fully-controlled diffs
hhsize_contributions <- calculate_counterfactual(
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
  mutate(diff = contribution_diff / prop_2019)

bedroom_contributions <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample, 
  p1_data = p1_sample,
  outcome = "persons_per_bedroom"
)$contributions |>
  group_by(CPUMA0010) |>
  summarize(contribution_diff = sum(contribution_diff, na.rm = TRUE),
            prop_2019 = sum(percent_2019) / 100, .groups = "drop",
            pop_2019 = sum(weighted_count_2019)) |>
  mutate(diff = contribution_diff / prop_2019)

# Attach row specifying state to the CPUMA-level diffs
hhsize_contributions_state <- merge(
  cpuma_state_cross,
  hhsize_contributions,
  by = "CPUMA0010"
)
bedroom_contributions_state <- merge(
  cpuma_state_cross,
  bedroom_contributions,
  by = "CPUMA0010"
)

# Data validity checks
is.na(hhsize_contributions_state$State) |> sum() # No NA values! Great!
is.na(bedroom_contributions_state$State) |> sum() # No NA values! Great!

# Create summary tables (each row is one state) showing the median, weighted median,
# and mean diffs. Used as a scaffolding for Shiny app tables 3.3 and 3.4
# Table 3.3 (Persons per household)
hhsize_state_summary <- hhsize_contributions_state |>
  group_by(State, STATEFIP) |>
  summarize(
    median = median(diff, na.rm = TRUE),
    weighted_median = rep(diff, times = pop_2019) |> median(),
    weighted_mean = weighted.mean(diff, w = pop_2019, na.rm = TRUE),
    .groups = "drop"
  )
# Table 3.4 (Persons per bedroom)
bedroom_state_summary <- bedroom_contributions_state |>
  group_by(State, STATEFIP) |>
  summarize(
    median = median(diff, na.rm = TRUE),
    weighted_median = rep(diff, times = pop_2019) |> median(),
    weighted_mean = weighted.mean(diff, w = pop_2019, na.rm = TRUE),
    .groups = "drop"
  )

##############################################
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv#
##############################################
# Creates a 2x2 matrix representing the rotation transformation relative to angle
# a
rot <- function(a) {
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
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

# Load shapefiles. Data is unzipped from https://www.weather.gov/gis/USStates
# Unfortunately, the Census shapefiles I tried to download all had strange shapes
# because they included water bodies.
state_sf <- st_read("data/s_05mr24/s_05mr24.shp") |>
  rename(STATEFIP = FIPS) |> # For consistency with household size data
  filter(!STATEFIP %in% c('60', '64', '66', '68', '69', '70', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

# Rotate and move Alaska and Hawaii to fit on map
alaska <- transform_state(state_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii <- transform_state(state_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
state_sf_final <- state_sf %>%
  filter(!STATEFIP %in% c("02", "15")) |>
  
  bind_rows(alaska, hawaii)

# Join the state data with household size differences
state_sf_hhsize <- state_sf_final |>
  left_join(hhsize_state_summary, by = "STATEFIP")

# Choropleth map (color version)
fig03 <- ggplot(state_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = weighted_mean), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "2000 to 2019 \nUnexplained Change in Household Size",
    low = "#577590", mid = "white", high = "#F94144", midpoint = 0,
    breaks = seq(from = -0.1, to = 0.2, by = 0.05)
  ) +
  theme_void()
fig03
##############################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
##############################################


# A function that produces a dotplot by state
dotplot_by_state <- function(
    state = "New Jersey",
    data = hhsize_contributions_state, # or bedroom_contributions_state
    x_min = -0.5, # Lowest x-value on dotplot
    x_max = 0.5 # Highest x-value on dotplot
) {
  # Subset the data to just that state
  boxplot_data <- subset(data, State == state)
  
  # Calculate median, weighted median, and weighted mean
  median <- boxplot_data |>
    pull(diff) |> 
    median()
  weighted_median <- rep(boxplot_data$diff, times = boxplot_data$pop_2019) |>
    median()
  weighted_mean <- weighted.mean(boxplot_data$diff, w = boxplot_data$pop_2019)
  
  # Create the horizontal boxplot with overlaid points
  output_plot <- ggplot(boxplot_data, aes(x = diff, y = "")) +
    geom_dotplot(stackdir = "center", dotsize = 0.5, alpha = 0.6, binwidth = 0.02) +
    theme_minimal() +
    labs(title = "",
         x = "",
         y = "") +
    theme_void() +
    geom_vline(xintercept = weighted_mean, linetype = "dotted", color = "red", size = 0.5) +
    geom_vline(xintercept = weighted_median, linetype = "dotted", color = "blue", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
    xlim(x_min, x_max)
  
  
  return(output_plot)
}

# Function to generate base64-encoded ggplot images
dotplot_base64 <- function(
    state, 
    data, 
    x_min = -0.5, # Lowest x-value on dotplot
    x_max = 0.5 # Highest x-value on dotplot
    ) {
  file_path <- tempfile(fileext = ".png")
  plot <- dotplot_by_state(state, data, x_min, x_max)
  # Save plot as PNG to a temporary file
  ggsave(file_path, plot = plot, width = 5, height = 1, dpi = 100, units = "in")
  # Convert to base64
  base64_img <- base64encode(file_path)
  # Create an HTML img tag with the base64 string
  img_tag <- sprintf('<img src="data:image/png;base64,%s" width="400px" height="80px"/>', base64_img)
  
  return(img_tag)
}

# Add to tables 3.3 and 3.4: "Plot" column which includes base 64 encoded images of dotplots.
# Table 3.3 (Persons per household)
# The warning messages of removal of rows are expected: since our x range is -0.5 to
# 0.5, we exclude two observations falling outside that range. Small sacrifice to make 
# the data easier to view.
# TODO: Add warning showing number of excluded observations based on inputted x_min
# and x_max
# TODO: attach metadata about these x-axis limits to plots themselves and automatically
# read/display in the server rendering of figures
hhsize_state_summary$plot <- sapply(hhsize_state_summary$State, function(state) {
  dotplot_base64(
    state = state, 
    data = hhsize_contributions_state,
    x_min = -0.5,
    x_max = 0.5 
    )
})
# Table 3.4 (Persons per bedroom)
bedroom_state_summary$plot <- sapply(bedroom_state_summary$State, function(state) {
  dotplot_base64(
    state = state, 
    data = bedroom_contributions_state,
    x_min = -0.9,
    x_max = 0.1
  )
})

# ----- Step 4: Merge data for scatterplots ----- #
# Figures 3.1 and 3.2

# Figure 3.1: Scatter household density change with bedroom density change

# Figure 3.2: Scatter household density change with population change
fig3.2_tab <- hhsize_state_summary |> select(-plot) |>
  inner_join(state_pop_growth, by = "State")

# ----- Step 5: Save the results ----- #

# Diff data
save(
  hhsize_contributions_state,
  hhsize_state_summary,
  bedroom_contributions_state,
  bedroom_state_summary,
  list_of_states,
  fig_3.2_tab,
  file = "shiny-app/data/diffs-by-geography.rda"
)
