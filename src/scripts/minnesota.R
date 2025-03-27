# minnesota.R
# Specific facts and figures helpful for Minnesota

# ----- Step 0: Load necessary modules, libraries ----- #
library("dplyr")
library("duckdb")
library("readxl")
library("dbplyr")
library("glue")
library("purrr")
library("ggplot2")
library("sf")
library("svglite")

devtools::load_all("../dataduck")

source("src/utils/aggregation-tools.R")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 1: Define miscellaneous functions ----- #


# ----- Step 2: Load database and add needed columns ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Import CPI-U data ----- #
# TODO: this is a copypaste from counterfactual-multiscenario.R. Do this data processing 
# upstream to avoid reiteration
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

# Add columns to ipums_db data 
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

# Merge with data on state 
load("data/helpers/cpuma-state-cross.rda") # Crosswalks CPUMA0010 to state
dbWriteTable(con, "cpuma_state_cross", cpuma_state_cross, overwrite = TRUE) # Write the crosswalk table into DuckDB

ipums_state_db <- ipums_db |>
  left_join(tbl(con, "cpuma_state_cross"), by = "CPUMA0010")

minnesota_db <- ipums_state_db |>
  filter(State == "Minnesota")


# ----- Step 3: Calculate counterfactual ----- #

# Calculate CPUMA-level fully-controlled diffs
hhsize_contributions <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = minnesota_db |> filter(YEAR == 2000, GQ %in% c(0,1,2)), 
  p1_data = minnesota_db |> filter(YEAR == 2019, GQ %in% c(0,1,2)),
  outcome = "NUMPREC"
)$contributions |>
  group_by(CPUMA0010) |>
  summarize(contribution_diff = sum(contribution_diff, na.rm = TRUE),
            prop_2019 = sum(percent_2019) / 100, .groups = "drop",
            pop_2019 = sum(weighted_count_2019)) |>
  mutate(diff = contribution_diff / prop_2019)


# ----- Step 4: Generate counterfactual by CPUMA map ----- #

# Read in shapefiles of every CPUMA0010 region in the United States
sf <- read_sf("data/ipums-cpuma0010-sf/ipums_cpuma0010.shp")

# Minnesota CPUMA map (without coloring)
fig01_minnesota_map <- map_geographies(sf |> filter(State == "Minnesota"))

fig01_minnesota_map
ggsave("results/fig01_minnesota.png", plot = fig01_minnesota_map, width = 6.5, height = 5, dpi = 300)

# Minnesota CPUMA map (coloring by change in household size)
# Values represent the difference between average household size in 2019 and counterfactual
# household size in 2019, using 2000 preferences. Blue values represent smaller households
# than expected; red represents larger households than expected.
cpuma_sf_hhsize <- hhsize_contributions |>
  left_join(sf, by = "CPUMA0010")

fig02_minnesota_map_cf <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = "black", size = 0) +
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.05, to = 0.2, by = 0.05)
  ) +
  theme_void()

fig02_minnesota_map_cf
ggsave("results/fig02_minnesota.png", plot = fig02_minnesota_map_cf, width = 6.5, height = 5, dpi = 300)




# map_geographies is in data_duck
ggsave("docs/images/minnesota_cpumas.svg", plot = minnesota_map, width = 1200, height = 1200, units = "px")

# ----- Step 5: Generate bar graph showing actual, counterfactual, diff (fig 3) ----- #

# ----- Step 6: Calculate housing supply shortage / surfeit ----- #
