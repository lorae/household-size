# counterfactual-multiscenario.R
#
# The purpose of this script is to calculate what - after controlling for demographic 
# factors - average person-level household size would be in 2019 compared to 2000 
# values. This is the newer, updated version of src/counterfactual-density.R, which
# will soon be deprecated.
# 
# The script is closely related to counterfactual-regional.R. This script produces
# the data underlying:
# - Table 3.1
# - Table 3.2
# in the Shiny app, while counterfactual-regional.R produces the data underlying
# Tables 3.3 and 3.4 in the shiny app.
#
# Rather than using all the demographic features at once, it layers them on one at
# a time. This allows us to conclude what happens with the introduction of each 
# individual layer and how robust the results are to various controls.
#
# These layered results are saved in .rda files that are placed in the shiny-app/data
# directory.
#
# Inputs:
#   - data/db/ipums.duckdb
#   - draws from function defined in src/utils/counterfactual-tools.R
# Outputs:
#   - shiny-app/data/counterfactuals.rda 
# TODO: Do a shiny-app wide audit of data names: right now, the RDA files are
# sillily named and it's basically the Wild West out here.
# 
# TODO: step 2, 2a, and 2b are identical between this script and counterfactual-regional.R.
# Figure out how potentially to move this data wrangling upstream.
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #

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

# ----- Step 3: Run counterfactuals ----- #
# These results are used to produce tables 3.1 and 3.2 (in tab 3 of the shiny app)
# relies on the calculate-counterfactual function, which is loaded in the 
# src/utils/counterfactual-tools.R

# List of scenarios
scenarios <- list(
  c("AGE_bucket"),
  c("SEX"),
  c("us_born"),
  c("EDUC"),
  c("INCTOT_cpiu_2010_bucket"),
  c("CPUMA0010"),
  c("RACE_ETH_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010")
)

# Generate data for all scenarios
nrow_pull <- 10000000
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()

# Persons per bedroom
bedroom_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019,
    p0_data = p0_sample,
    p1_data = p1_sample,
    outcome = "persons_per_bedroom"
    )$summary # Extract only the summary tibble
  )
)

# TODO: add a check in the function calculate_counterfactual that tests whether
# all of the potential categories are populated by data?
# motivated by my observation that smaller samples (e.g. 10,000) don't capture all
# CPUMA0010s, resulting in NA estimates.
# Persons per household
hhsize_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019, 
    p0_data = p0_sample, 
    p1_data = p1_sample,
    outcome = "NUMPREC"
    )$summary # Extract only the summary tibble
  )
)

#----- Step 4: Save the results to the Shiny app ----- #
# Counterfactuals
save(
  hhsize_cf, 
  bedroom_cf,
  file = "shiny-app/data/counterfactuals.rda"
)
