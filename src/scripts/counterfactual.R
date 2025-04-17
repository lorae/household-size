# The purpose of this script is to calculate all counterfactuals and save
# the results to the throughput folder for further analysis
# counterfactual-multiscenario.R
#
# This script is meant to eventually supplant all the previous counterfactual scripts:
# counterfactual-multiscenario, counterfactual-density, counterfactual-regional.
# As of mid-April 2025 this is a top priority for Lorae and a work in progress.
#
# These layered results are saved in .rda files that are placed in the shiny-app/data
# directory. # oh really?  I didn't now this
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
library("ggplot2")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# TODO: eventually write a function in dataduck that when buckets are created,
# the code automatically writes a list of vectors containing factor
# labels. For now, I'm just generating factor labels directly from the lookup
# table here, but this code is more brittle since it relies on me remembering
# which lookup table I used.
age_factor_levels <- extract_factor_label(
  lookup_table = read.csv("lookup_tables/age/age_buckets01.csv"),
  colname = "bucket_name"
)

# ----- Step 3: Run counterfactuals ----- #
# These results are used to produce tables 3.1 and 3.2 (in tab 3 of the shiny app)
# relies on the calculate-counterfactual function, which is loaded in the 
# src/utils/counterfactual-tools.R

# List of scenarios
scenarios <- list(
  scen01 = c("AGE_bucket"),
  scen02 = c("SEX"),
  scen03 = c("us_born"),
  scen04 = c("EDUC_bucket"),
  scen05 = c("INCTOT_cpiu_2010_bucket"),
  scen06 = c("OWNERSHP"),
  scen07 = c("CPUMA0010"),
  scen08 = c("RACE_ETH_bucket"),
  scen09 = c("RACE_ETH_bucket", "AGE_bucket"),
  scen10 = c("RACE_ETH_bucket", "AGE_bucket", "SEX"),
  scen11 = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born"),
  scen12 = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket"),
  scen13 = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket"),
  scen14 = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP"),
  scen15 = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010")
)

# Output directory
output_dir <- "data/counterfactual-results"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Generate data for all scenarios
nrow_pull <- 10000000
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()

# Loop over named list of scenarios
walk2(scenarios, names(scenarios), function(cf_vars, scen_name) {
  result <- calculate_counterfactual(
    cf_categories = cf_vars, 
    p0 = 2000, 
    p1 = 2019, 
    p0_data = p0_sample, 
    p1_data = p1_sample,
    outcome = "NUMPREC"
  )
  
  save(result, file = glue("{output_dir}/{scen_name}.rda"))
})

# ----- Quality checking the counterfactuals

load("data/counterfactual-results/scen15.rda")

cf <- result$contributions

# A function which outputs the counterfactual diff given input of the cf table
# filtered for a certain group. Example usages below.
in_group_cf <- function(
  cf  
) {
  total_diff = cf |> pull(contribution_diff) |> sum()
  total_prop_2019 = (cf |> pull(percent_2019) |> sum()) / 100
  
  return(total_diff / total_prop_2019)
}

# This says that the average white person lives in a household with 0.129 more persons 
# than predicted using 2000 means
in_group_cf(cf |> filter(RACE_ETH_bucket == "White"))
# ... etc.
in_group_cf(cf |> filter(RACE_ETH_bucket == "Black"))
in_group_cf(cf |> filter(RACE_ETH_bucket == "Hispanic"))

# We can also do it by tenure (homeowner or renter)
in_group_cf(cf |> filter(OWNERSHP == 1)) # Homeowner
in_group_cf(cf |> filter(OWNERSHP == 2)) # Renter

# Or more fine-grained ... eg by income
in_group_cf(cf |> filter(INCTOT_cpiu_2010_bucket == "over 100k" & RACE_ETH_bucket == "White")) 
in_group_cf(cf |> filter(INCTOT_cpiu_2010_bucket == "30k to 100k" & RACE_ETH_bucket == "White"))
in_group_cf(cf |> filter(INCTOT_cpiu_2010_bucket == "10 to 30k" & RACE_ETH_bucket == "White"))
in_group_cf(cf |> filter(INCTOT_cpiu_2010_bucket == "under 10k" & RACE_ETH_bucket == "White"))
in_group_cf(cf |> filter(INCTOT_cpiu_2010_bucket == "0" & RACE_ETH_bucket == "White"))
in_group_cf(cf |> filter(INCTOT_cpiu_2010_bucket == "neg" & RACE_ETH_bucket == "White"))
# TODO: some really interesting tables could come from this, once I quality check
# the results.
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Define races
races <- c("All", "Black", "White", "Hispanic")

# Define row groupings
tenure_categories <- c("Renter" = 2, "Homeowner" = 1)
education_categories <- c(
  "Less than HS" = "less_than_hs",
  "High school" = "hs",
  "Some college" = "some_college",
  "College 4yr+" = "college_4yr+"
)
income_categories <- c(
  "<0" = "neg",
  "0" = "0",
  "Under 10k" = "under 10k",
  "10 to 30k" = "10 to 30k",
  "30 to 100k" = "30k to 100k",
  "Over 100k" = "over 100k"
)

# Helper to compute values for all races given a filter condition
get_values_by_race <- function(data, filter_expr) {
  map_dbl(races, function(race) {
    filtered <- data |> filter(!!rlang::enquo(filter_expr))
    if (race != "All") {
      filtered <- filtered |> filter(RACE_ETH_bucket == race)
    }
    in_group_cf(filtered)
  }) |> set_names(races)
}

# Tenure Table
tenure_table <- map_dfr(
  names(tenure_categories),
  function(label) {
    values <- get_values_by_race(cf, OWNERSHP == tenure_categories[[label]])
    tibble(Group = label, !!!values)
  }
)

# Education Table
education_table <- map_dfr(
  names(education_categories),
  function(label) {
    values <- get_values_by_race(cf, EDUC_bucket == education_categories[[label]])
    tibble(Group = label, !!!values)
  }
)

# Income Table
income_table <- map_dfr(
  names(income_categories),
  function(label) {
    values <- get_values_by_race(cf, INCTOT_cpiu_2010_bucket == income_categories[[label]])
    tibble(Group = label, !!!values)
  }
)

# Overall Table
overall_values <- c(
  All      = in_group_cf(cf),
  Black    = in_group_cf(cf |> filter(RACE_ETH_bucket == "Black")),
  White    = in_group_cf(cf |> filter(RACE_ETH_bucket == "White")),
  Hispanic = in_group_cf(cf |> filter(RACE_ETH_bucket == "Hispanic"))
)

overall_table <- tibble(
  Group    = "Overall",
  All      = overall_values["All"],
  Black    = overall_values["Black"],
  White    = overall_values["White"],
  Hispanic = overall_values["Hispanic"]
)

income_table
education_table
tenure_table
overall_table
# Save to CSVs
# write_csv(tenure_table, "tenure_by_race.csv")
# write_csv(education_table, "education_by_race.csv")
# write_csv(income_table, "income_by_race.csv")

