# The purpose of this script is to produce fast facts that are used in the draft 
# version of this paper. 
# Last modified mid-February 2025.

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

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 3a: Fast facts! ----- #
# Overall household size, group quarters excluded, in 2000 and 2019.

# Function to compute weighted household size
get_overall_hhsize <- function(data, year) {
  data_filtered <- data |> filter(YEAR == year) |> filter(GQ %in% c(0,1,2))

  crosstab_mean(
    data = data_filtered,
    value = "NUMPREC",
    wt_col = "PERWT",
    group_by = c(),
    every_combo = FALSE
  ) |> pull(weighted_mean)
}

# Compute household sizes for multiple years
years <- c(2000, 2019)
overall_hhsize_results <- tibble(
  year = years,
  hhsize = map_dbl(year, ~ get_overall_hhsize(ipums_db, .x))
)

hhsize_2000 <- overall_hhsize_results |> filter(year == 2000) |> pull(hhsize)
hhsize_2019 <- overall_hhsize_results |> filter(year == 2019) |> pull(hhsize)

hhsize_pctchg_2000_2019 <- (hhsize_2019 - hhsize_2000) / hhsize_2000 * 100 # pct change in hhsize from 2000 to 2019
   
# ----- Step 3b: Fast facts! ----- #
# Household size by race/ethnicity, group quarters excluded, in 2000 and 2019.  
   
   
   