# counterfactual-density.R
#
# This script produces two large summary data frames. Each one has one row per
# unique combination of AGE_bucket, SEX, and RACE_ETH_bucket. It then includes
# data for the average household size, the number of person-level observations
# and weighted person-level observations producing that metric, and the standard
# error of the household size mean.
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

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

mean2005 <- estimate_with_bootstrap_se(
  data = ipums_db |> filter(YEAR == 2005),
  f = crosstab_mean,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("CPUMA0010"),
  id_cols = c("CPUMA0010"),
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("weighted_mean"),
  every_combo = TRUE
)  

# ----- Step X: Clean up ----- #
DBI::dbDisconnect(con)
