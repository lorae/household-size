# counterfactual-density-multiscenario.R
#
# I hate to have two scripts so similarly named, but I want to build some new counter-
# factuals without affecting the old ones. This is the newer, updated version of 
# src/counterfactual-density.R. The purpose of this script is to calculate what -
# controlling for demographic factors - average person-level household size would be
# in 2022.
#
# Rather than using all the demographic features at once, it layers them on one at
# a time. This allows us to conclude what happens with the introduction of each 
# individual layer and how robust the results are to various controls.
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

# ----- Step 2a: Import CPI-U data ----- #
# EXTRA (transfer to process-ipums.R eventually): import CPI-U series
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

# ----- Step 3: Functionalize counterfactual calculation ----- #

calculate_counterfactual <- function(
  cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
  p0 = 2005, # An integer for the year of the first (base) period
  p1 = 2022, # An integer for the year of the second (recent) period
  outcome = "NUMPREC"
  # TODO: add back standard errors later. Not needed for now.
  ) {

  # Subset the data for each of the years
  p0_data = ipums_db |> filter(YEAR == p0) |> filter(GQ %in% c(0,1,2)) # Data for the first (base) period
  p1_data = ipums_db |> filter(YEAR == p1) |> filter(GQ %in% c(0,1,2)) # Data for the second (recent) period
  
  # TODO: add a step that catches errors if the specified data set is empty.
  # Note that this should be done at this level, but I'm also surprised the crosstab_mean
  # and crosstab_percent functions aren't producing errors when I do this.
  
  print(glue("Calculating {p0} means..."))
  mean_p0 <- crosstab_mean(
    data = p0_data,
    value = outcome,
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p0} means done!"))

  print(glue("Calculating {p1} means..."))
  mean_p1 <- crosstab_mean(
    data = p1_data,
    value = outcome,
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p1} means done!"))

  print(glue("Calculating {p1} percents..."))
  percent_p1 <- crosstab_percent(
    data = p1_data,
    wt_col = "PERWT",
    group_by = cf_categories,
    percent_group_by = c(),
    every_combo = TRUE
  ) |>
    select(-weighted_count, -count)
  print(glue("{p1} percents done!"))

  crosstab_p0_p1 <- 
    full_join(
      # Join the p0 and p1 mean data frames, appending a _{year} suffix to columns
      mean_p0 |> rename_with(~paste0(., "_", p0), -all_of(cf_categories)),
      mean_p1 |> rename_with(~paste0(., "_", p1), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    full_join(
      # Join the p0 and p1 mean data frames with p1 percent data frame, appending _{year} suffixes
      percent_p1 |> rename_with(~paste0(., "_", p1), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    mutate(
      # Apply rules for handling NA values
      across(
        all_of(paste0("weighted_mean_", p1)), 
        ~replace_na(., 0),
        .names = "{.col}"
      ),
      across(
        all_of(paste0("weighted_mean_", p0)),
        ~ifelse(is.na(.), .data[[paste0("weighted_mean_", p1)]], .),
        .names = "{.col}"
      )
    ) |>
    arrange(across(all_of(cf_categories))) |>
    mutate(
      # Diff = period 1 household size minus period 0 household size
      diff = .data[[paste0("weighted_mean_", p1)]] - .data[[paste0("weighted_mean_", p0)]],
      # Calculate contributions (See Shiny app, main tab, for more explanation on contributions).
      cont_p1 = .data[[paste0("percent_", p1)]] * .data[[paste0("weighted_mean_", p1)]] / 100,
      cont_p1_cf = .data[[paste0("percent_", p1)]] * .data[[paste0("weighted_mean_", p0)]] / 100,
      contribution_diff = cont_p1 - cont_p1_cf,
    ) |>
    select(any_of(c(
      # Reorder columns more intuitively
      cf_categories, 
      paste0("count_", p0), paste0("count_", p1),
      paste0("weighted_count_", p0), paste0("weighted_count_", p1),
      paste0("percent_", p1), paste0("se_percent_", p1),
      paste0("weighted_mean_", p0), paste0("weighted_mean_", p1),
      "diff", 
      "contribution_diff"
    )))

  # Actual mean household size in period 1
  actual_outcome_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p1)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  # Counterfactual household size in period 1
  cf_outcome_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p0)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  # Diff
  diff <- actual_outcome_p1 - cf_outcome_p1
  
  print(glue("The actual average {outcome} in {p1} is {actual_outcome_p1}. The
           counterfactual in this scenario is {cf_outcome_p1}. Actual - counterfactual = {diff}"))
  
  return(crosstab_p0_p1)
}

# Test out the function... 
# x1 <- calculate_counterfactual(cf_categories = c("RACE_ETH_bucket", "AGE_bucket"), p0 = 2000, p1 = 2023)
# x2 <- calculate_counterfactual(cf_categories = c("AGE_bucket"), p0 = 2000, p1 = 2023)
# x3 <- calculate_counterfactual(cf_categories = c("RACE_ETH_bucket"), p0 = 2000, p1 = 2023)
# x4 <- calculate_counterfactual(cf_categories = c("EDUC"), p0 = 2000, p1 = 2023)
# x5 <- calculate_counterfactual(cf_categories = c("SEX"), p0 = 2000, p1 = 2023)
# x6 <- calculate_counterfactual(cf_categories = c("us_born"), p0 = 2000, p1 = 2023)
# # empstat? marst?
# 
# calculate_counterfactual(cf_categories = c("CPUMA0010"), p0 = 2000, p1 = 2019)

# Do it incrementally

# ----- Step 4: Generate persons per household results ----- #
z1 <- calculate_counterfactual(
  cf_categories = c("AGE_bucket"), 
  p0 = 2000, p1 = 2019)
z2 <- calculate_counterfactual(
  cf_categories = c("SEX"), 
  p0 = 2000, p1 = 2019)
z5 <- calculate_counterfactual(
  cf_categories = c("us_born"),
  p0 = 2000, p1 = 2019)
z3 <- calculate_counterfactual(
  cf_categories = c("EDUC"), 
  p0 = 2000, p1 = 2019)
z4 <- calculate_counterfactual(
  cf_categories = c("INCTOT_cpiu_2010_bucket"),
  p0 = 2000, p1 = 2019)
z5 <- calculate_counterfactual(
  cf_categories = c("CPUMA0010"), 
  p0 = 2000, p1 = 2019)


y1 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket"), 
  p0 = 2000, p1 = 2019)
y2 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket"), 
  p0 = 2000, p1 = 2019)
y3 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX"), 
  p0 = 2000, p1 = 2019)
y4 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born"), 
  p0 = 2000, p1 = 2019)
y5 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC"), 
  p0 = 2000, p1 = 2019)
y6 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket"), 
  p0 = 2000, p1 = 2019)
y7 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010"), 
  p0 = 2000, p1 = 2019)

# ----- Step 5: Generate persons per bedroom results ----- #
a1 <- calculate_counterfactual(
  cf_categories = c("AGE_bucket"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
a2 <- calculate_counterfactual(
  cf_categories = c("SEX"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
a3 <- calculate_counterfactual(
  cf_categories = c("EDUC"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
a4 <- calculate_counterfactual(
  cf_categories = c("INCTOT_cpiu_2010_bucket"),
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
a5 <- calculate_counterfactual(
  cf_categories = c("CPUMA0010"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")

b1 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
b2 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
b3 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
b4 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
b5 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
b6 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
b7 <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010"), 
  p0 = 2000, p1 = 2019, outcome = "persons_per_bedroom")
