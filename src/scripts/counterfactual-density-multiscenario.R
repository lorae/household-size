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

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

# We're adding some simplified/binary variables for counterfactual calculations
ipums_db <- tbl(con, "ipums_processed") |>
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

# ----- Step 3: Functionalize counterfactual calculation ----- #

calculate_counterfactual <- function(
  cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
  p0_data = ipums_db |> filter(YEAR == 2005), # Data for the first (base) period
  p0_name = "2005", # A string name for period 0
  p1_data = ipums_db |> filter(YEAR == 2022), # Data for the second (recent) period
  p1_name = "2022" # A string name for period 1
  # TODO: P0 and P1 can each probably be simplified into one argument (year)
  # TODO: add back standard errors later. Not needed for now.
  ) {
  
  # TODO: add a step that catches errors if the specified data set is empty.
  # Note that this should be done at this level, but I'm also surprised the crosstab_mean
  # and crosstab_percent functions aren't producing errors when I do this.
  
  print(glue("Calculating {p0_name} means..."))
  mean_p0 <- crosstab_mean(
    data = p0_data,
    value = "NUMPREC",
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p0_name} means done!"))

  print(glue("Calculating {p1_name} means..."))
  mean_p1 <- crosstab_mean(
    data = p1_data,
    value = "NUMPREC",
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p1_name} means done!"))

  print(glue("Calculating {p1_name} percents..."))
  percent_p1 <- crosstab_percent(
    data = p1_data,
    wt_col = "PERWT",
    group_by = cf_categories,
    percent_group_by = c(),
    every_combo = TRUE
  ) |>
    select(-weighted_count, -count)
  print(glue("{p1_name} percents done!"))

  crosstab_p0_p1 <- 
    full_join(
      # Join the p0 and p1 mean data frames, appending a _{year} suffix to columns
      mean_p0 |> rename_with(~paste0(., "_", p0_name), -all_of(cf_categories)),
      mean_p1 |> rename_with(~paste0(., "_", p1_name), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    full_join(
      # Join the p0 and p1 mean data frames with p1 percent data frame, appending _{year} suffixes
      percent_p1 |> rename_with(~paste0(., "_", p1_name), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    arrange(across(all_of(cf_categories))) |>
    mutate(
      # Diff = period 1 household size minus period 0 household size
      diff = .data[[paste0("weighted_mean_", p1_name)]] - .data[[paste0("weighted_mean_", p0_name)]],
      # Calculate contributions (See Shiny app, main tab, for more explanation on contributions).
      cont_p1 = .data[[paste0("percent_", p1_name)]] * .data[[paste0("weighted_mean_", p1_name)]] / 100,
      cont_p1_cf = .data[[paste0("percent_", p1_name)]] * .data[[paste0("weighted_mean_", p0_name)]] / 100,
      contribution_diff = cont_p1 - cont_p1_cf,
    ) |>
    select(any_of(c(
      # Reorder columns more intuitively
      cf_categories, 
      paste0("count_", p0_name), paste0("count_", p1_name),
      paste0("weighted_count_", p0_name), paste0("weighted_count_", p1_name),
      paste0("percent_", p1_name), paste0("se_percent_", p1_name),
      paste0("weighted_mean_", p0_name), paste0("weighted_mean_", p1_name),
      "diff", 
      "contribution_diff"
    )))

  # Actual mean household size in period 1
  actual_hhsize_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p1_name)]] * .data[[paste0("percent_", p1_name)]] / 100)) |>
    pull(total)
  
  # Counterfactual household size in period 1
  cf_hhsize_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p0_name)]] * .data[[paste0("percent_", p1_name)]] / 100)) |>
    pull(total)
  
  print(glue("The actual average household size in {p1_name} is {actual_hhsize_p1}. The
           counterfactual in this scenario is {cf_hhsize_p1}."))
  
  return(crosstab_p0_p1)
}

# Test out the function...
calculate_counterfactual(c("RACE_ETH_bucket", "AGE_bucket")) -> x1
calculate_counterfactual(c("AGE_bucket")) -> x2
calculate_counterfactual(c("RACE_ETH_bucket")) -> x3
calculate_counterfactual(c("EDUC")) -> x4
calculate_counterfactual(c("SEX")) -> x5
calculate_counterfactual(c("us_born")) -> x6
# empstat? marst?

# calculate_counterfactual(c("CPUMA0010")) -> xx # doesn't work b/c no CPUMA0010 data for 2022
