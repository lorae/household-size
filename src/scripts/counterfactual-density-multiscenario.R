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

# Function to calculate the p-value from a two-sample t-test with different standard errors
# TODO: Make these names more general
calc_pval <- function(mean_2005, mean_2022, se_2005, se_2022) {
  z_score <- (mean_2022 - mean_2005) / sqrt(se_2005^2 + se_2022^2)
  pval <- 2 * pnorm(-abs(z_score))
  return(pval)
}
# ----- Step 3: Functionalize counterfactual calculation ----- #

calculate_counterfactual <- function(
  cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
  p0_data = ipums_db |> filter(YEAR == 2005), # Data for the first (base) period
  p0_name = "2005", # A string name for period 0
  p1_data = ipums_db |> filter(YEAR == 2022), # Data for the second (recent) period
  p1_name = "2022" # A string name for period 1
  # TODO: P0 and P1 can each probably be simplified into one argument (year)
  ) {
  
  # TODO: add a step that catches errors if the specified data set is empty.
  # Note that this should be done at this level, but I'm also surprised the crosstab_mean
  # and crosstab_percent functions aren't producing errors when I do this.
  
  print(glue("Calculating {p0_name} means..."))
  mean_p0 <- estimate_with_bootstrap_se(
    data = p0_data,
    f = crosstab_mean,
    value = "NUMPREC",
    wt_col = "PERWT",
    group_by = cf_categories,
    id_cols = cf_categories,
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
    constant = 4/80,
    se_cols = c("weighted_mean"),
    every_combo = TRUE
  ) 
  print(glue("{p0_name} means done!"))

  print(glue("Calculating {p1_name} means..."))
  mean_p1 <- estimate_with_bootstrap_se(
    data = p1_data,
    f = crosstab_mean,
    value = "NUMPREC",
    wt_col = "PERWT",
    group_by = cf_categories,
    id_cols = cf_categories,
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
    constant = 4/80,
    se_cols = c("weighted_mean"),
    every_combo = TRUE
  )
  print(glue("{p1_name} means done!"))

  print(glue("Calculating {p1_name} percents..."))
  percent_p1 <- estimate_with_bootstrap_se(
    data = p1_data,
    f = crosstab_percent,
    wt_col = "PERWT",
    group_by = cf_categories,
    percent_group_by = c(),
    id_cols = cf_categories,
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
    constant = 4/80,
    se_cols = c("percent"),
    every_combo = TRUE
  ) |> 
    select(-weighted_count, -count)
  print(glue("{p1_name} percents done!"))

  crosstab_p0_p1 <- 
    full_join(
      mean_p0 |> rename_with(~paste0(., "_", p0_name), -all_of(cf_categories)),
      mean_p1 |> rename_with(~paste0(., "_", p1_name), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    full_join(
      percent_p1 |> rename_with(~paste0(., "_", p1_name), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    arrange(across(all_of(cf_categories))) |>
    mutate(
      diff = .data[[paste0("weighted_mean_", p1_name)]] - .data[[paste0("weighted_mean_", p0_name)]],
      pval = mapply(
        calc_pval,
        .data[[paste0("weighted_mean_", p0_name)]],
        .data[[paste0("weighted_mean_", p1_name)]],
        .data[[paste0("se_weighted_mean_", p0_name)]],
        .data[[paste0("se_weighted_mean_", p1_name)]]
      ),
      sig = (pval <= 0.05),
      cont_p1 = .data[[paste0("percent_", p1_name)]] * .data[[paste0("weighted_mean_", p1_name)]] / 100,
      cont_p1_cf = .data[[paste0("percent_", p1_name)]] * .data[[paste0("weighted_mean_", p0_name)]] / 100,
      contribution_diff = cont_p1 - cont_p1_cf,
      mean_p0_95_ci = map2(
        .data[[paste0("weighted_mean_", p0_name)]],
        .data[[paste0("se_weighted_mean_", p0_name)]],
        ~ c(.x - qnorm(0.975) * .y, .x + qnorm(0.975) * .y)
      ),
      mean_p1_95_ci = map2(
        .data[[paste0("weighted_mean_", p1_name)]],
        .data[[paste0("se_weighted_mean_", p1_name)]],
        ~ c(.x - qnorm(0.975) * .y, .x + qnorm(0.975) * .y)
      )
    ) |>
    select(any_of(c(
      cf_categories, 
      paste0("count_", p0_name), paste0("count_", p1_name),
      paste0("weighted_count_", p0_name), paste0("weighted_count_", p1_name),
      paste0("percent_", p1_name), paste0("se_percent_", p1_name),
      paste0("weighted_mean_", p0_name), paste0("weighted_mean_", p1_name),
      paste0("se_weighted_mean_", p0_name), paste0("se_weighted_mean_", p1_name),
      paste0("mean_", p0_name, "_95_ci"), paste0("mean_", p1_name, "_95_ci"),
      "diff", "pval", "sig", "contribution_diff"
    )))

  actual_hhsize_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p1_name)]] * .data[[paste0("percent_", p1_name)]] / 100)) |>
    pull(total)
  
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
calculate_counterfactual(c("EDUC")) -> x5

# calculate_counterfactual(c("CPUMA0010")) -> x4 # doesn't work b/c no CPUMA0010 data for 2022
