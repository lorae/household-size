# test-calculate-counterfactual
# ----- Step 0: Workspace setup ----- #

library("testthat")
library("dplyr")
library("rlang")
library("duckdb")
library("rprojroot")
library("glue")
library("purrr")
library("tidyr")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# Load the dataduck package
devtools::load_all("../dataduck")

# Define the function that is being tested
# TODO: This function needs to be modularized
calculate_counterfactual <- function(
    cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
    p0 = 2000, # An integer for the year of the first (base) period
    p1 = 2019, # An integer for the year of the second (recent) period
    p0_data, # Data for period 0
    p1_data, # Data for period 1
    outcome = "NUMPREC" # Name of the outcome variable.
    # TODO: add back standard errors later. Not needed for now.
) {
  
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
  
  # Calculate overall values
  actual_outcome_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p1)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  cf_outcome_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p0)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  # Return structured data frame
  tibble(
    RACE_ETH_bucket = ("RACE_ETH_bucket" %in% cf_categories),
    AGE_bucket = ("AGE_bucket" %in% cf_categories),
    SEX = ("SEX" %in% cf_categories),
    us_born = ("us_born" %in% cf_categories),
    EDUC = ("EDUC" %in% cf_categories),
    INCTOT_cpiu_2010_bucket = ("INCTOT_cpiu_2010_bucket" %in% cf_categories),
    CPUMA0010 = ("CPUMA0010" %in% cf_categories),
    counterfactual = cf_outcome_p1,
    actual = actual_outcome_p1,
    diff = actual_outcome_p1 - cf_outcome_p1
  )
}

# ----- Step 1: Create test inputs ----- #

# Test tibble inputs
data_2000_tb <- tibble(
  SEX = c(1, 1, 1, 2, 2),
  value = c(2, 3, 2, 5, 6),
  PERWT = c(100, 80, 90, 110, 120),
)

data_2019_tb <- tibble(
  SEX = c(1, 2, 1, 2, 2),
  value = c(2, 8, 3, 7, 9),
  PERWT = c(80, 100, 70, 120, 110),
)

# ----- Step 2: Try the function ----- #

calculate_counterfactual(
  cf_categories = c("SEX"), 
  p0 = 2000, 
  p1 = 2019, 
  p0_data = data_2000_tb,
  p1_data = data_2019_tb,
  outcome = "value"
)

weighted.mean(x = data_2019_tb$value, w = data_2019_tb$PERWT)

# That looks correct.

# ----- Step 3: Try the function on real data ----- #

nrow_pull <- 15000000

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

sample_2000_db <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) |> head(nrow_pull)
sample_2000_tb <- sample_2000_db |> collect()
sample_2019_db <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) |> head(nrow_pull)
sample_2019_tb <- sample_2019_db |> collect()

cf <- calculate_counterfactual(
  cf_categories = c("SEX"), 
  p0 = 2000, 
  p1 = 2019, 
  p0_data = sample_2000_db,
  p1_data = sample_2019_db,
  outcome = "NUMPREC"
)

mean_1 <- cf$actual[1]
mean_2 <- weighted.mean(x = sample_2019_tb$NUMPREC, w = sample_2019_tb$PERWT)

success <- abs(mean_1 - mean_2) < 0.0001
if(success) {
  print("The test passed.")
} else {
  print(glue("The test failed. The mean according to the calculate_counterfactual function
  is {mean_1} but the mean according to the weighted.mean function is {mean_2}."))
}