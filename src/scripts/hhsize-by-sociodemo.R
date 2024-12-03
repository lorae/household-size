# hhsize-by-sociodemo.R
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

# Function to calculate the p-value from a two-sample t-test with different standard errors
calc_pval <- function(mean_2005, mean_2022, se_2005, se_2022) {
  z_score <- (mean_2022 - mean_2005) / sqrt(se_2005^2 + se_2022^2)
  pval <- 2 * pnorm(-abs(z_score))
  return(pval)
}

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
  group_by = c("AGE_bucket", "RACE_ETH_bucket"),
  id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("weighted_mean"),
  every_combo = TRUE
) |> 
  mutate(AGE_bucket = factor(AGE_bucket, levels = age_factor_levels)) |>
  arrange(RACE_ETH_bucket, AGE_bucket)

mean2022 <- estimate_with_bootstrap_se(
  data = ipums_db |> filter(YEAR == 2022),
  f = crosstab_mean,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("AGE_bucket", "RACE_ETH_bucket"),
  id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("weighted_mean"),
  every_combo = TRUE
) |> 
  mutate(AGE_bucket = factor(AGE_bucket, levels = age_factor_levels)) |>
  arrange(RACE_ETH_bucket, AGE_bucket)

percent2022 <- estimate_with_bootstrap_se(
  data = ipums_db |> filter(YEAR == 2022),
  f = crosstab_percent,
  wt_col = "PERWT",
  group_by = c("AGE_bucket", "RACE_ETH_bucket"),
  percent_group_by = c(),
  id_cols = c("AGE_bucket", "RACE_ETH_bucket"),
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  every_combo = TRUE
) |> 
  mutate(AGE_bucket = factor(AGE_bucket, levels = age_factor_levels)) |>
  arrange(RACE_ETH_bucket, AGE_bucket) |>
  select(-weighted_count, -count)


crosstab_2005_2022 <- full_join(
  mean2005 |> rename_with(~paste0(., "_2005"), -c(AGE_bucket, RACE_ETH_bucket)),
  mean2022 |> rename_with(~paste0(., "_2022"), -c(AGE_bucket, RACE_ETH_bucket)),
  by = c("AGE_bucket", "RACE_ETH_bucket")
) |>
  # TODO: build in a check that the count, weighted_count from percent2022 equal the 
  # count, weighted_count from mean2022
  full_join(
    percent2022 |> rename_with(~paste0(., "_2022"), -c(AGE_bucket, RACE_ETH_bucket)),
    by = c("AGE_bucket", "RACE_ETH_bucket")
  )

bonferroni_corrector <- nrow(crosstab_2005_2022)

crosstab_2005_2022 <- crosstab_2005_2022 |>
  mutate(
    # Difference between 2022 and 2005 weighted means
    diff = weighted_mean_2022 - weighted_mean_2005,
    
    # Calculate the p-value using the function above
    pval = mapply(
      calc_pval,
      weighted_mean_2005, 
      weighted_mean_2022, 
      se_weighted_mean_2005, 
      se_weighted_mean_2022
    ),
    
    # Significant if pval <= 0.05
    sig = (pval <= 0.05),
    
    # Bonferroni correction: Significant if pval <= 0.05 / (number of comparisons made) 
    sig_bonferroni = pval <= 0.05 / bonferroni_corrector,
    
    # Calculation the acutal contribution toward 2022 household size, the contribution
    # toward counterfactual household size (had population proporitons been at 2022
    # levels but weights at 2005 levels), and the difference between the two
    cont_2022 = percent_2022 * weighted_mean_2022 / 100,
    cont_2022_cf = percent_2022 * weighted_mean_2005 / 100,
    contribution_diff = cont_2022 - cont_2022_cf,
    
    mean_2005_95_ci = map2(
      weighted_mean_2005, 
      se_weighted_mean_2005, 
      ~ c(.x - qnorm(0.975) * .y, .x + qnorm(0.975) * .y)
    ),
    
    mean_2022_95_ci = map2(
      weighted_mean_2022, 
      se_weighted_mean_2022, 
      ~ c(.x - qnorm(0.975) * .y, .x + qnorm(0.975) * .y)
    )
  ) |>
  select(
    RACE_ETH_bucket,
    AGE_bucket,
    count_2005,
    count_2022,
    weighted_count_2005,
    weighted_count_2022,
    percent_2022,
    se_percent_2022,
    weighted_mean_2005,
    weighted_mean_2022,
    se_weighted_mean_2005,
    se_weighted_mean_2022,
    mean_2005_95_ci,
    mean_2022_95_ci,
    diff,
    pval,
    sig,
    sig_bonferroni,
    contribution_diff
    )

contributions <- crosstab_2005_2022 |>
  select(
    RACE_ETH_bucket,
    AGE_bucket,
    percent_2022,
    weighted_mean_2005,
    weighted_mean_2022
  ) |>
  mutate(
    cont_2005 = (percent_2022 / 100) * weighted_mean_2005,
    cont_2022 = (percent_2022 / 100) * weighted_mean_2022,
    cont_2022cf = (percent_2022 / 100) * weighted_mean_2005,
    cont_diff = cont_2022 - cont_2022cf,
    cont_diff_pct = cont_diff / sum(cont_diff)
  )

cont.tmp <- contributions |>
  group_by(RACE_ETH_bucket) %>%
  summarize(cont_diff_sum = sum(cont_diff, na.rm = TRUE))

# Precompute data for the data table
data_for_table <- crosstab_2005_2022 %>%
  mutate(
    weighted_mean_2005 = round(weighted_mean_2005, 3),
    weighted_mean_2022 = round(weighted_mean_2022, 3),
    diff = round(diff, 3),
    percent_2022 = round(percent_2022, 3)
  ) %>%
  select(
    -mean_2005_95_ci, -mean_2022_95_ci, -se_percent_2022,
    -se_weighted_mean_2005, -se_weighted_mean_2022, -contribution_diff
  )

# Precompute long format data for the household size plot
data_long <- crosstab_2005_2022 %>%
  select(
    RACE_ETH_bucket, AGE_bucket, weighted_mean_2005,
    weighted_mean_2022, se_weighted_mean_2005, se_weighted_mean_2022
  ) %>%
  pivot_longer(
    cols = c(
      "weighted_mean_2005", "weighted_mean_2022",
      "se_weighted_mean_2005", "se_weighted_mean_2022"
    ),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(
    year = factor(year, levels = c("2005", "2022")),
    AGE_bucket = factor(AGE_bucket, levels = unique(AGE_bucket))
  )



# Precompute summary contributions for the summary waterfall chart
cont_summary <- contributions %>%
  group_by(RACE_ETH_bucket) %>%
  summarize(cont_diff_sum = sum(cont_diff, na.rm = TRUE)) %>%
  mutate(
    end_bar = cumsum(cont_diff_sum),
    start_bar = lag(end_bar, default = 0)
  )

# ----- Step 3: Save results and clean up ----- #

save(
  crosstab_2005_2022, 
  contributions, 
  cont.tmp, 
  data_for_table, 
  data_long, 
  cont_summary, 
  file = "shiny-app/data/all_tables.rda"
)

DBI::dbDisconnect(con)

