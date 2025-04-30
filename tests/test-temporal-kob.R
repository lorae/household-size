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
library("oaxaca")
library("tibble")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

model_2000 <- lm(data = ipums_db |> filter(YEAR == 2000 & GQ %in% c(0,1,2)),
   weights = PERWT,
   formula = NUMPREC ~ RACE_ETH_bucket + AGE_bucket + SEX + us_born + EDUC_bucket + INCTOT_cpiu_2010_bucket + OWNERSHP + CPUMA0010)

model_2019 <- lm(data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
  weights = PERWT,
  formula = NUMPREC ~ RACE_ETH_bucket + AGE_bucket + SEX + us_born + EDUC_bucket + INCTOT_cpiu_2010_bucket + OWNERSHP + CPUMA0010)


weighted_oaxaca <- function() {
  
}

############################ NEW STUFF

set.seed(123)  # for reproducibility
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# Function to draw positive integers from a normal distribution
draw_positive_ints <- function(n, mean, sd) {
  pmax(1, round(rnorm(n, mean, sd)))
}


# ----- "all_coefficient" data frame ----- #
# Manufacture synthetic data
n <- 50000

education_levels <- c("less_than_hs", "hs", "some_college", "college_4yr_plus")
income_levels <- c("less_than_10k", "from_10k_to_100k", "greater_than_100k")

# 2000 data
input_2000 <- tibble(
  year = 2000,
  NUMPREC = draw_positive_ints(n, mean = 3, sd = 1.5),
  EDUC_bucket = sample(
    c("less_than_hs", "hs", "some_college", "college_4yr_plus"),
    size = n,
    replace = TRUE,
    prob = c(0.1, 0.3, 0.3, 0.3)
  ),
  HHINCOME_bucket = sample(
    c("less_than_10k", "from_10k_to_100k", "greater_than_100k"),
    size = n,
    replace = TRUE,
    prob = c(0.2, 0.6, 0.2)
  ),
  AGE = sample(1:80, size = n, replace = TRUE),
  PERWT = 1
) |>
  mutate(
    EDUC_bucket = factor(EDUC_bucket, levels = education_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = income_levels, ordered = TRUE)
  )

# 2019 data
input_2019 <- tibble(
  year = 2019,
  NUMPREC = draw_positive_ints(n, mean = 4, sd = 1.5),
  EDUC_bucket = sample(
    c("less_than_hs", "hs", "some_college", "college_4yr_plus"),
    size = n,
    replace = TRUE,
    prob = c(0.2, 0.3, 0.3, 0.2)
  ),
  HHINCOME_bucket = sample(
    c("less_than_10k", "from_10k_to_100k", "greater_than_100k"),
    size = n,
    replace = TRUE,
    prob = c(0.3, 0.6, 0.1)
  ),
  AGE = sample(1:80, size = n, replace = TRUE), 
  PERWT = 1
) |>
  mutate(
    EDUC_bucket = factor(EDUC_bucket, levels = education_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = income_levels, ordered = TRUE)
  )

# Combine the groups
input_all <- bind_rows(input_2000, input_2019)

# View the result
glimpse(input_all)

source("src/utils/counterfactual-tools.R")


adjust_coefs_relative_to <- function(coef_df, ref_name) {
  # Extract reference values
  ref_2000 <- coef_df$mean_2000[coef_df$name == ref_name]
  ref_2019 <- coef_df$mean_2019[coef_df$name == ref_name]
  
  if (length(ref_2000) == 0 || length(ref_2019) == 0) {
    stop("Reference name not found in coef_df.")
  }
  
  # Create adjusted columns
  coef_df <- coef_df %>%
    mutate(
      coef_adj_2000 = mean_2000 - ref_2000,
      coef_adj_2019 = mean_2019 - ref_2019
    )
  
  # Add new "intercept" row using the actual reference values
  intercept_row <- tibble(
    name = "intercept",
    mean_2000 = 0,
    mean_2019 = 0,
    coef_adj_2000 = ref_2000,
    coef_adj_2019 = ref_2019
  )
  
  # Combine intercept with rest of the data
  bind_rows(intercept_row, coef_df)
}


# Step 1: Fit models and extract coefficients
x <- lm(data = input_2000, formula = NUMPREC ~ 1)
y <- lm(data = input_2019, formula = NUMPREC ~ 1)

model_2000 <- lm(data = input_2000, formula = NUMPREC ~ -1 + EDUC_bucket:HHINCOME_bucket)
model_2019 <- lm(data = input_2019, formula = NUMPREC ~ -1 + EDUC_bucket:HHINCOME_bucket)

coef_df <- full_join(
  enframe(model_2000$coefficients, name = "name", value = "mean_2000"),
  enframe(model_2019$coefficients, name = "name", value = "mean_2019"),
  by = "name"
)

# Step 2: Extract bucket levels from the name field
coef_df <- coef_df %>%
  mutate(
    EDUC_bucket = str_extract(name, "EDUC_bucket[^:]+") %>% str_remove("EDUC_bucket"),
    HHINCOME_bucket = str_extract(name, "HHINCOME_bucket.+") %>% str_remove("HHINCOME_bucket")
  )

# Step 3: Compute weighted counts for 2000 and 2019
weighted_2000 <- input_2000 %>%
  group_by(EDUC_bucket, HHINCOME_bucket) %>%
  summarise(weighted_count_2000 = sum(PERWT), .groups = "drop")

weighted_2019 <- input_2019 %>%
  group_by(EDUC_bucket, HHINCOME_bucket) %>%
  summarise(weighted_count_2019 = sum(PERWT), .groups = "drop")

# Step 4: Join both weighted counts into coef_df
coef_df <- coef_df %>%
  left_join(weighted_2000, by = c("EDUC_bucket", "HHINCOME_bucket")) %>%
  left_join(weighted_2019, by = c("EDUC_bucket", "HHINCOME_bucket"))

# Step 5: add the prop cols
coef_df <- coef_df |>
  mutate(
    prop_2000 = weighted_count_2000 / sum(weighted_count_2000),
    prop_2019 = weighted_count_2019 / sum(weighted_count_2019)
  )

coef <- adjust_coefs_relative_to(
  coef_df,
  ref_name = "EDUC_bucketless_than_hs:HHINCOME_bucketless_than_10k"
)




intercept_2000 <- coef |> filter(name == "intercept") |> pull(coef_adj_2000)
intercept_2019 <- coef |> filter(name == "intercept") |> pull(coef_adj_2019)

coef <- coef |>
  mutate(
    e_component = coef_adj_2019*(prop_2019 - prop_2000),
    c_component = (coef_adj_2019 - coef_adj_2000)*prop_2000
  )

u <- intercept_2019 - intercept_2000
e <- sum(coef$e_component, na.rm = TRUE)
c <- sum(coef$c_component, na.rm = TRUE)
u
e
c

sum(u,e,c)

# Halleleujah!



interaction_dummies <- model.matrix(
  ~ EDUC_bucket:HHINCOME_bucket - 1,  # '-1' removes intercept, so all combinations get their own column
  data = input_all
)
input_all_dummies <- cbind(input_all, interaction_dummies) |>
  mutate(
    is_2000 = if_else(year == 2000, 1, 0)
  )


oaxaca_result <- oaxaca(
  data = input_all_dummies,
  formula = NUMPREC ~ 
    #`EDUC_bucketless_than_hs:HHINCOME_bucketless_than_10k` +
    `EDUC_buckeths:HHINCOME_bucketless_than_10k` +
    `EDUC_bucketsome_college:HHINCOME_bucketless_than_10k` +
    `EDUC_bucketcollege_4yr_plus:HHINCOME_bucketless_than_10k` +
    `EDUC_bucketless_than_hs:HHINCOME_bucketfrom_10k_to_100k` +
    `EDUC_buckeths:HHINCOME_bucketfrom_10k_to_100k` +
    `EDUC_bucketsome_college:HHINCOME_bucketfrom_10k_to_100k` +
    `EDUC_bucketcollege_4yr_plus:HHINCOME_bucketfrom_10k_to_100k` +
    `EDUC_bucketless_than_hs:HHINCOME_bucketgreater_than_100k` +
    `EDUC_buckeths:HHINCOME_bucketgreater_than_100k` +
    `EDUC_bucketsome_college:HHINCOME_bucketgreater_than_100k` +
    `EDUC_bucketcollege_4yr_plus:HHINCOME_bucketgreater_than_100k`
  | is_2000,
  R = NULL
)
oaxaca_result$y
