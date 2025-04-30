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


x <- lm(data = input_2000,
   formula = NUMPREC ~ 1) # Think floating point rounding is to blame
# This matches my results exactly in the below calculate_counterfactual
y <- lm(data = input_2019,
   formula = NUMPREC ~ 1) 

w_df <- enframe(w$coefficients, name = "name", value = "w_coef")
z_df <- enframe(z$coefficients, name = "name", value = "z_coef")

coef_df <- full_join(w_df, z_df, by = "name")

## TODO: add percent_2000 to this function, and also make the percent_2019 and 
## percent_2000 columns instead be prop_2019 and prop_2000
result <- calculate_counterfactual(
    cf_categories = c("EDUC_bucket", "HHINCOME_bucket"), # A vector of string names for the group_by variable 
    p0 = 2000, # An integer for the year of the first (base) period
    p1 = 2019, # An integer for the year of the second (recent) period
    p0_data = input_2000, # Data for period 0
    p1_data = input_2019, # Data for period 1
    outcome = "NUMPREC" # Name of the outcome variable.
    # TODO: add back standard errors later. Not needed for now.
)

kob_result <- result$contributions |>
  mutate(
    int_2000 = if_else(row_number() == 1, weighted_mean_2000, NA),
    coef_2000 = if_else(row_number() != 1, weighted_mean_2000 - int_2000[1], NA),
    int_2019 = if_else(row_number() == 1, weighted_mean_2019, NA),
    coef_2019 = if_else(row_number() != 1, weighted_mean_2019 - int_2019[1], NA),
  ) |>
  mutate(
    prop_2000 = weighted_mean_2000 / sum(weighted_mean_2000),
    prop_2019 = percent_2019 / 100
  )
  

cf <- result$summary |> pull(cf_final)
init <- result$summary |> pull(actual_init)
final <- result$summary |> pull(actual_final)
diff = final - init
init
final
diff

u <- (kob_result[1,] |> pull(int_2019)) - (kob_result[1,] |> pull(int_2000))
c <- sum(kob_result$prop_2000*(kob_result$coef_2019 - kob_result$coef_2000), na.rm = TRUE)
e <- sum(kob_result$coef_2019*(kob_result$prop_2019 - kob_result$prop_2000), na.rm = TRUE)
u
c
e


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
