# The purpose of this test is to verify the accuracy of the Kitagawa-Oaxaca-Blinder 
# decomposition on an artificially produced data set before applying to real data.

library(tibble)
library(dplyr)
library(oaxaca)

# First, generate the synthetic data. There are going to be three data frames:
# input_all_coefficient, where the difference in average household size is explained
# fully by an underlying process that depends only on the is_black variable.

# Then there will be input_all_endowment where differences in average household
# size are explained fully by an underlying process that depends on the EDUC_bucket
# and HHINCOME_bucket variable, which varies systematically between the is_black = 1
# and is_black = 0 groups. But notably, NUMPREC is not generated at all as a direct
# function of is_black.

# Finally, there will be an input_some_discrimination where differences in average 
# household size are explained partially by an underlying process that depends on 
# EDUC_bucket and HHINCOME_bucket variables (which, as before, vary systematically
# based on 

set.seed(123)  # for reproducibility

# Function to draw positive integers from a normal distribution
draw_positive_ints <- function(n, mean, sd) {
  pmax(1, round(rnorm(n, mean, sd)))
}


# ----- "all_coefficient" data frame ----- #

# is_black = 0 group
n <- 50000
all_coefficient_not_black <- tibble(
  is_black = 0,
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
  AGE = sample(1:80, size = n, replace = TRUE)
)

# is_black = 1 group
all_coefficient_black <- tibble(
  is_black = 1,
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
  AGE = sample(1:80, size = n, replace = TRUE)
)

# Combine the groups
input_all_coefficient <- bind_rows(all_coefficient_not_black, all_coefficient_black)

# View the result
glimpse(input_all_coefficient)

# ---- Dummy coding EDUC_bucket and HHINCOME_bucket ----
input_all_coefficient_binary <- input_all_coefficient |>
  mutate(
    hs = as.integer(EDUC_bucket == "hs"),
    some_college = as.integer(EDUC_bucket == "some_college"),
    college_4yr_plus = as.integer(EDUC_bucket == "college_4yr_plus"),
    from_10k_to_100k = as.integer(HHINCOME_bucket == "from_10k_to_100k"),
    greater_than_100k = as.integer(HHINCOME_bucket == "greater_than_100k")
  )

# ----- try oaxaca on it

results_01 <- oaxaca(
  formula = NUMPREC ~ hs + some_college + college_4yr_plus + from_10k_to_100k + greater_than_100k |
    is_black |
    hs + some_college + college_4yr_plus,
  data = input_all_coefficient_binary
)

results_01$y
results_01$threefold$overall
plot(results_01, components = c("endowments","coefficients"))

# In this pre-programmed case of total discrimination, you can see that over many
# iterations, the actual difference of 0.94 is almost identica to the difference in 
# coefficients, which is also 0.94, and the other two coefficients are statistically 
# indistinguishable from 0 at a p = 0.05 level (I'm approximating by multiplying the
# SE by 1.96 and adding / subtracting from the result.)

# The plot gives special insight. The way the data was designed, the only hard-coded
# differences were between black and not_black populations. All the remaining 
# covariates remained 


# ----- "all_endowment" data frame ----- #

# Now we produce data where all of the difference between the is_black = 0 and 
# is_black = 1 groups arises solely through the mechanism of within-group differences in
# distribution of education and income.

# Set up parameters
n_small <- 500000  # per group
education_levels <- c("less_than_hs", "hs", "some_college", "college_4yr_plus")
income_levels <- c("less_than_10k", "from_10k_to_100k", "greater_than_100k")

# Turn into ordered factors
EDUC_levels <- factor(education_levels, levels = education_levels, ordered = TRUE)
HHINCOME_levels <- factor(income_levels, levels = income_levels, ordered = TRUE)

# Define helper to assign EDUC and INCOME using group-specific probabilities
generate_group <- function(is_black, edu_probs, inc_probs) {
  tibble(
    is_black = is_black,
    EDUC_bucket = factor(
      sample(education_levels, size = n_small, replace = TRUE, prob = edu_probs),
      levels = education_levels,
      ordered = TRUE
    ),
    HHINCOME_bucket = factor(
      sample(income_levels, size = n_small, replace = TRUE, prob = inc_probs),
      levels = income_levels,
      ordered = TRUE
    )
  ) |>
    mutate(
      # Convert factor levels to numeric 1, 2, 3, ...
      EDUC_num = as.integer(EDUC_bucket),
      HHINCOME_num = as.integer(HHINCOME_bucket),
      # Compute NUMPREC: 8 - (EDUC_num + HHINCOME_num), then sample with that mean
      mean_household_size = 8 - (EDUC_num + HHINCOME_num),
      NUMPREC = draw_positive_ints(n_small, mean = mean_household_size, sd = 1.5),
      AGE = sample(1:80, size = n_small, replace = TRUE)
    ) |>
    select(is_black, NUMPREC, EDUC_bucket, HHINCOME_bucket, AGE)
}

# Not Black group: more favorable distribution
group_not_black <- generate_group(
  is_black = 0,
  edu_probs = c(0.1, 0.3, 0.3, 0.3),
  inc_probs = c(0.2, 0.6, 0.2)
)

# Black group: less favorable distribution
group_black <- generate_group(
  is_black = 1,
  edu_probs = c(0.2, 0.3, 0.3, 0.2),
  inc_probs = c(0.3, 0.6, 0.1)
)

# Combine into full dataset
input_all_endowment <- bind_rows(group_not_black, group_black)

# Quick look
glimpse(input_all_endowment)

# ---- Dummy coding EDUC_bucket and HHINCOME_bucket ----
input_all_coefficient_binary <- input_all_endowment |>
  mutate(
    hs = as.integer(EDUC_bucket == "hs"),
    some_college = as.integer(EDUC_bucket == "some_college"),
    college_4yr_plus = as.integer(EDUC_bucket == "college_4yr_plus"),
    less_than_10k = as.integer(HHINCOME_bucket == "less_than_10k"),
    from_10k_to_100k = as.integer(HHINCOME_bucket == "from_10k_to_100k"),
    greater_than_100k = as.integer(HHINCOME_bucket == "greater_than_100k")
  )

# ----- try oaxaca on it

results_02 <- oaxaca(
  formula = NUMPREC ~ hs + some_college + college_4yr_plus + from_10k_to_100k + greater_than_100k |
    is_black |
    from_10k_to_100k + greater_than_100k,
    # hs + some_college + college_4yr_plus,
  data = input_all_coefficient_binary,
  R = NULL # no bootstrapped SEs
)

results_02$y
results_02$threefold$overall
plot(results_02, components = c("endowments","coefficients"))

