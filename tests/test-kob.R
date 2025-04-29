# The purpose of this test is to verify the accuracy of the Kitagawa-Oaxaca-Blinder 
# decomposition on an artificially produced data set before applying to real data.

library(tibble)
library(dplyr)
library(oaxaca)

# First, generate the synthetic data. There are going to be three data frames:
# input_all_discrimination, where the difference in average household size is explained
# fully by an underlying process that depends only on the is_black bariable.

# Then there will be input_no_discrimination where differences in average household
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


# ----- "all_discrimination" data frame ----- #

# is_black = 0 group
n <- 50000
all_discrimination_not_black <- tibble(
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
all_discrimination_black <- tibble(
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
input_all_discrimination <- bind_rows(all_discrimination_not_black, all_discrimination_black)

# View the result
glimpse(input_all_discrimination)

# ---- Dummy coding EDUC_bucket and HHINCOME_bucket ----
input_all_discrimination_binary <- input_all_discrimination |>
  mutate(
    hs = as.integer(EDUC_bucket == "hs"),
    some_college = as.integer(EDUC_bucket == "some_college"),
    college_4yr_plus = as.integer(EDUC_bucket == "college_4yr_plus"),
    from_10k_to_100k = as.integer(HHINCOME_bucket == "from_10k_to_100k"),
    greater_than_100k = as.integer(HHINCOME_bucket == "greater_than_100k")
  )

# ----- try oaxaca on it

results <- oaxaca(
  formula = NUMPREC ~ hs + some_college + college_4yr_plus + from_10k_to_100k + greater_than_100k |
    is_black |
    hs + some_college + college_4yr_plus,
  data = input_all_discrimination_binary
)

results$y
results$threefold$overall


# In this pre-programmed case of total discrimination, you can see that over many
# iterations, the actual difference of 0.94 is almost identica to the difference in 
# coefficients, which is also 0.94, and the other two coefficients are statistically 
# indistinguishable from 0 at a p = 0.05 level (I'm approximating by multiplying the
# SE by 1.96 and adding / subtracting from the result.)


# ----- "no_discrimination" data frame ----- #

