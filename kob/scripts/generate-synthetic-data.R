# ----- STEP 0: Configuration ----- #

set.seed(123)
n <- 5000

educ_levels <- c("less_than_hs", "hs", "some_college", "college_4yr_plus")
inc_levels <- c("less_than_10k", "from_10k_to_100k", "greater_than_100k")

# Explicitly calculate the maximum sum of the factor levels
length(educ_levels) + length(inc_levels)

# Function to draw positive integers from a normal distribution
draw_positive_ints <- function(n, mean, sd) {
  pmax(1, round(rnorm(n, mean, sd)))
}

# ----- STEP 1: Create u-component KOB decomp data frame ----- #
# Via this data generation process, 2000 an 2019 data differ in 
# their correlates (education, income) as well as in their average
# outcome NUMPREC. However, NUMPREC is generated independently and
# depends only on year: It is not generate based on education or
# income. 

# 2000 synthetic data: u (intercept) only
u_only_2000 <- tibble(
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
    EDUC_bucket = factor(EDUC_bucket, levels = educ_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = inc_levels, ordered = TRUE)
  )

# 2019 synthetic data: u (intercept) only
u_only_2019 <- tibble(
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
    EDUC_bucket = factor(EDUC_bucket, levels = educ_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = inc_levels, ordered = TRUE)
  )

# Combine into one data frame
u_only <- bind_rows(u_only_2000, u_only_2019)
save(u_only, file = "kob/synthetic-data/u-only.rds")

# ----- STEP 2: Create e-component KOB decomp data frame ----- #
# Via this data generation process, 2000 an 2019 data differ in 
# their correlates (education, income) as well as in their average
# outcome NUMPREC. NUMPREC is generated as a function of education
# and income.

# 2000 synthetic data: e (endowment) only
e_only_2000 <- tibble(
  year = 2000,
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
    EDUC_bucket = factor(EDUC_bucket, levels = educ_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = inc_levels, ordered = TRUE)
  ) |>
  mutate(
    expected_NUMPREC = 
      (length(educ_levels) - as.numeric(EDUC_bucket)) + 
      (length(inc_levels) - as.numeric(HHINCOME_bucket)) +
      1, # Highest income and education has NUMPREC 1, anything else is linearly increasing
    NUMPREC = draw_positive_ints(n, mean = expected_NUMPREC, sd = 1.5)
  )

# 2019 synthetic data: e (endowment) only
e_only_2019 <- tibble(
  year = 2019,
  EDUC_bucket = sample(
    c("less_than_hs", "hs", "some_college", "college_4yr_plus"),
    size = n,
    replace = TRUE,
    prob = c(0.05, 0.2, 0.4, 0.35)
  ),
  HHINCOME_bucket = sample(
    c("less_than_10k", "from_10k_to_100k", "greater_than_100k"),
    size = n,
    replace = TRUE,
    prob = c(0.1, 0.65, 0.25)
  ),
  AGE = sample(1:80, size = n, replace = TRUE),
  PERWT = 1
) |>
  mutate(
    EDUC_bucket = factor(EDUC_bucket, levels = educ_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = inc_levels, ordered = TRUE)
  ) |>
  mutate(
    expected_NUMPREC = 
      (length(educ_levels) - as.numeric(EDUC_bucket)) + 
      (length(inc_levels) - as.numeric(HHINCOME_bucket)) +
      1, # Highest income and education has NUMPREC 1, anything else is linearly increasing
    NUMPREC = draw_positive_ints(n, mean = expected_NUMPREC, sd = 1.5)
  )

# Combine into one data frame
e_only <- bind_rows(e_only_2000, e_only_2019)
save(e_only, file = "kob/synthetic-data/e-only.rds")

# ----- STEP 3: Create c-component KOB decomp data frame ----- #
