# ----- STEP 0: Configuration ----- #

set.seed(123)
n <- 5000

education_levels <- c("less_than_hs", "hs", "some_college", "college_4yr_plus")
income_levels <- c("less_than_10k", "from_10k_to_100k", "greater_than_100k")

# Function to draw positive integers from a normal distribution
draw_positive_ints <- function(n, mean, sd) {
  pmax(1, round(rnorm(n, mean, sd)))
}

# ----- STEP 1: Create u-component KOB decomp data frame ----- #

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
    EDUC_bucket = factor(EDUC_bucket, levels = education_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = income_levels, ordered = TRUE)
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
    EDUC_bucket = factor(EDUC_bucket, levels = education_levels, ordered = TRUE),
    HHINCOME_bucket = factor(HHINCOME_bucket, levels = income_levels, ordered = TRUE)
  )

# Combine into one data frame
u_only <- bind_rows(u_only_2000, u_only_2019)
save(u_only, file = "kob/synthetic-data/u-only.rds")

# ----- STEP 2: Create e-component KOB decomp data frame ----- #
