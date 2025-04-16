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
source("src/utils/counterfactual-tools.R") # source the calculate_counterfactual function

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

nrow_pull <- 15000

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

mean_1 <- cf$summary$actual[1]
mean_2 <- weighted.mean(x = sample_2019_tb$NUMPREC, w = sample_2019_tb$PERWT)

success <- abs(mean_1 - mean_2) < 0.0001
if(success) {
  print("The test passed.")
} else {
  print(glue("The test failed. The mean according to the calculate_counterfactual function
  is {mean_1} but the mean according to the weighted.mean function is {mean_2}."))
}