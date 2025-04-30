# temporal-kob.R
# apply learnings from test-temporal-kob.r to produce the KOB decomposition

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

ipums_db <- ipums_db |>
  mutate(
    tenure = ifelse(OWNERSHP == 1, "homeowner", "renter"),
    sex = ifelse(SEX == 1, "male", "female")
  )

# ipums_db <- ipums_db |>
#   mutate(
#     tenure = ifelse(OWNERSHP == 1, as.character("homeowner"), as.character("renter")),
#     sex = ifelse(SEX == 1, as.character("male"), as.character("female"))
#   )
# ipums_db <- ipums_db |>
#   mutate(
#     tenure = sql("CASE WHEN OWNERSHP = 1 THEN 'homeowner' ELSE 'renter' END"),
#     sex = sql("CASE WHEN SEX = 1 THEN 'male' ELSE 'female' END")
#   )



# These two models take about a two minutes to compute. No CPUMAs since they are difficult
# to handle. 8 Gb, whoa!
model_2000 <- lm(data = ipums_db |> filter(YEAR == 2000 & GQ %in% c(0,1,2)),
                 weights = PERWT,
                 formula = NUMPREC ~ RACE_ETH_bucket + AGE_bucket + sex + us_born + 
                   EDUC_bucket + INCTOT_cpiu_2010_bucket + tenure
)

model_2019 <- lm(data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
                 weights = PERWT,
                 formula = NUMPREC ~ RACE_ETH_bucket + AGE_bucket + sex + us_born + 
                   EDUC_bucket + INCTOT_cpiu_2010_bucket + tenure
)

coef_df <- full_join(
  enframe(model_2000$coefficients, name = "name", value = "mean_2000"),
  enframe(model_2019$coefficients, name = "name", value = "mean_2019"),
  by = "name"
)

# Known varnames (exact strings)
known_varnames <- c(
  "RACE_ETH_bucket", "AGE_bucket", "sex", "us_born",
  "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "tenure"
)

intercept_row <- coef_df |>
  filter(name == "(Intercept)") |>
  mutate(varname = NA_character_, value = NA_character_)

non_intercepts <- coef_df |>
  filter(name != "(Intercept)") |>
  mutate(
    varname = map_chr(name, function(nm) {
      matched <- keep(known_varnames, function(vn) str_starts(nm, vn))
      if (length(matched) != 1) stop(paste("Could not uniquely match varname for:", nm))
      matched
    }),
    value = str_remove(name, varname)
  )

coef_df <- bind_rows(intercept_row, non_intercepts)



get_weighted_count <- function(varname, value, year) {
  # Return NA for intercept or missing input
  if (is.na(varname) || is.na(value)) {
    return(NA_real_)
  }
  
  # Coerce specific label strings to their underlying codes
  if (value == "male" && varname == "sex") {
    value <- 1
    varname <- "SEX"
  } else if (value == "homeowner" && varname == "tenure") {
    value <- 1
    varname <- "OWNERSHP"
  }
  
  ipums_db |>
    filter(YEAR == !!year, GQ %in% c(0, 1, 2)) |>
    filter(!!sym(varname) == !!value) |>
    summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
    collect() |>
    pull(weighted_count)
}


coef <- coef_df |>
  mutate(
    weighted_count_2000 = map2_dbl(varname, value, ~ get_weighted_count(.x, .y, 2000)),
    weighted_count_2019 = map2_dbl(varname, value, ~ get_weighted_count(.x, .y, 2019))
    )

pop_2000 <- ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |> 
  summarize(weighted_count = sum(PERWT), na.rm = TRUE) |>
  collect() |>
  pull(weighted_count)
  
pop_2019 <- ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |> 
  summarize(weighted_count = sum(PERWT), na.rm = TRUE) |>
  collect() |>
  pull(weighted_count)

coef <- coef |>
  mutate(
    prop_2000 = weighted_count_2000 / pop_2000,
    prop_2019 = weighted_count_2019 / pop_2019
  )