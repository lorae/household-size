# ppl_per_hh.R
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

# ----- Step 2a: Import ipums_db data ----- #

ipums_db <- tbl(con, "ipums_processed")

# ----- Step 3: 2000 summary stats ----- #

ipums_2000 <- ipums_db |>
  filter(YEAR == 2000) |>
  mutate(hh_id = paste0(SAMPLE, "_", SERIAL)) |>  # Create household identifier
  group_by(hh_id) |>
  mutate(is_child = any(AGE <= 18, na.rm = TRUE)) |>
  ungroup() |>
  filter(PERNUM == 1) |>
  collect()

ipums_2023 <- ipums_db |>
  filter(YEAR == 2023) |>
  mutate(hh_id = paste0(SAMPLE, "_", SERIAL)) |>  # Create household identifier
  group_by(hh_id) |>
  mutate(is_child = any(AGE <= 18, na.rm = TRUE)) |>
  ungroup() |>
  filter(PERNUM == 1) |>
  collect()

weighted.mean(
  ipums_2000 |> filter(is_child == TRUE) |> pull(NUMPREC), 
  ipums_2000 |> filter(is_child == TRUE) |> pull(HHWT),
  na.rm = TRUE
)

