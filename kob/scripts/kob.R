# ----- STEP 0: Configuration ----- #

library(tibble)
library(dplyr)
library(oaxaca)

# Load synthetic data
load_path <- "kob/synthetic-data"
c_only <- readRDS(paste0(load_path, "/c-only.rds"))
e_only <- readRDS(paste0(load_path, "/e-only.rds"))
u_only <- readRDS(paste0(load_path, "/u-only.rds"))

# Function for one hot encoding `varlist`categorical variables in `data`
one_hot_encode <- function(data, varlist, prefixes) {
  stopifnot(length(varlist) == length(prefixes))  # sanity check
  
  data <- data |> mutate(row_id = row_number())
  
  encoded_list <- map2(varlist, prefixes, function(var, prefix) {
    data |>
      mutate({{ var }} := as.factor(.data[[var]])) |>
      mutate(dummy = 1L) |>
      pivot_wider(
        id_cols = row_id,
        names_from = all_of(var),
        values_from = dummy,
        values_fill = list(dummy = 0L),
        names_prefix = paste0(prefix, "_")
      )
  })
  
  encoded_data <- reduce(encoded_list, full_join, by = "row_id")
  full_data <- left_join(data, encoded_data, by = "row_id")
  
  return(full_data)
}
  
  # Combine all encoded blocks by row_id
  encoded_data <- reduce(encoded_list, full_join, by = "row_id")
  
  # Add back the original data
  full_data <- left_join(data, encoded_data, by = "row_id")
  
  return(full_data)
}


# ----- STEP 1: Oaxaca the u_only data ----- #

c_only_binary <- 
  one_hot_encode(c_only, 
                 varlist = c("year", "HHINCOME_bucket", "EDUC_bucket"),
                 prefixes = c("year", "HHINCOME", "EDUC"))

c_only_oaxaca <- oaxaca(
  formula = NUMPREC ~ hs + some_college + college_4yr_plus + from_10k_to_100k + greater_than_100k |
    `2000` |
    from_10k_to_100k + greater_than_100k,
  # hs + some_college + college_4yr_plus,
  data = c_only_binary,
  R = NULL # no bootstrapped SEs
)