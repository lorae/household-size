# The purpose of this script is to produce fast facts that are used in the draft 
# version of this paper. 
# Last modified mid-February 2025.

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")
library(base64enc)

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 3a: Fast facts! ----- #
# Overall household size, group quarters excluded, in 2000 and 2019.

# Function to compute weighted household size (or bedroom size) by designated group_by category
# in designated year
tabulate_summary <- function(
  data, 
  year = 2000,
  value = "NUMPREC", # could also be `persons_per_bedroom`
  group_by = "SEX", # For now, only c() or one string (e.g. "SEX") are supported. No multi-string vectors
  group_encoding = NULL # Optional encoding of factor labels for group_by variable. E.g. if
  # group_by = "SEX", you may input 1 = "Male", 2 = "Female"
  ){
  
  data_filtered <- data |> filter(YEAR == year) |> filter(GQ %in% c(0,1,2))

  result <- crosstab_mean(
    data = data_filtered,
    value = "NUMPREC",
    wt_col = "PERWT",
    group_by = group_by,
    every_combo = FALSE
  ) 
  
  # Add the "subgroup" column.
  if (length(group_by) == 0) { 
    # only one entry, "overall", if no group_by string is given
    result <- result |> mutate(subgroup = "overall")
  } else {
    # rename the output column called `group_by` with the more generic "subgroup" name
    result <- result |> rename(subgroup= all_of(group_by))
    
    # Apply factor encoding if provided
    if (!is.null(group_encoding)) {
      # Ensure that group_encoding is a named vector and map values
      if (is.factor(result$subgroup)) {
        result <- result |> mutate(subgroup = fct_recode(subgroup, !!!group_encoding))
      } else {
        result <- result |> mutate(subgroup = recode(subgroup, !!!group_encoding))
      }
    }
  }
  
  # Rename the weighted_mean column
  if (value == "NUMPREC") {
    result <- result |> rename(hhsize = weighted_mean)
  } else if (value == "persons_per_bedroom") {
    result <- result |> rename(ppbedroom = weighted_mean)
  } else {
    stop("`value` argument must either be \"NUMPREC\" or \"persons_per_bedroom\"")
  }
  
  # Keep only needed columns, drop the rest
  result <- result |>
    select(any_of(c("subgroup", "hhsize", "ppbedroom")))
  
  return(result)
}

# Example usage
tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket", group_encoding = NULL)
tabulate_summary(data = ipums_db, year = 2000, group_by = "SEX", group_encoding = c("1" = "Male", "2" = "Female"))
tabulate_summary(data = ipums_db, year = 2000, group_by = c())

# Compute household sizes for multiple years
years <- c(2000, 2019)
overall_hhsize_results <- tibble(
  year = years,
  hhsize = map_dbl(year, ~ get_overall_hhsize(ipums_db, .x))
)

hhsize_2000 <- overall_hhsize_results |> filter(year == 2000) |> pull(hhsize)
hhsize_2019 <- overall_hhsize_results |> filter(year == 2019) |> pull(hhsize)

hhsize_pctchg_2000_2019 <- (hhsize_2019 - hhsize_2000) / hhsize_2000 * 100 # pct change in hhsize from 2000 to 2019
   
# ----- Step 3b: Fast facts! ----- #
# Household size by race/ethnicity, group quarters excluded, in 2000 and 2019.  
   
   
   