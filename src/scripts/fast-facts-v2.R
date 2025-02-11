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

# ----- Step 3: Define functions for tabulating summaries ----- #
# Function to compute weighted household size (or bedroom size) by designated group_by category
# in designated year
tabulate_summary <- function(
  data, 
  year = 2000,
  value = "NUMPREC", # could also be `persons_per_bedroom`
  group_by = NULL, # For now, only NULL or one string (e.g. "SEX") are supported. No multi-string vectors
  group_encoding = NULL # Optional encoding of factor labels for group_by variable. E.g. if
  # group_by = "SEX", you may input 1 = "Male", 2 = "Female"
  ){
  
  data_filtered <- data |> filter(YEAR == year) |> filter(GQ %in% c(0,1,2))

  result <- crosstab_mean(
    data = data_filtered,
    value = value,
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

# Wrapper function to tabulate summary of 2 years
tabulate_summary_2year <- function(
    data, 
    years = c(2000, 2019),
    value = "NUMPREC", # could also be `persons_per_bedroom`
    group_by = NULL, # For now, only NULL or one string (e.g. "SEX") are supported. No multi-string vectors
    group_encoding = NULL # Optional encoding of factor labels for group_by variable. E.g. if
    # group_by = "SEX", you may input 1 = "Male", 2 = "Female"
) {
  # Extract years dynamically
  year1 <- years[1]
  year2 <- years[2]
  
  # Compute summaries for both years
  year1_table <- tabulate_summary(data, year1, value, group_by, group_encoding)
  year2_table <- tabulate_summary(data, year2, value, group_by, group_encoding)
  
  # Merge results using dynamically constructed suffixes
  combined_table <- merge(year1_table, year2_table, by = "subgroup", suffixes = paste0("_", years))
  
  # Find all columns ending in _year1 and _year2 dynamically
  year1_cols <- grep(paste0("_", year1, "$"), names(combined_table), value = TRUE)
  year2_cols <- grep(paste0("_", year2, "$"), names(combined_table), value = TRUE)
  
  # Ensure matched pairs
  common_vars <- intersect(gsub(paste0("_", year1), "", year1_cols), gsub(paste0("_", year2), "", year2_cols))
  
  # Compute percent changes dynamically
  for (var in common_vars) {
    col1 <- paste0(var, "_", year1)
    col2 <- paste0(var, "_", year2)
    pct_col <- paste0(var, "_pctchg_", year1, "_", year2)
    
    result <- combined_table |> mutate(!!pct_col := (!!sym(col2) - !!sym(col1)) / !!sym(col1) * 100)
  }
  
  return(result)
}


# Example usage of tabulate_summary()
tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket")
tabulate_summary(data = ipums_db, year = 2000, group_by = "SEX", group_encoding = c("1" = "Male", "2" = "Female"))
tabulate_summary(data = ipums_db, year = 2000, group_by = c())
#tabulate_summary(data = ipums_db, year = 2000, value = "persons_per_bedroom", group_by = c())

# Example usage of tabulate_summary_2year()
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "RACE_ETH_bucket")
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "SEX", group_encoding = c("1" = "Male", "2" = "Female"))
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = c())


# ----- Step 4: Fast facts! ----- #
# Household size in 2000 and 2019
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = c())
   
   
   