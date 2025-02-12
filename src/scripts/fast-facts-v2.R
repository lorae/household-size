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
# TODO: Unit test!!!
# Function to compute weighted household size (or bedroom size) by designated group_by category
# in designated year
tabulate_summary <- function(
    data, 
    year = 2000,
    value = "NUMPREC", # Could also be `persons_per_bedroom`
    group_by = NULL,  # Supports only NULL or a single string (e.g., "SEX")
    group_encoding = NULL # Optional: a named vector for mapping values (e.g., c("1" = "Male", "2" = "Female"))
) {
  # Filter data for the specified year and not living in group quarters
  data_filtered <- data |> filter(YEAR == year, GQ %in% c(0,1,2))
  
  # Compute weighted mean household size by the specified group
  result <- crosstab_mean(
    data = data_filtered,
    value = value,
    wt_col = "PERWT",
    group_by = group_by,
    every_combo = FALSE
  ) 
  
  # Add the "subgroup" column
  if (is.null(group_by)) { 
    # If no group_by string is given, add a column called "subgroup" and title the one
    # row entry "overall"
    result <- result |> mutate(subgroup = "overall")
  } else {
    # If group_by string is given, rename the output column with a more generic name of
    # "subgroup"
    result <- result |> rename(subgroup = all_of(group_by))
    
    # Apply factor encoding if provided
    if (!is.null(group_encoding)) {
      result <- result |> 
        mutate(subgroup = recode(subgroup, !!!group_encoding)) |>  # Rename values
        mutate(subgroup = factor(subgroup, levels = group_encoding)) |> # Ensure correct order
        arrange(subgroup)
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
  result <- result |> select(any_of(c("subgroup", "hhsize", "ppbedroom")))
  
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
  
  # Compute percent changes dynamically
  for (var in intersect(names(year1_table), names(year2_table))) {
    if (var != "subgroup") { # Ensure we don't try to mutate 'subgroup'
      col1 <- paste0(var, "_", year1)
      col2 <- paste0(var, "_", year2)
      pct_col <- paste0(var, "_pctchg_", year1, "_", year2)
      
      combined_table <- combined_table |> mutate(!!pct_col := (!!sym(col2) - !!sym(col1)) / !!sym(col1) * 100)
    }
  }
  
  return(combined_table)
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


# ----- Step 4a: RESULTS - Household size in 2000 and 2019 ----- #
# TODO: export as table to Shiny app
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = c())

# ----- Step 4b: RESULTS - Household size in 2000 and 2019 by race/ethnicity ----- #
# TODO: export as table to Shiny app
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "RACE_ETH_bucket")

### # FIGURE 1: Create the bar plot with side-by-side bars for 2000 and 2019
# Create data for bar plot
race_summary_2000 <- tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket") |> mutate(year = 2000)
race_summary_2019 <- tabulate_summary(data = ipums_db, year = 2019, group_by = "RACE_ETH_bucket") |> mutate(year = 2019)
race_summary <- union_all(race_summary_2000, race_summary_2019) # Row bind the tables

# Define a single main color for all bars
main_color <- "steelblue"

# Generate the plot
fig01 <- ggplot(race_summary, aes(x = subgroup, y = hhsize, fill = factor(year))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.8, color = "black") +  # Bar border
  geom_text(aes(label = round(hhsize, 2), group = year), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +  
  scale_fill_manual(
    values = c("2000" = alpha(main_color, 0.4), "2019" = alpha(main_color, 0.8)), 
    name = "") +  # Ensures the legend colors match bar colors
  labs(y = "Average Household Size") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
  )

# Save the plot
ggsave("results/fig01.png", plot = fig01, width = 6.5, height = 3.5, dpi = 500)

# ----- Step 4c: RESULTS - Household size in 2000 and 2019 by age ----- #
# TODO: export as table to Shiny app
age_summary <- tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "AGE")

# Ages with the largest household sizes
# TODO: highlight these rows in the Shiny app table
age_summary |> slice_max(hhsize_2000, n = 1)
age_summary |> slice_max(hhsize_2019, n = 1)

# ----- Step 4d: RESULTS - Household size in 2000 and 2019 by age bucket ----- #
# Define ordered levels for AGE_bucket
age_bucket_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", 
                       "25-29", "30-34", "35-39", "40-44", "45-49", 
                       "50-54", "55-59", "60-64", "65-69", "70-74", 
                       "75-79", "80-84", "85plus")

# Create named vector for group_encoding
group_encoding_age_bucket <- setNames(age_bucket_levels, age_bucket_levels)

# Use `tabulate_summary_2year()` with group_encoding to enforce natural order
age_bucket_summary <- tabulate_summary_2year(
  data = ipums_db, 
  years = c(2000, 2019), 
  group_by = "AGE_bucket", 
  group_encoding = group_encoding_age_bucket
)

# Graph
# Filter data to only include ages up to 90
age_summary_filtered <- age_summary |>
  mutate(hhsize_diff_2019_2000 = hhsize_2019 - hhsize_2000) |>
  filter(subgroup <= 90)

# Create the bar plot
ggplot(age_summary_filtered, aes(x = factor(subgroup), y = hhsize_pctchg_2000_2019)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentage Change in Household Size (2000-2019) by Age (≤90)",
    x = "Age",
    y = "Percentage Change"
  ) +
  scale_x_discrete(breaks = seq(0, 90, by = 5)) +  # Labels only every 5th subgroup
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Create the bar plot
ggplot(age_summary_filtered, aes(x = factor(subgroup), y = hhsize_diff_2019_2000)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(
    title = "Absolute Change in Household Size (2000-2019) by Age (≤90)",
    x = "Age",
    y = "Household Size Change (Persons per Household)"
  ) +
  scale_x_discrete(breaks = seq(0, 90, by = 5)) +  # Labels only every 5th subgroup
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Household size in 2000 and 2019 by age bucket
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "AGE_bucket")
