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
library("base64enc")
library("patchwork")

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
    every_combo = TRUE # Necessary to ensure all tables with the same inputs produce the same rows
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
  
  # Merge results, add _year suffixes to analogous columns
  combined_table <- left_join(year1_table, year2_table, by = "subgroup", suffix = paste0("_", years))

  # Compute percent changes dynamically
  for (var in intersect(names(year1_table), names(year2_table))) {
    if (var != "subgroup") { # Ensure we don't try to mutate 'subgroup'
      col1 <- paste0(var, "_", year1)
      col2 <- paste0(var, "_", year2)
      pct_col <- paste0(var, "_pctchg_", year1, "_", year2)
      
      combined_table <- combined_table |> 
        mutate(!!pct_col := (!!sym(col2) - !!sym(col1)) / !!sym(col1) * 100)
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
# Note, this is brittle, since age buckets may change
# TODO: repo-wide factor dictionary, updated upon initial data ingestion
age_bucket_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", 
                       "25-29", "30-34", "35-39", "40-44", "45-49", 
                       "50-54", "55-59", "60-64", "65-69", "70-74", 
                       "75-79", "80-84", "85plus")

# Create named vector for group_encoding
group_encoding_age_bucket <- setNames(age_bucket_levels, age_bucket_levels)

# Use `tabulate_summary_2year()` with group_encoding to enforce natural order of age
# buckets and generate data for figure 2
# Define race/ethnicity categories, including "All"
race_ethnicities <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")

# Generate summary for all categories in a loop (with 5 cores, takes about 10 seconds)
age_bucket_summary <- map_dfr(race_ethnicities, function(race) {
  data_filtered <- if (race == "All") ipums_db else ipums_db |> filter(RACE_ETH_bucket == race)
  
  tabulate_summary_2year(
    data = data_filtered,
    years = c(2000, 2019),
    group_by = "AGE_bucket",
    group_encoding = group_encoding_age_bucket
  ) |> mutate(RACE_ETH_bucket = race) # Add race/ethnicity label
})

### # FIGURE 2: Bar plot with percentage differences between 2000 and 2019 by age;
# faceted by race
# Graph
ggplot(age_bucket_summary |> filter(RACE_ETH_bucket == "All"), 
       aes(x = factor(subgroup), y = hhsize_pctchg_2000_2019)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentage Change in Household Size (2000-2019) by Age",
    x = "Age",
    y = "Percentage Change"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Create the bar plot
# ggplot(age_summary_filtered, aes(x = factor(subgroup), y = hhsize_diff_2019_2000)) +
#   geom_bar(stat = "identity", fill = "darkred") +
#   labs(
#     title = "Absolute Change in Household Size (2000-2019) by Age (≤90)",
#     x = "Age",
#     y = "Household Size Change (Persons per Household)"
#   ) +
#   scale_x_discrete(breaks = seq(0, 90, by = 5)) +  # Labels only every 5th subgroup
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

##############################################
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv#
##############################################
# Define race order (ensuring correct factor levels for consistent ordering)
race_order <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")
age_bucket_summary <- age_bucket_summary %>%
  mutate(RACE_ETH_bucket = factor(RACE_ETH_bucket, levels = race_order))

# Split data into two halves
left_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[1:4])  # First 4 races
right_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[5:8])  # Last 4 races

# Left plot (Facet titles on the LEFT)
left_plot <- ggplot(left_data, aes(x = subgroup, y = hhsize_pctchg_2000_2019, 
                                   fill = RACE_ETH_bucket == "All")) +  # Conditional fill
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "steelblue"), guide = "none") +  # Define colors
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 10),  # Labels on the left
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 6),
    
    # Hide left-side axis labels
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    
    # Add subtle tick marks on the right side without grid lines
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),  # Right-align text
    axis.ticks.y.right = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),  # Tick marks every 5%
    limits = c(-15, 15),
    labels = function(x) paste0(x, "%"),  # Add percentage symbol
    sec.axis = dup_axis(name = NULL, labels = function(x) paste0(x, "%"))  # Adds tick marks with percentage on the right
  ) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

left_plot

# Right plot (Facet titles on the RIGHT)
right_plot <- ggplot(right_data, aes(x = subgroup, 
                                     y = pmin(hhsize_pctchg_2000_2019, 14))) +  # Cap at 14% (leave room for asterisk at 14.5%)
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  
  # Add an asterisk above the capped bar at a safe y-position (14.5 instead of 16)
  geom_text(data = right_data %>% filter(RACE_ETH_bucket == "Other", subgroup == "80-84"),
            aes(x = subgroup, y = 14.5, label = "*"),
            size = 5, color = "black") +
  
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, color = "black", size = 10),  # Labels on the right
    strip.placement = "outside",  # Fixing the warning by removing strip.position
    panel.spacing = unit(0.8, "lines"),  # Slightly reduce panel spacing
    panel.spacing.y = unit(0.1, "lines"),  # Reduce vertical spacing
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 6),
    
    # Add subtle tick marks on the LEFT side without labels
    axis.text.y.left = element_blank(),  # No numeric labels
    axis.ticks.y.left = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Hide right-side axis labels
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # Adjust plot margins to reduce left-side empty space
    plot.margin = margin(5, 5, 5, -10)  # Reduce left margin
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),  # Tick marks every 5%
    limits = c(-15, 15),
    sec.axis = dup_axis(name = NULL)  # Keep tick marks without labels
  ) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

right_plot

# Combine both plots into one, side-by-side
combined_plot <- left_plot + right_plot + 
  plot_layout(widths = c(1, 1)) + 
  plot_annotation(
    caption = "*The 80-84 age group's increase in the 'Other' category exceeds the y-axis limits of ±15%."
    )

# Save the combined plot
ggsave("results/fig02.png", plot = combined_plot, width = 6.5, height = 6.5, dpi = 500)

# Display the combined plot
print(combined_plot)
##############################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
##############################################


# Household size in 2000 and 2019 by age bucket
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "AGE_bucket")
