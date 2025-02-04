# counterfactual-density-multiscenario.R
#
# The purpose of this script is to calculate what - after controlling for demographic 
# factors - average person-level household size would be in 2019 compared to 2000 
# values. This is the newer, updated version of src/counterfactual-density.R. 
#
# Rather than using all the demographic features at once, it layers them on one at
# a time. This allows us to conclude what happens with the introduction of each 
# individual layer and how robust the results are to various controls.
#
# These layered results are saved in .csv files that are placed in the shiny-app/data
# directory.
#
# Inputs:
#   - data/db/ipums.duckdb
# Outputs:
#   - shiny-app/data/something.csv
#   - shiny-app/data/something-else.csv
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

# ----- Step 2a: Import CPI-U data ----- #
# TODO: Transfer this code to process-ipums.R: import CPI-U series (and bucketing,
# too - see below)
cpiu <- read_excel(
  path = "data/helpers/CPI-U.xlsx",
  sheet = "BLS Data Series",
  range = "A12:N36",
  col_names = TRUE
  ) |>
  select(Year, Annual) |>
  rename(
    YEAR = Year,
    cpiu = Annual
  )

# Get the 2010 value of cpi_u
cpiu_2010_value <- cpiu |>
  filter(YEAR == 2010) |>
  pull(cpiu)

# Add a new column cpi_u_2010
cpiu <- cpiu |>
  mutate(cpiu_2010_deflator = cpiu / cpiu_2010_value)

# ----- Step 2b: Add columns to ipums_db data ----- #

ipums_db <- tbl(con, "ipums_processed")

# TODO: introduce this process into process-ipums.R rather than being here
# Adjust incomes for CPI-U
ipums_db <- ipums_db |>
  left_join(cpiu, by = "YEAR", copy = TRUE) |>
  mutate(
    INCTOT_cpiu_2010 = if_else(
      INCTOT %in% c(9999999, 9999998), 
      NA_real_, 
      INCTOT / cpiu_2010_deflator
    )
  ) |>
  mutate(
    INCTOT_cpiu_2010 = if_else(
      AGE < 15,
      0,
      INCTOT_cpiu_2010 # Keep the existing value if AGE >= 15
    )
  ) |>
  mutate(
    INCTOT_cpiu_2010_bucket = case_when(
      INCTOT_cpiu_2010 < 0 ~ "neg",
      INCTOT_cpiu_2010 == 0 ~ "0",
      INCTOT_cpiu_2010 < 10000 ~ "under 10k",
      INCTOT_cpiu_2010 >= 10000 & INCTOT_cpiu_2010 < 30000 ~ "10 to 30k",
      INCTOT_cpiu_2010 >= 30000 & INCTOT_cpiu_2010 < 100000 ~ "30k to 100k",
      INCTOT_cpiu_2010 >= 100000 ~ "over 100k",
      TRUE ~ NA_character_ # Handles unexpected cases
    )
  )

  
# We're adding some simplified/binary variables for counterfactual calculations
 ipums_db <- ipums_db |>
  mutate(
    us_born = BPL <= 120 # TRUE if person born in US or US territories
  )

# TODO: eventually write a function in dataduck that when buckets are created,
# the code automatically writes a list of vectors containing factor
# labels. For now, I'm just generating factor labels directly from the lookup
# table here, but this code is more brittle since it relies on me remembering
# which lookup table I used.
age_factor_levels <- extract_factor_label(
  lookup_table = read.csv("lookup_tables/age/age_buckets01.csv"),
  colname = "bucket_name"
)

# Introduce a variable for number of people per bedroom
ipums_db <- ipums_db |>
  mutate(
    persons_per_bedroom = NUMPREC / BEDROOMS
  )

# ----- Step 3: Functionalize counterfactual calculation ----- #
# This function takes data and metadata about desired counterfactual simulations
# as an input. As an output, it produces a list with two elements. The first output
# is a tibble with a single row summarizing the variables controlled for, the 
# counterfactual output, and the actual output. The second output is a crosstab 
# table that shows the exact contribution from each component into the summary statistics.

# TODO: This function needs to be modularized and unit-tested.
calculate_counterfactual <- function(
  cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
  p0 = 2000, # An integer for the year of the first (base) period
  p1 = 2019, # An integer for the year of the second (recent) period
  p0_data, # Data for period 0
  p1_data, # Data for period 1
  outcome = "NUMPREC" # Name of the outcome variable.
  # TODO: add back standard errors later. Not needed for now.
  ) {

  # TODO: add a step that catches errors if the specified data set is empty.
  # Note that this should be done at this level, but I'm also surprised the crosstab_mean
  # and crosstab_percent functions aren't producing errors when I do this.
  
  # Calculate the mean outcome in periods 0 and 1 of the data
  print(glue("Calculating {p0} means..."))
  mean_p0 <- crosstab_mean(
    data = p0_data,
    value = outcome,
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p0} means done!"))

  print(glue("Calculating {p1} means..."))
  mean_p1 <- crosstab_mean(
    data = p1_data,
    value = outcome,
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p1} means done!"))

  # Calculate the population composition in period 1
  print(glue("Calculating {p1} percents..."))
  percent_p1 <- crosstab_percent(
    data = p1_data,
    wt_col = "PERWT",
    group_by = cf_categories,
    percent_group_by = c(),
    every_combo = TRUE
  ) |>
    select(-weighted_count, -count)
  print(glue("{p1} percents done!"))

  # Combine means from periods 0 and 1 with population composition from period 1
  crosstab_p0_p1 <- 
    full_join(
      # Join the p0 and p1 mean data frames, appending a _{year} suffix to columns
      mean_p0 |> rename_with(~paste0(., "_", p0), -all_of(cf_categories)),
      mean_p1 |> rename_with(~paste0(., "_", p1), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    full_join(
      # Join the p0 and p1 mean data frames with p1 percent data frame, appending _{year} suffixes
      percent_p1 |> rename_with(~paste0(., "_", p1), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    mutate(
      # Apply rules for handling NA values
      across(
        all_of(paste0("weighted_mean_", p1)), 
        ~replace_na(., 0),
        .names = "{.col}"
      ),
      across(
        all_of(paste0("weighted_mean_", p0)),
        ~ifelse(is.na(.), .data[[paste0("weighted_mean_", p1)]], .),
        .names = "{.col}"
      )
    ) |>
    arrange(across(all_of(cf_categories))) |>
    mutate(
      # Diff = period 1 household size minus period 0 household size
      diff = .data[[paste0("weighted_mean_", p1)]] - .data[[paste0("weighted_mean_", p0)]],
      # Calculate contributions (See Shiny app, main tab, for more explanation on contributions).
      cont_p1 = .data[[paste0("percent_", p1)]] * .data[[paste0("weighted_mean_", p1)]] / 100,
      cont_p1_cf = .data[[paste0("percent_", p1)]] * .data[[paste0("weighted_mean_", p0)]] / 100,
      contribution_diff = cont_p1 - cont_p1_cf,
    ) |>
    select(any_of(c(
      # Reorder columns more intuitively
      cf_categories, 
      paste0("count_", p0), paste0("count_", p1),
      paste0("weighted_count_", p0), paste0("weighted_count_", p1),
      paste0("percent_", p1), paste0("se_percent_", p1),
      paste0("weighted_mean_", p0), paste0("weighted_mean_", p1),
      "diff", 
      "contribution_diff"
    )))

  # Calculate overall values
  actual_outcome_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p1)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  cf_outcome_p1 <- crosstab_p0_p1 |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p0)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  return(list(
    summary = tibble(
      RACE_ETH_bucket = ("RACE_ETH_bucket" %in% cf_categories),
      AGE_bucket = ("AGE_bucket" %in% cf_categories),
      SEX = ("SEX" %in% cf_categories),
      us_born = ("us_born" %in% cf_categories),
      EDUC = ("EDUC" %in% cf_categories),
      INCTOT_cpiu_2010_bucket = ("INCTOT_cpiu_2010_bucket" %in% cf_categories),
      CPUMA0010 = ("CPUMA0010" %in% cf_categories),
      counterfactual = cf_outcome_p1,
      actual = actual_outcome_p1,
      diff = actual_outcome_p1 - cf_outcome_p1
    ),
    contributions = crosstab_p0_p1
    ))
}

# ----- Step 4: Run counterfactuals ----- #
# List of scenarios
scenarios <- list(
  c("AGE_bucket"),
  c("SEX"),
  c("us_born"),
  c("EDUC"),
  c("INCTOT_cpiu_2010_bucket"),
  c("CPUMA0010"),
  c("RACE_ETH_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010")
)

# Generate data for all scenarios
nrow_pull <- 10000000
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()

# Persons per bedroom
bedroom_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019,
    p0_data = p0_sample,
    p1_data = p1_sample,
    outcome = "persons_per_bedroom"
    )$summary # Extract only the summary tibble
  )
)

# TODO: add a check in the function calculate_counterfactual that tests whether
# all of the potential categories are populated by data?
# motivated by my observation that smaller samples (e.g. 10,000) don't capture all
# CPUMA0010s, resulting in NA estimates.
# Persons per household
hhsize_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019, 
    p0_data = p0_sample, 
    p1_data = p1_sample,
    outcome = "NUMPREC"
    )$summary # Extract only the summary tibble
  )
)

# ----- Step 5: Create mappable diff data by CPUMA0010 ----- #
# Calculate CPUMA-level fully-controlled diffs and contributions to put into a 
# box-and-whisker plot.
hhsize_contributions <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample, 
  p1_data = p1_sample,
  outcome = "NUMPREC"
)$contributions

bedroom_contributions <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC", "INCTOT_cpiu_2010_bucket", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample, 
  p1_data = p1_sample,
  outcome = "persons_per_bedroom"
)$contributions

hhsize_contributions_summary <- hhsize_contributions |>
  group_by(CPUMA0010) |>
  summarize(contribution_diff = sum(contribution_diff, na.rm = TRUE),
            prop_2019 = sum(percent_2019) / 100, .groups = "drop",
            pop_2019 = sum(weighted_count_2019)) |>
  mutate(diff = contribution_diff / prop_2019)

bedroom_contributions_summary <- bedroom_contributions |>
  group_by(CPUMA0010) |>
  summarize(contribution_diff = sum(contribution_diff, na.rm = TRUE),
            prop_2019 = sum(percent_2019) / 100, .groups = "drop",
            pop_2019 = sum(weighted_count_2019)) |>
  mutate(diff = contribution_diff / prop_2019)

# Merge with CPUMA to state matching
load("data/helpers/cpuma-state-cross.rda")
# Create a list of states to loop through later
list_of_states <- cpuma_state_cross |>
  select(State) |>
  unique()

hhsize_contributions_state <- merge(
  cpuma_state_cross,
  hhsize_contributions_summary,
  by = "CPUMA0010"
)
bedroom_contributions_state <- merge(
  cpuma_state_cross,
  bedroom_contributions_summary,
  by = "CPUMA0010"
)

# Data validity checks
is.na(hhsize_contributions_state$State) |> sum() # No NA values! Great!
is.na(bedroom_contributions_state$State) |> sum() # No NA values! Great!

#### Sample data viz
# Filter for New York (STATEFIP = 36)
state <- "New Jersey"
data = hhsize_contributions_state
#data = bedroom_contributions_state

state_summary <- hhsize_contributions_state |>
  group_by(State) |>
  summarize(
    median = median(diff, na.rm = TRUE),
    weighted_median = rep(diff, times = pop_2019) |> median(),
    weighted_mean = weighted.mean(diff, w = pop_2019, na.rm = TRUE),
    .groups = "drop"
  )

# A function that produces a dotplot by state
dotplot_by_state <- function(
  state = "New Jersey",
  data = hhsize_contributions_state
) {
  # Subset the data to just that state
  boxplot_data <- subset(data, State == state)
  
  # Calculate median, weighted median, and weighted mean
  median <- boxplot_data |>
    pull(diff) |> 
    median()
  weighted_median <- rep(boxplot_data$diff, times = boxplot_data$pop_2019) |>
    median()
  weighted_mean <- weighted.mean(boxplot_data$diff, w = boxplot_data$pop_2019)
  
  # Create the horizontal boxplot with overlaid points
  output_plot <- ggplot(boxplot_data, aes(x = diff, y = "")) +
    geom_dotplot(stackdir = "center", dotsize = 0.5, alpha = 0.6, binwidth = 0.05) +
    theme_minimal() +
    labs(title = glue("{state}"),
         x = "Difference (diff)",
         y = "") +
    theme(legend.position = "none") +
    annotate("segment", x = median, xend = median, y = 0.8, yend = 1.2, 
             linetype = "dotted", color = "black", size = 0.5) +
    annotate("segment", x = weighted_median, xend = weighted_median, y = 0.8, yend = 1.2, 
             linetype = "dotted", color = "blue", size = 0.5) +
    annotate("segment", x = weighted_mean, xend = weighted_mean, y = 0.8, yend = 1.2, 
             linetype = "dotted", color = "red", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
    xlim(-0.8, 0.6)
    
      
      return(output_plot)
}



#### Sample of unweighted vs weighted histogram
binwidth = 0.1 # thickness of bars

# unweighted histogram
ggplot(boxplot_data, aes(x = diff)) +
  geom_histogram(binwidth = binwidth, fill = "blue", color = "black") +
  labs(title = "UNweighted Histogram of diff",
       x = "diff",
       y = "Weighted Count") +
  theme_minimal()

# weighted histogram
ggplot(boxplot_data, aes(x = diff, weight = pop_2019)) +
  geom_histogram(binwidth = binwidth, fill = "blue", color = "black") +
  labs(title = "Weighted Histogram of diff",
       x = "diff",
       y = "Weighted Count") +
  theme_minimal()

# ----- Step 5: Save the results ----- #

# Counterfactuals
save(
  hhsize_cf, 
  bedroom_cf,
  file = "shiny-app/data/counterfactuals.rda"
)

# Diff data
save(
  hhsize_contributions_state,
  bedroom_contributions_state,
  state_summary,
  list_of_states,
  file = "shiny-app/data/diffs-by-geography.rda"
)
