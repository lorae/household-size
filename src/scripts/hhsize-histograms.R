# Generate facet plot histograms of household size by year, age, race/eth, etc

library(dplyr)
library(patchwork)
library(DBI)
library(duckdb)

source("src/utils/graphing-tools.R") # source the make_histogram function
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed") # Connect to the database

# ----- Define a function which creates a 5-facet histogram plot ----- #

hist_ages <- function(
    data,
    title = "",
    bar_fill = list(color = "skyblue", alpha = 0.5),
    xmax = 6,
    ymax = 0.6,
    params
) {
  # Map across params in parallel
  plots <- purrr::pmap(params, function(index, age, ytitle) {
    make_histogram(
      data = data |> filter(AGE_bucket == age),
      title = paste("Age", age),
      xtitle = "",
      ytitle = ytitle,
      xmax = xmax,
      ymax = ymax,
      bar_fill = bar_fill
    )
  })
  
  # Combine into one horizontal layout with 5 columns
  final_plot <- wrap_plots(plots, nrow = 1) +
    plot_annotation(title = title)
  
  return(final_plot)
}

# ----- Make a plot of hhsize by age group in 2019 ----- #
params <- tibble(
  index = c(1,2,3,4,5), # indices for pmap to loop through
  age = c("0-4", "20-24", "40-44", "60-64", "80-84"),
  ytitle = c("Proportion", "", "", "", "")
)

hist_ages_2019 <- hist_ages(
  data = ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)),
  title = "Household Size Distribution by Age Group (2019)",
  params = params
)

hist_ages_2019
ggsave("results/histograms/2019-all-byage.png", hist_ages_2019, width = 10, height = 6, scale = 1)

# ----- Make a plot of hhsize by age group among Hispanics in 2000, 2019 ----- #
double_hist_ages <- function(
    data,
    title = "",
    xmax = 6,
    ymax = 0.6,
    per1 = default_per1,
    per2 = default_per2,
    bar_fills = list(
      per1 = list(color = "skyblue", alpha = 0.4, line_color = "skyblue", line_type = "dashed"),
      per2 = list(color = "forestgreen", alpha = 0.2, line_color = "forestgreen", line_type = "solid")
    ),
    params
) {
  plots <- purrr::pmap(params, function(index, age, ytitle) {
    make_double_histogram(
      data = data |> filter(AGE_bucket == age),
      title = paste("Age", age),
      xtitle = "",
      ytitle = ytitle,
      xmax = xmax,
      ymax = ymax,
      per1 = per1,
      per2 = per2,
      bar_fills = bar_fills
    )
  })
  
  wrap_plots(plots, nrow = 1) +
    plot_annotation(title = title)
}


hist_hispan_ages_comparison <- double_hist_ages(
  data = ipums_db |> filter(GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Hispanic"),
  title = "Hispanic Household Size by Age Group (2000 vs 2019)",
  xmax = 10,
  ymax = 0.4,
  per1 = 2000,
  per2 = 2019,
  params = params
)

hist_white_ages_comparison <- double_hist_ages(
  data = ipums_db |> filter(GQ %in% c(0, 1, 2), RACE_ETH_bucket == "White"),
  title = "White Household Size by Age Group (2000 vs 2019)",
  xmax = 6,
  ymax = 0.7,
  per1 = 2000,
  per2 = 2019,
  params = params
)

hist_black_ages_comparison <- double_hist_ages(
  data = ipums_db |> filter(GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Black"),
  title = "Black Household Size by Age Group (2000 vs 2019)",
  xmax = 8,
  ymax = 0.4,
  per1 = 2000,
  per2 = 2019,
  params = params
)

hist_hispan_ages_comparison
hist_white_ages_comparison
hist_black_ages_comparison
ggsave("results/histograms/hispanic-2000-2019-byage.png", hist_hispan_ages_comparison, width = 10, height = 6)
ggsave("results/histograms/white-2000-2019-byage.png", hist_white_ages_comparison, width = 10, height = 6)
ggsave("results/histograms/black-2000-2019-byage.png", hist_black_ages_comparison, width = 10, height = 6)


