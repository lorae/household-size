# Generate facet plot histograms of household size by year, age, race/eth, etc

library(dplyr)
library(patchwork)

source("src/utils/graphing-tools.R") # source the hist function
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed") # Connect to the database

# ----- Define a function which creates a 5-facet histogram plot ----- #

hist_ages <- function(
  data,
  title = "",
  params
) {
  # Map across params in parallel
  plots <- purrr::pmap(params, function(index, age, ytitle) {
    hist(
      data = ipums_db |>
        filter(YEAR == 2019, GQ %in% c(0, 1, 2), AGE_bucket == age),
      title = paste("Age", age),
      xtitle = "",
      ytitle = ytitle,
      xmax = 8,
      ymax = 0.6
    )
  })
  
  # Combine into one horizontal layout with 5 columns
  final_plot <- wrap_plots(plots, nrow = 1) +
    plot_annotation(title = title)
  
  return(final_plot)
}

# ----- Make a plot of hhsize by age group in 2019 ----- #
# Inputs to hist_ages
params <- tibble(
  index = c(1,2,3,4,5), # indices for pmap to loop through
  age = c("0-4", "20-24", "40-44", "60-64", "80-84"),
  ytitle = c("Proportion", "", "", "", "")
)

hist_ages(
  data =  ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)),
  title = "Household Size Distribution by Age Group (2019)",
  params = params
)

