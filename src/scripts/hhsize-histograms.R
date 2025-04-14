# Generate facet plot histograms of household size by year, age, race/eth, etc

library(dplyr)
library(patchwork)

source("src/utils/graphing-tools.R") # source the hist function
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed") # Connect to the database


age_groups <- c("0-4", "20-24", "40-44", "60-64", "80-84")

plots <- purrr::imap(age_groups, ~ {
  hist(
    data = ipums_db |>
      filter(YEAR == 2019, GQ %in% c(0, 1, 2), AGE_bucket == .x),
    title = paste("Age", .x),
    xtitle = "",
    ytitle = if (.y == 1) "Proportion" else "",
    xmax = 8,
    ymax = 0.6
  )
})


# Combine into one horizontal layout with 5 columns
final_plot <- wrap_plots(plots, nrow = 1) +
  plot_annotation(title = "Household Size Distribution by Age Group (2019)")
print(final_plot)

