library(shiny)
library(DT)  # For creating the interactive data table
library(dplyr)

# Load the data (change the path if the RDS file is located elsewhere)
crosstab_2000_2020 <- readRDS("data/crosstab_2000_2020.rds") |>
  mutate(
    weighted_mean_2000 = round(weighted_mean_2000, 3),
    weighted_mean_2020 = round(weighted_mean_2020, 3),
    diff = round(diff, 3)
    # Keep pval as numeric for sorting, we'll format it in the datatable
  )

# Define the UI
ui <- fluidPage(
  titlePanel("Household Size Summary (2000 vs. 2020)"),
  
  # Create a table with sorting, filtering, and scrolling options
  DTOutput("data_table")
)

# Define the server logic
server <- function(input, output) {
  output$data_table <- renderDT({
    datatable(
      crosstab_2000_2020,
      options = list(
        pageLength = -1,  # Show all rows by default
        scrollX = TRUE,   # Enable horizontal scrolling
        searching = TRUE, # Enable column filtering
        orderClasses = TRUE,  # Enable column sorting
        autoWidth = TRUE
      ),
      rownames = FALSE,
      colnames = c(
        "Age Group" = "AGE_bucket",
        "Race/Ethnicity" = "RACE_ETH_bucket",
        "Weighted Count 2000" = "weighted_count_2000",
        "Count 2000" = "count_2000",
        "Weighted Mean 2000" = "weighted_mean_2000",
        "Weighted Count 2020" = "weighted_count_2020",
        "Count 2020" = "count_2020",
        "Weighted Mean 2020" = "weighted_mean_2020",
        "Difference (2020 - 2000)" = "diff",
        "P-value" = "pval",
        "Significant (p ≤ 0.05)" = "sig",
        "Bonferroni Sig. (p ≤ 0.05/n)" = "sig_bonferroni"
      )
    ) %>%
      formatSignif("P-value", digits = 3)  # Format pval in scientific notation with 3 significant digits
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
