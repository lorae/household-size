library(shiny)
library(DT)       # For creating the interactive data table
library(dplyr)
library(ggplot2)  # For plotting
library(tidyr)    # For data manipulation

# Load the data (adjust the path if necessary)
crosstab_2005_2022 <- readRDS("data/crosstab_2005_2022.rds") |>
  mutate(
    weighted_mean_2005 = round(weighted_mean_2005, 3),
    weighted_mean_2022 = round(weighted_mean_2022, 3),
    diff = round(diff, 3)
  )

# Create long-formatted data for later graphing
data_long <- crosstab_2005_2022 |>
  select(RACE_ETH_bucket, AGE_bucket, weighted_mean_2005, weighted_mean_2022, mean_standard_error_2005, mean_standard_error_2022) |>
  tidyr::pivot_longer(
    cols = c("weighted_mean_2005", "weighted_mean_2022", "mean_standard_error_2005", "mean_standard_error_2022"),
    names_to = c(".value", "year"),
    names_pattern = "(.*)_(\\d+)"
  ) |>
  mutate(
    year = factor(year, levels = c("2005", "2022"))
  )

# Define the UI
ui <- fluidPage(
  # Add custom CSS for margins around the entire content
  tags$head(
    tags$style(HTML(".container-fluid { max-width: 90%; margin-left: auto; margin-right: auto; padding: 20px; }
                    .datatable-container { padding: 0; }
                    table.dataTable { width: 100% !important; }"))
  ),
  
  titlePanel("Household Size Summary (2005 vs. 2022)"),
  
  # Data table at the top
  fluidRow(
    column(width = 12,
           # Data table output
           DTOutput("data_table")
    )
  ),
  
  # Graph and controls side by side
  fluidRow(
    column(width = 8,
           # Plot output
           plotOutput("household_size_plot")
    ),
    column(width = 4,
           # Controls for the plot
           tags$h3("Options"),
           selectInput(
             inputId = "race_eth_bucket",
             label = "Select Race/Ethnicity Group:",
             choices = unique(crosstab_2005_2022$RACE_ETH_bucket),
             selected = unique(crosstab_2005_2022$RACE_ETH_bucket)[1]
           ),
           checkboxInput(
             inputId = "show_error_bars",
             label = "Show 95% Confidence Intervals",
             value = FALSE
           ),
           radioButtons(
             inputId = "plot_type",
             label = "Select Plot Type:",
             choices = c("Plot Household Size" = "household_size", "Plot Difference" = "difference"),
             selected = "household_size"
           )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Render the data table
  output$data_table <- renderDT({
    datatable(
      crosstab_2005_2022,
      options = list(
        pageLength = -1,  # Show all rows by default
        scrollX = TRUE,   # Enable horizontal scrolling
        searching = TRUE, # Enable column filtering
        orderClasses = TRUE,  # Enable column sorting
        fixedHeader = TRUE  # Keep headers fixed while scrolling
      ),
      rownames = FALSE,
      colnames = c(
        "Race / Ethnicity" = "RACE_ETH_bucket",
        "Age Group" = "AGE_bucket",
        "% of 2022 Pop." = "percent_2022",
        "Num. Surveyed 2005" = "count_2005",
        "Num. Surveyed 2022" = "count_2022",
        "Pop. 2005" = "weighted_count_2005",
        "Pop. 2022" = "weighted_count_2022",
        "HH Size 2005" = "weighted_mean_2005",
        "HH Size 2022" = "weighted_mean_2022",
        "HH Size Difference (2022 - 2005)" = "diff",
        "P-value" = "pval",
        "Significant (p ≤ 0.05)" = "sig",
        "Bonferroni Sig. (p ≤ 0.05/n)" = "sig_bonferroni"
      )
    ) |>
      formatSignif("P-value", digits = 3)  # Format pval in scientific notation with 3 significant digits
  })
  
  # Prepare filtered data for plotting based on user input
  plot_data <- reactive({
    # Filter data_long for the selected RACE_ETH_bucket
    data_long |>
      filter(RACE_ETH_bucket == input$race_eth_bucket) |>
      # Convert AGE_bucket to a factor with ordered levels for better plot rendering
      mutate(AGE_bucket = factor(AGE_bucket, levels = unique(AGE_bucket)))
  })
  
  # Render the plot based on plot type selection
  output$household_size_plot <- renderPlot({
    data <- plot_data()
    
    if (input$plot_type == "household_size") {
      # Plot household size by age bucket
      ggplot(data, aes(x = AGE_bucket, y = weighted_mean, color = year, group = year)) +
        geom_point(alpha = 0.8, size = 3) +
        geom_line(alpha = 0.8) +
        # Add error bars with series-specific standard error
        { if (input$show_error_bars) 
          geom_errorbar(aes(
            ymin = weighted_mean - mean_standard_error * 1.96, 
            ymax = weighted_mean + mean_standard_error * 1.96
          ), width = 0.2) 
        } +
        scale_color_manual(values = c("2005" = "#577590", "2022" = "#F94144")) +
        labs(
          title = paste("Average Household Size by Age for", input$race_eth_bucket),
          x = "Age Group",
          y = "Average Household Size",
          color = "Year"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
          legend.position = "bottom"
        )
      
    } else if (input$plot_type == "difference") {
      # Plot difference
      ggplot(crosstab_2005_2022 |> filter(RACE_ETH_bucket == input$race_eth_bucket), aes(x = AGE_bucket, y = diff)) +
        geom_bar(stat = "identity", fill = "#F94144", alpha = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        labs(
          title = paste("Difference in Household Size (2022 - 2005) by Age for", input$race_eth_bucket),
          x = "Age Group",
          y = "Difference in Average Household Size"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1, size = 8)
        )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# # HHsize in 2022
# (crosstab_2005_2022$percent_2022/100 * crosstab_2005_2022$weighted_mean_2022) |> sum()
# # counterfactual 2022 HH size if means were at 2005 levels
# (crosstab_2005_2022$percent_2022/100 * crosstab_2005_2022$weighted_mean_2005) |> sum()
