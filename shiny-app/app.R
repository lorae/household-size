library(shiny)
library(DT)       # For creating the interactive data table
library(dplyr)
library(ggplot2)  # For plotting
library(tidyr)    # For data manipulation

# Load the data (adjust the path if necessary)
crosstab_2000_2020 <- readRDS("data/crosstab_2000_2020.rds") %>%
  mutate(
    weighted_mean_2000 = round(weighted_mean_2000, 3),
    weighted_mean_2020 = round(weighted_mean_2020, 3),
    diff = round(diff, 3)
  )

# Define the UI
ui <- fluidPage(
  # Add custom CSS for margins around the entire content
  tags$head(
    tags$style(HTML(".container-fluid { max-width: 90%; margin-left: auto; margin-right: auto; padding: 20px; }
                    .datatable-container { padding: 0; }
                    table.dataTable { width: 100% !important; }"))
  ),
  
  titlePanel("Household Size Summary (2000 vs. 2020)"),
  
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
             choices = unique(crosstab_2000_2020$RACE_ETH_bucket),
             selected = unique(crosstab_2000_2020$RACE_ETH_bucket)[1]
           ),
           checkboxInput(
             inputId = "show_error_bars",
             label = "Show 95% Confidence Intervals",
             value = FALSE
           )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Render the data table
  output$data_table <- renderDT({
    datatable(
      crosstab_2000_2020,
      options = list(
        pageLength = -1,  # Show all rows by default
        scrollX = TRUE,   # Enable horizontal scrolling
        searching = TRUE, # Enable column filtering
        orderClasses = TRUE,  # Enable column sorting
        fixedHeader = TRUE  # Keep headers fixed while scrolling
      ),
      rownames = FALSE,
      colnames = c(
        "Race/Ethnicity" = "RACE_ETH_bucket",
        "Age Group" = "AGE_bucket",
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
  
  # Prepare data for plotting based on user input
  plot_data <- reactive({
    # Filter data for the selected RACE_ETH_bucket
    crosstab_2000_2020 %>%
      filter(RACE_ETH_bucket == input$race_eth_bucket) %>%
      # Convert AGE_bucket to a factor with ordered levels
      mutate(AGE_bucket = factor(AGE_bucket, levels = unique(AGE_bucket))) %>%
      # Gather the data for plotting
      select(AGE_bucket, weighted_mean_2000, weighted_mean_2020) %>%
      tidyr::pivot_longer(
        cols = c("weighted_mean_2000", "weighted_mean_2020"),
        names_to = "Year",
        values_to = "Weighted_Mean"
      ) %>%
      mutate(
        Year = ifelse(Year == "weighted_mean_2000", "2000", "2020"),
        Year = factor(Year, levels = c("2000", "2020"))
      )
  })
  
  # Compute confidence intervals if needed
  plot_data_with_ci <- reactive({
    data <- plot_data()
    
    if (input$show_error_bars) {
      # Assuming you have standard errors or need to compute them
      # For demonstration, we'll compute dummy confidence intervals
      # Replace this with your actual computation
      data <- data %>%
        mutate(
          # Example: Assuming a standard error of 0.1 (replace with actual SE)
          SE = 0.1,
          CI_Lower = Weighted_Mean - 1.96 * SE,
          CI_Upper = Weighted_Mean + 1.96 * SE
        )
    } else {
      data <- data %>%
        mutate(
          CI_Lower = NA,
          CI_Upper = NA
        )
    }
    return(data)
  })
  
  # Render the plot
  output$household_size_plot <- renderPlot({
    data <- plot_data_with_ci()
    
    ggplot(data, aes(x = AGE_bucket, y = Weighted_Mean, color = Year, group = Year)) +
      geom_point(alpha = 0.8, size = 3) +
      geom_line(alpha = 0.8) +
      # Add error bars if showing confidence intervals
      { if (input$show_error_bars) geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) } +
      scale_color_manual(values = c("2000" = "#577590", "2020" = "#F94144")) +
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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


