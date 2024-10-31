library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the data
load("data/all_tables.rda")

# Compute the overall average household size for 2005 and 2022
avg_household_size_2005 <- sum(contributions$cont_2005, na.rm = TRUE)
avg_household_size_2022 <- sum(contributions$cont_2022, na.rm = TRUE)

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      ".container-fluid { max-width: 90%; margin-left: auto; margin-right: auto; padding: 20px; }
       .datatable-container { padding: 0; }
       table.dataTable { width: 100% !important; }"
    ))
  ),
  
  titlePanel("Household Size Summary (2005 vs. 2022)"),
  
  fluidRow(
    column(
      width = 12,
      p("This table provides a summary of household sizes for different age and race/ethnicity groups in the years 2005 and 2022."),
      DTOutput("data_table")
    )
  ),
  
  # New waterfall chart summarizing contributions by RACE_ETH_bucket
  fluidRow(
    column(
      width = 12,
      p("This waterfall chart shows the summed contributions by race/ethnicity groups to the change in average household size."),
      plotOutput("waterfall_chart_summary")
    )
  ),
  
  fluidRow(
    column(
      width = 8,
      p("The graph below shows the trends in average household size across different age groups and years. Use the options to customize the view."),
      plotOutput("household_size_plot")
    ),
    column(
      width = 4,
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
        choices = c(
          "Plot Household Size" = "household_size",
          "Plot Difference" = "difference"
        ),
        selected = "household_size"
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      p(
        "This graph calculates the total contribution of various race/ethnicity ",
        "and age groups to the change in overall household size since 2005. ",
        "First, we calculate what the average household size would be in 2022 ",
        "if group-level household sizes stayed constant since 2005, ",
        "but population composition were at 2022 levels. Then, we perform the same ",
        "calculation using only 2022 values."
      )
    )
  ),
  
  # Interactive waterfall chart and group selection
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = "waterfall_group",
        label = "Select Group of Interest for Waterfall Chart:",
        choices = unique(crosstab_2005_2022$RACE_ETH_bucket),
        selected = "White"
      )
    ),
    column(
      width = 8,
      plotOutput("waterfall_chart")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Render the data table
  output$data_table <- renderDT({
    datatable(
      data_for_table,
      options = list(
        pageLength = 18,
        scrollX = TRUE,
        searching = TRUE,
        orderClasses = TRUE,
        fixedHeader = TRUE
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
    ) %>%
      formatSignif("P-value", digits = 3)
  })
  
  # Render the summary waterfall chart
  output$waterfall_chart_summary <- renderPlot({
    ggplot(cont_summary, aes(x = RACE_ETH_bucket, fill = cont_diff_sum > 0)) +
      geom_rect(
        aes(
          xmin = as.numeric(factor(RACE_ETH_bucket)) - 0.4,
          xmax = as.numeric(factor(RACE_ETH_bucket)) + 0.4,
          ymin = start_bar,
          ymax = end_bar
        )
      ) +
      scale_fill_manual(values = c("red", "green")) +
      labs(
        x = "Race/Ethnicity Group",
        y = "Summed Contribution Difference",
        title = "Waterfall Chart Summing Contributions by Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Reactive expression for filtering data based on user input
  filtered_data <- reactive({
    data_long %>%
      filter(RACE_ETH_bucket == input$race_eth_bucket)
  })
  
  # Render the household size plot
  output$household_size_plot <- renderPlot({
    data <- filtered_data()
    
    if (input$plot_type == "household_size") {
      p <- ggplot(data, aes(x = AGE_bucket, y = weighted_mean, color = year, group = year)) +
        geom_linerange(
          aes(
            x = AGE_bucket,
            ymin = weighted_mean - 0.05,
            ymax = weighted_mean + 0.05
          ),
          size = 2,
          alpha = 0.8
        ) +
        scale_color_manual(values = c("2005" = "#577590", "2022" = "#F94144")) +
        ylim(1.5, 5.5) +
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
      
      if (input$show_error_bars) {
        p <- p +
          geom_errorbar(
            aes(
              ymin = weighted_mean - mean_standard_error * 1.96,
              ymax = weighted_mean + mean_standard_error * 1.96
            ),
            width = 0.2
          )
      }
      
      p
    } else {
      diff_data <- crosstab_2005_2022 %>%
        filter(RACE_ETH_bucket == input$race_eth_bucket)
      
      ggplot(diff_data, aes(x = AGE_bucket, y = diff)) +
        geom_bar(stat = "identity", fill = "#F94144", alpha = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        ylim(-0.7, 0.7) +
        labs(
          title = paste("Difference in Household Size (2022 - 2005) by Age for", input$race_eth_bucket),
          x = "Age Group",
          y = "Difference in Average Household Size"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
    }
  })
  
  # Render the interactive waterfall chart
  output$waterfall_chart <- renderPlot({
    cont_tmp <- contributions %>%
      filter(RACE_ETH_bucket == input$waterfall_group) %>%
      select(RACE_ETH_bucket, AGE_bucket, cont_diff) %>%
      arrange(AGE_bucket) %>%
      mutate(
        end_bar = cumsum(cont_diff),
        start_bar = lag(end_bar, default = 0)
      )
    
    ggplot(cont_tmp, aes(x = AGE_bucket, fill = cont_diff > 0)) +
      geom_rect(
        aes(
          xmin = as.numeric(factor(AGE_bucket)) - 0.4,
          xmax = as.numeric(factor(AGE_bucket)) + 0.4,
          ymin = start_bar,
          ymax = end_bar
        )
      ) +
      scale_fill_manual(values = c("red", "green")) +
      labs(
        x = "Age Group",
        y = "Contribution Difference",
        title = paste("Waterfall Chart of Contribution Differences for", input$waterfall_group)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



