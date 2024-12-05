# app.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)

# Load necessary data
load("data/all_tables.rda")

# Define some constant variables used in the text description of the data
hhsize_2022 <- (contributions$percent_2022 * contributions$weighted_mean_2022/100) |> sum()
cf_hhsize_2022 <- (contributions$percent_2022 * contributions$weighted_mean_2005/100) |> sum()

# Define UI
ui <- fluidPage(
  
  # Sidebar for potential controls (add inputs here as needed)
  div(class = "scroll-container",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          p(a("Introduction", href = "#intro")),
          p(a("Table 1: Hypothetical Example", href = "#table1")),
          p(a("Table 2: 2005-2022 Changes by Race and Age", href = "#table2")),
          p(a("Table 3: Contributions", href = "#table3"))
        ),
        
        # Main panel for displaying content
        mainPanel(
          titlePanel("Changes in American Household Size: 2005 to 2022"),
          
          tags$h3("Introduction", id = "intro"),
          p("In 2005, the average American lived in a household of", 
            round(weighted.mean(crosstab_2005_2022$weighted_mean_2005, crosstab_2005_2022$weighted_count_2005), 3),
            "people. By 2022, average household size shrunk to",
            round(weighted.mean(crosstab_2005_2022$weighted_mean_2022, crosstab_2005_2022$weighted_count_2022), 3), "people."
          ),
          p("Some of this effect may be compositional. The demographic makeup of Americans has changed in the last 17 years. Older people, on average, tend to live in smaller households, and this shift toward an older America could explain the reduction in household size. At the same time, there are countervailing forces: the population of Hispanic and Latino Americans has grown, and those individuals tend to live in larger households than average."),
          p("We're interested in understanding how Americans' housing behaviors may or may not reflect a housing supply shortage. If insufficient housing stock exists, then Americans may be forced to live in closer quarters than they would otherwise prefer. As suggested by Galster (citation), we calculate a simple counterfactual."),
          p("We divide Americans into fine-grained buckets based on their age, sex, race/ethnicity, geography, and birthplace (American-born or foreign-born). Within each of these buckets, we calculate average household size in both 2005 and 2022. We then calculate a simple counterfactual: "),
          p(em("Had average household sizes remained at their 2005 levels, but population demographics shifted to 2022 levels, what would we expect the average American household size to be?")),
          
          tags$h3("Table 1: Hypothetical Example", id = "table1"),
          
          p(paste("Table 1 shows a simple theoretical example of population proportions",
                  "and average household sizes across different demographic groups in 2005",
                  "and 2022. We keep things simple with just 2 groups in the population: White",
                  "Americans and Hispanic Americans. From 2005 to 2022 we imagine a hypothetical",
                  "situation where the White population, as a fraction of the total, decreases",
                  "by 10 percentage points while the Hispanic population increases commensurately.",
                  "Average household size in each group changes dramatically over the 17-year",
                  "period. White households grow in size from an average of 3.5 to 3.8, while",
                  "Hispanic households shrink from 5 to 4.")),
          p(paste("The `Actual Contribution (2005)` and `Actual Contribution (2022)`",
                  "columns are derived my multiplying average household size by population",
                  "percentage. The sum of the entries in each of these columns produces the",
                  "overall weighted average household size in that year. In our example,",
                  "average household size increased from 3.8 in 2005 to 3.86 in 2022.")),
          p(paste("The `Counterfactual Contribution (2022)` column reveals what household",
                  "size would be, had average household size by demographic group held steady",
                  "in the 17-year period. According to this calculation, had we been perfect",
                  "at predicting demographic change in the year 2005, we would have expected",
                  "that the average American in 2022 would live in a 3.95-person household.",
                  "The fact that the measured average household size in 2022 is actually 3.86",
                  "indicates that individuals are spread apart more, on average, than they",
                  "were in 2005.")),
          
          p(strong("Table 1")),
          DTOutput("table1"),
          
          p(paste("The `Difference from Counterfactual (2022)`",
                  "column shows us how much larger or smaller a given group's contribution",
                  "to the overall average household size is from what we expected",
                  "holding preferences constant at 2022 levels. The relatively small difference",
                  "between our 2022 counterfactual and actual result conceals large shifts",
                  "in each population. Although Hispanic Americans comprise a minority of the",
                  "population, their shrinking average household size more than compensated",
                  "for the increasing household sizes among White Americans.")),
          
          tags$h3("Table 2: Table 2: 2005-2022 Changes by Race and Age", id = "table2"),
          p(paste("Now we move on from an example to using actual data from IPUMS. Table 2",
                  "shows actual data for 8 race/ethnicity groups:")),
          tags$ul(
            tags$li("Asian Americans and Pacific Islanders (\"AAPI\")"),
            tags$li("American Indians and Alaska Natives (\"AIAN\")"),
            tags$li("Black Americans (\"Black\")"),
            tags$li("Hispanic Americans (\"Hispanic\")"),
            tags$li("Multiracial Americans (\"Multiracial\")"),
            tags$li("White Americans (\"White\")"),
            tags$li("All other self-identified (\"Other\")")
          ),
          p(paste("The table also splits Americans into 18 age buckets, which span 5 year",
                  "intervals from ages 0 to 84 as well as a bucket for age 85+ Americans. P-values",
                  "represent the result of a two-tailed test on whether average household sizes",
                  "differ between 2005 and 2022. P < 0.05 is the naive result on whether the",
                  "difference is significant at a 5% level. The Bonferroni correction makes",
                  "the P value threshold significantly more strict, to account for repeated",
                  "tests. The total probability of at least one false positive result in the",
                  "126 tests is just under 5% after the Bonferroni correction is applied. [add source]"),
            p("proof: Probability of no false positive with p < 0.05: (1-0.05)^126 = 0.95^125 = 0.00156, meaning there is a 99.844% chance of at least one false positive. Probability of no false positive with p < 0.05/126 = (1 - 0.05/126)^126 = 0.999603^126 = 0.9512, meaning there is just under a 5% chance of at least one false positive among the Bonferroni-corrected results."),
            p(HTML(paste("There are ",
                         data_for_table$sig_bonferroni |> sum(),
                         "significant results out of the ",
                         nrow(data_for_table),
                         "tests run at the P &le; ",
                         (0.05 / nrow(data_for_table)) |> round(4) |> format(scientific = FALSE),
                         "level used in the strict criteria for the Bonferroni test."))),
            
            p(strong("Table 2")),
            DTOutput("table2"),
            
            tags$h3("Table 3: Contributions", id = "table3"),
            p("The total average household size in 2022 is ",
              hhsize_2022 |> round(3),
              ". The counterfactual household size in 2022, holding preferences fixed from 2005, is ",
              cf_hhsize_2022 |> round(3),
              ".",
              strong("In other words, Americans today live in households that are",
                     (hhsize_2022 - cf_hhsize_2022) |> round(3),
                     "person larger than demographic shifts alone would predict."),
              "[TODO: Add: 'This difference is statistically significant at the p < xxxx level.']"),
            
            p("We scale this value of",
              (hhsize_2022 - cf_hhsize_2022) |> round(3),
              "to 100% and input individual contributions in the `Difference from Counterfactual (2022) as Percent`",
              "column. The sum of all values in this column is 100%, though the absolute",
              "sum of the values is much larger due to large positive and negative values."),
            p("Table 3 shows that the white middle-aged adults tend to produce the most",
              "strongly positive contributions, while Hispanic young adults tend to produce the",
              "most negative contributions."),
            
            p(strong("Table 3")),
            DTOutput("table3"),
            
            h3("Waterfall Chart"),
            
            p("In Figure 1, we display the data on contributions as a waterfall."),
            
            p(strong("Figure 1")),
            plotOutput("figure1"),
            
            p("Due to their large portion of the population and large differences in household",
              "size, white Americans contribute the most to the demographic shift towards larger",
              "households. Hispanic Americans, Black Americans, and Asian Americans and Pacific",
              "Islanders, on net, have contributed toward diminished household size over the",
              "17-year observation period."),
            
            p("Figure 3 breaks down the top-line values from Figure 1 into age group buckets.",
              "Select a race/ethnicity group from the drop down menu to see the patterns for",
              "each group."),
            
            p(strong("Figure 3")),
            
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
                plotOutput("figure3")
              )
            ),
            
            p(strong("Figure 2")),
            
            fluidRow(
              column(
                width = 8,
                p("The graph below shows the trends in average household size across different age groups and years. Use the options to customize the view."),
                plotOutput("figure2")
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
            
          )
        )
      )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Table 1: Render theoretical example table
  output$table1 <- renderDT({
    example_table <- data.frame(
      group = c("White", "Hispanic"),
      perc_2005 = c(80, 20),
      perc_2022 = c(70, 30),
      hhsize_2005 = c(3.5, 5),
      hhsize_2022 = c(3.8, 4)
    ) |>
      mutate(
        cont_2005 = perc_2005 * hhsize_2005 / 100,
        cont_2022 = perc_2022 * hhsize_2022 / 100,
        cont_2022cf = perc_2022 * hhsize_2005 / 100,
        cont_diff = cont_2022 - cont_2022cf
      )
    
    sum_row <- example_table |>
      summarize(
        group = "Sum",
        perc_2005 = sum(perc_2005),
        perc_2022 = sum(perc_2022),
        hhsize_2005 = NA,
        hhsize_2022 = NA,
        cont_2005 = sum(cont_2005),
        cont_2022 = sum(cont_2022),
        cont_2022cf = sum(cont_2022cf),
        cont_diff = sum(cont_diff)
      )
    
    example_table <- bind_rows(example_table, sum_row) |>
      # TODO: divide percentages by 100 upstream in the code
      mutate(
        perc_2005 = perc_2005 /100,
        perc_2022 = perc_2022 / 100
      ) |>
      
      datatable(
        example_table,
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Group" = "group",
          "Percent of 2005 Population" = "perc_2005",
          "Percent of 2022 Population" = "perc_2022",
          "Average HH Size (2005)" = "hhsize_2005",
          "Average HH Size (2022)" = "hhsize_2022",
          "Actual Contribution (2005)" = "cont_2005",
          "Actual Contribution (2022)" = "cont_2022",
          "Counterfactual Contribution (2022)" = "cont_2022cf",
          "Difference from Counterfactual (2022)" = "cont_diff"
        )
      ) |>
      formatStyle(
        "Group",
        target = "row",
        fontWeight = styleEqual("Sum", "bold")
      ) |>
      formatRound("Difference from Counterfactual (2022)", digits = 2) |>
      formatPercentage(c("Percent of 2005 Population", "Percent of 2022 Population"), digits = 0)
  })
  
  # Table 2: Render actual data by Race/Ethnicity and Age
  output$table2 <- renderDT({
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
    ) |>
      formatSignif("P-value", digits = 3)
  })
  
  # Table 3: Render contributions table
  output$table3 <- renderDT({
    datatable(
      contributions,
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
        "Age" = "AGE_bucket",
        "Percent of 2022 Population" = "percent_2022",
        "Average HH Size (2005)" = "weighted_mean_2005",
        "Average HH Size (2022)" = "weighted_mean_2022",
        "Actual Contribution (2005)" = "cont_2005",
        "Actual Contribution (2022)" = "cont_2022",
        "Counterfactual Contribution (2022)" = "cont_2022cf",
        "Difference from Counterfactual (2022)" = "cont_diff",
        "Difference from Counterfactual (2022) as Percent" = "cont_diff_pct"
      )
    ) |>
      formatPercentage(
        "Difference from Counterfactual (2022) as Percent", 
        digits = 1) |>
      formatRound(c(
        "Percent of 2022 Population",
        "Average HH Size (2005)",
        "Average HH Size (2022)"), 
        digits = 2) |>
      formatSignif(c(
        "Actual Contribution (2005)",
        "Actual Contribution (2022)",
        "Counterfactual Contribution (2022)",
        "Difference from Counterfactual (2022)"), digits = 3)
  })
  
  # Render the summary waterfall chart (Figure 1)
  output$figure1 <- renderPlot({
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
  
  
  # Reactive expression for filtering data based on user input (Figure 2)
  filtered_data <- reactive({
    data_long %>%
      filter(RACE_ETH_bucket == input$race_eth_bucket)
  })
  
  # Render the household size plot: Figure 2
  output$figure2 <- renderPlot({
    data <- filtered_data()
    
    if (input$plot_type == "household_size") {
      p <- ggplot(data, aes(x = AGE_bucket, y = weighted_mean, color = year, group = year)) +
        geom_line(linewidth = 1, alpha = 0.8) +
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
              ymin = weighted_mean - se_weighted_mean * 1.96,
              ymax = weighted_mean + se_weighted_mean * 1.96
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
  
  # Render the interactive waterfall chart (figure 3)
  output$figure3 <- renderPlot({
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

# Run the application 
shinyApp(ui = ui, server = server)

