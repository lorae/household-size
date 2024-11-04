# app.R

library(shiny)
library(DT)
library(dplyr)

# Load necessary data
load("data/all_tables.rda")

# Define UI
ui <- fluidPage(

    # Sidebar for potential controls (add inputs here as needed)
  div(class = "scroll-container",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          p(a("Introduction", href = "#intro")),
          p(a("Table 1: Theoretical Example", href = "#table1")),
          p(a("Table 2: Actual Example - By Race/Ethnicity and Age", href = "#table2")),
          p("A third test entry")
        ),
    
    # Main panel for displaying content
    mainPanel(
      titlePanel("Changes in American Household Size: 2005 - 2022"),
      
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
      p("The answer to this question reveals the reasons American households are changing in size. We'll refer to changes in average household size explained by demographic changes in the underlying population as ",
        strong("compositional"),
        "shifts and changes explained by shifts within individual demographic groups as ",
        strong("preference"),
        "shifts. [footnote: there are some caveats on this characterization, described in Section 3 (write section 3!!!)]. If we find that preference shifts point to increasing average household size, that may suggest that there is a housing supply shortage."),
      
      tags$h3("Table 1: Theoretical Example", id = "table1"),
      p("Table 1 shows a simple theoretical example of population proportions and average household sizes across different demographic groups in 2005 and 2022. We keep things simple with just 2 groups in the population: White Americans and Hispanic Americans. From 2005 to 2022 we imagine a hypothetical situation where the White population, as a fraction of the total, decreases by 10 percentage points while the Hispanic population increases commensurately. Average household size in each group changes dramatically over the 17-year period. White households grow in size, lifting up the average, while Hispanic households shrink."),
      p("The `Actual Contribution (2005)` and `Actual Contribution (2022)` columns are derived my multiplying average household size by population percentage. The sum of the entries in each of these columns produces the overall weighted average household size in that year. In our example, average household size increased from 3.8 in 2005 to 4.0 in 2022."),
      p("The `Counterfactual Contribution (2022)` column reveals what household size would be, had average household size by demographic group held steady in the 17-year period. According to this calculation, had we been perfect at predicting demographic change in the year 2005, we would have expected that the average American in 2022 would live in a 3.95-person household. The fact that the measured average household size in 2022 is actually 4.0 indicates that individuals are bunching together more, on average, than they were in 2005."),

      DTOutput("table1"),
      
      p("Where are these preference shifts concentrated? Changes in the `Average HH Size` columns from 2005 to 2022 provides a helpful indication. But these differences do not account for the strength of each group's effect on the overall weighted average American aggregate. A large shift among a small group will oftentimes have a more modest effect on the overall population than a smaller shift in a large group."),
      p("The `Difference from Counterfactual (2022)` column shows us how much larger (or smaller) a given group's contribution to the overall average household size is from what we expected naively holding preferences constant at 2022 levels. The relatively small difference between our 2022 counterfactual and actual result conceals large preference shifts in each population. The decrease in average size of a Hispanic household, which fell on average by 1 person over the 17-year period, nearly fully cancelled out the increase in average size of a White household. Thus, indicating the large absolute differences in the `Difference from Counterfactual` column can reveal where these changes are most impactful on population aggregates."),
      
      tags$h3("Table 2: Actual Example - by Race/Ethnicity and Age", id = "table2"),
      p("Now we move onto an example using actual population values. The following table shows actual data for 8 race/ethnicity groups:"),
      tags$ul(
        tags$li("Asian Americans and Pacific Islanders (\"AAPI\")"),
        tags$li("American Indians and Alaska Natives (\"AIAN\")"),
        tags$li("Black Americans (\"Black\")"),
        tags$li("Hispanic Americans (\"Hispanic\")"),
        tags$li("Multiracial Americans (\"Multiracial\")"),
        tags$li("White Americans (\"White\")"),
        tags$li("All other self-identified (\"Other\")")
      ),
      p("The table also splits Americans into 18 age buckets, which span every 5 year period from 0 to 84 as well as a bucket for age 85+ Americans. P-values represent the result of a two-tailed test on whether average household sizes differ between 2005 and 2022. P < 0.05 is the naive result on whether the difference is significant at a 5% level. The Bonferroni correction makes the P value threshold significantly more strict, to account for repeated tests. The total probability of at least one false positive result in the 126 tests is just under 5% after the Bonferroni correction is applied. [Source?]"),
      p("proof: Probability of no false positive with p < 0.05: (1-0.05)^126 = 0.95^125 = 0.00156, meaning there is a 99.844% chance of at least one false positive. Probability of no false positive with p < 0.05/126 = (1 - 0.05/126)^126 = 0.999603^126 = 0.9512, meaning there is just under a 5% chance of at least one false positive among the Bonferroni-corrected results."),
      
      DTOutput("table2"),
      
      p(HTML(paste("There are ",
        data_for_table$sig_bonferroni |> sum(),
        "significant results out of the ",
        nrow(data_for_table),
        "tests run at the P &le; ",
        (0.05 / nrow(data_for_table)) |> round(4) |> format(scientific = FALSE),
        "level used in the strict criteria for the Bonferroni test, clearly indicating that there are several preference shifts in the American population over the 17-year observation period, at least as categorized under this scheme."
        ))),
      p("Let's disambiguate these changes."),
      
      h3("Table 3: Contributions Table"),
      p("The total average household size in 2022 is ",
        (contributions$percent_2022 * contributions$weighted_mean_2022/100) |> sum() |> round(3),
        ". The counterfactual household size in 2022, holding preferences fixed from 2005, is ",
        (contributions$percent_2022 * contributions$weighted_mean_2005/100) |> sum() |> round(3),
        ". In other words, average households are ",
        ((contributions$percent_2022 * contributions$weighted_mean_2022/100) |> sum()  ) - ((contributions$percent_2022 * contributions$weighted_mean_2005/100) |> sum() ) |> round(3),
        "larger on average than the counterfactual would estimate."),
      p("Sorting the table by `Percentage of Counterfactual Difference`, either ascending or descending, is illuminating. At first glance, the percentages seem impossible: Many are in excess of 10%, and it is hard to imagine how they would add up to 100. But strongly positive and strongly negative numbers contribute to the total sum equalling 100."),
      
      DTOutput("table3"),
      
      h3("Waterfall Chart"),
      plotOutput("waterfall_chart"),
      
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
      hhsize_2022 = c(4.0, 4)
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
    
    example_table <- bind_rows(example_table, sum_row)
    
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
      formatRound("Difference from Counterfactual (2022)", digits = 2)
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
        "Percentage of Counterfactual Difference" = "cont_diff_pct"
      )
    ) |>
      formatRound(c(
        "Percent of 2022 Population",
        "Average HH Size (2005)",
        "Average HH Size (2022)",
        "Percentage of Counterfactual Difference"), digits = 2) |>
      formatSignif(c(
        "Actual Contribution (2005)",
        "Actual Contribution (2022)",
        "Counterfactual Contribution (2022)",
        "Difference from Counterfactual (2022)"), digits = 3)
  })
  
  # Placeholder for the waterfall chart
  output$waterfall_chart <- renderPlot({
    # Replace this with actual chart creation code using your `contributions` data
    plot(1, 1, main = "Waterfall Chart Placeholder")  # Placeholder plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
