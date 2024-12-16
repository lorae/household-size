# ui.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)

# Load necessary data
load("data/all_tables.rda")
# Source needed helper functions
source("../src/utils/graphing-tools.R")

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
          tabsetPanel(
            tabPanel("Main",
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
        ), # end of tabPanel
        tabPanel("Map",
          p("Hi I'm a map."),
          plotOutput("minnesota")
        ) # end of tabPanel
      )
  ) # end of mainPanel
)))