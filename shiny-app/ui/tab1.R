# Load necessary data
load("data/all_tables.rda")

# Define some constant variables used in the text description of the data
hhsize_2022 <- (contributions$percent_2022 * contributions$weighted_mean_2022/100) |> sum()
cf_hhsize_2022 <- (contributions$percent_2022 * contributions$weighted_mean_2005/100) |> sum()

# Define the UI
tab1_ui <- fluidPage(
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
  
  tags$h3("Constructing a Counterfactual", id = "table1"),
  
  tags$h4("Conceptual Explanation"),
  
  p("Suppose that we survey the population in two periods: \\(p = 0\\) and \\(p = 1\\).
    Let \\(\\pi_{i,p}\\) represent the proportion of the total population in period \\(p\\)
    belonging to population subgroup \\(i \\in \\{1, ..., S\\}\\), where subgroup proportions
    in each period sum to one ( \\( \\sum_{i = 1}^{S} \\pi_{i,p} = 1 \\) ). Average household
    size \\(h_{i,p}\\) is measured by population subgroup and period. Our base (reference) 
    period is \\(p = 0\\). 
    "),
  
  p("Actual population mean household size,
    as measured in \\(p = 1\\), is
    \\[
    H_{\\text{actual},p = 1} = \\sum_{i = 1}^{S} \\pi_{i,1} \\cdot h_{i,1}
    \\]
    Counterfactual population mean household size in period 1, holding household sizes
    fixed at period 0 levels, is
    \\[
    H_{\\text{cf},p = 1} = \\sum_{i = 1}^{S} \\pi_{i,1} \\cdot h_{i,0}
    \\]
    The difference between these two values represents the unexplained growth (or decline)
    in household size between periods 0 and 1, and can be broken down into a sum of
    contributions from individual population subgroups:
    \\[
    H_{\\text{actual},1} - H_{\\text{cf},1} = \\sum_{i = 1}^{S} \\pi_{i,1} \\cdot (h_{i,1} - h_{i,0})
    \\]
    The next section applies these calculations to a stylized data set.
    "),
  
  tags$h4("Stylized Example"),
  
  p("Table 1 presents hypothetical population proportions and average household sizes 
    across two demographic subgroups - White and Hispanic Americans - between 2005 and 
    2022. From 2005 to 2022, the proportion of the population that is White decreases 
    by 0.1 (from 0.8 to 0.7) while the Hispanic population increases commesurately. 
    Simultaneously, White households grow in size from 3.5 to 3.8, while
    Hispanic households shrink from 5 to 4."),
  p(HTML(
    "The <code>Actual Contribution (2005)</code> and <code>Actual Contribution (2022)</code>
    columns are derived my multiplying average household size by population proportion. 
    The sum of the entries in each of these columns produces the overall weighted average 
    household size in that year. In our example, average household size increased from 3.8 
    in 2005 to 3.86 in 2022."
    )),
  p(HTML("The <code>Counterfactual Contribution (2022)</code> column reveals what household
    size would be, had average household size by demographic group held steady in the
    17-year period. According to this calculation, had we been perfect at predicting 
    demographic change in the year 2005, we would have expected that the average American 
    in 2022 would live in a 3.95-person household."
    )),
  
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
  
  tags$h3("Getting more granular", id = "placeholder"),
  
  p("In this section we'll put specifically how the analysis then uses finer and finer
    bins of combinations of various variables. This will naturally segue into the missing
    data section, a problem that only arises when bins are extremely fine."),
  
  tags$h3("What about missing data?", id = "placeholder"),
  
  p("As we construct counterfactual situations that control for more and more factors,
    we run into the issue of an unbalanced panel dataset. Some combinations of factors -
    geography, race, age group, etc. - are simply so uncommon that they are not observed
    at all in one or both samples. This can pose issues in properly calculating a counterfactual, 
    as we show below."),
  
  p("We consider a stylized America with 3 groups 
    of interest - A, B, and C. We assume that membership in group A is sufficiently 
    unlikely that sometimes our survey does not capture these individuals in its
    random sampling. Under these circumstances, there are four possible scenarios:"),
  
  tags$ol(
    tags$li("(Base case) All groups are observed in both period 0 and period 1."),
    tags$li("All groups are observed in period 0, but group A is not observed in period 1."),
    tags$li("Group A is not observed in period 0, but all groups are observed in period 1."),
    tags$li("Group A is not observed in either period 0 or period 1.")
  ),
  
  p("In all four scenarios, we set average household sizes to grow by exactly 0.5 between
    period 0 (the year 2000) and period 1 (the year 2019).
    More specifically, group A initially has an average household size of 3 that grows to
    3.5, group B has an average household size of 4 that grows to 4.5, and group C
    initially has an average household size of 5 that grows to 5.5."),
  
  tags$h4("Scenario 1 (Base case): All groups are observed in both period 0 and period 1", id = "placeholder"),

  p("Because all groups are measured, and within each group, average household size grows
  by 0.5 persons between 2000 and 2019, our 2019 counterfactual household size of 
  4.26 is precisely 0.5 persons per household smaller than the actual measured household
  size of 4.76."),
  
  p(strong("Table 2A")),
  DTOutput("tab1table2a"),
  
  tags$h4("Scenario 2: All groups are observed in period 0, but group A is not observed in period 1", id = "placeholder"),

  p("In 2019, no members of group A are surveyed, leaving their average household 
    size unmeasured. However, because group A's estimated size is 0, this missing 
    value does not affect the actual or counterfactual measurements, and group A 
    is simply excluded from observation. The remaining data for groups B and C form 
    a balanced panel. As in scenario 1, the difference between the counterfactual 
    household size and actual household size is exactly 0.5. Unlike scenario 1, 
    however, these measured values are slightly elevated - average household size 
    is 4.8 instead of 4.76, for example - due to the exclusion of group A."),
  
  p(strong("Table 2B")),
  DTOutput("tab1table2b"),
  
  tags$h4("Scenario 3: Group A is not observed in period 0, but all groups are observed in period 1", id = "placeholder"),

  p("Group A is missing from the baseline survey in 2000. As such, it's impossible
    to construct a counterfactual contribution, since measured averages from 2000 are
    missing. Table 2C demonstrates the incomplete calculation. To produce a valid 
    counterfactual, we must construct an assumption about group A's average household
    size in 2000. The simplest approach is to assume that household size remains unchanged
    between 2000 and 2019, as is done in table 2D. The advantage of this approach - aside
    from its simplicity - is that it guarantees actual and counterfactual contributions
    are equal. This ensures that our data imputation has zero effect on the bottom-line
    difference between actual and counterfactual household size."),
  
  p("However, this form of data interpolation distorts the difference between 2019 actual and 
    counterfactual average household sizes - in this scenario, the difference is
    0.49, rather than 0.50. In general, the distortion will be small so long as the
    proportion of the population represented by group A is also small. This assumption is
    quite plausible, since it is precisely the smallest subgroups of the population that
    we're least likely to capture in our random sampling."),
  
  p(strong("Table 2C")),
  DTOutput("tab1table2c"),
  
  p(strong("Table 2D")),
  DTOutput("tab1table2d"),

  p("Given these limitations, another reasonable approach is to interpolate the average 
    household size in 2000 using nearby observations. For example, we
    could calculate the average change in household size from 2000 to 2019 and 
    subtract this change from group A's measured average household size in 2019 to 
    estimate the 2000 value. Alternatively, we could calculate the the difference in 
    average household size 
    between groups A and B in 2019 and subtract that constant from group B's 
    known value in 2000 to estimate group A's 2000 value."),
  
  p("Robusteness tests can be used to determine whether the interpolation strategy affects
    results. They are unlikely to, given the fact that only very small populations are
    likely to be sufficiently small to be unsampled in one of the two periods. Thus, their
    contribution to the overall 2019 average or counterfactual is likely to be small.
    [If desired, insert a sentence like, \"of the 100,000 unique combinations of 
    demographic factors observed in our data, only 1,000 rows display scenario 3.
    Even if all of them were zeroed out, their total contribution would be x.xxx.\"]]"),
  
  tags$h4("Scenario 4: Group A is not observed in either period 0 or period 1", id = "placeholder"),
  
  p("If group A is not observed in either year, then our dataset is once again a balanced
    panel, with actuals and counterfactuals in 2019 simply defined."),
  
  p(strong("Table 2E")),
  DTOutput("tab1table2e"),
  
  p("This scenario is functionally equivalent to scenario 2, where group A was not 
    observed in the recent sample. Both scenarios are based upon an observation of
    no group A members and therefore have identical actual and counterfactual 
    measurements. This also means that the differences between actual and counterfactual
    measurements sum to precisely 0.5, the change in average household size."),
  
  p("[Side note: what if, later we draw from the 
    census and it's literally not true that there exists a person in group A in X 
    year? This is conceptually different from just not observing a person in group 
    A in X year. A later extension of this write-up can explore this situation, but
    for now, I'll focus on the simpler one outpined here.] [Note: do income quintile
    and inflation adjusted income.]"),
  
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
    )
  )
)