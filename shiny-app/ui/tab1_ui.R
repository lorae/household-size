# Load necessary data
load("data/all_tables.rda")

# Define some constant variables used in the text description of the data
hhsize_2022 <- (contributions$percent_2022 * contributions$weighted_mean_2022/100) |> sum()
cf_hhsize_2022 <- (contributions$percent_2022 * contributions$weighted_mean_2005/100) |> sum()

# Define the UI
tab1_ui <- fluidPage(
  titlePanel("Methodology"),
  
  tags$h2("1 Introduction", id = "01introduction"),
  p("In 2005, the average American lived in a household of", 
    round(weighted.mean(crosstab_2005_2022$weighted_mean_2005, crosstab_2005_2022$weighted_count_2005), 3),
    "people. By 2022, average household size shrunk to",
    round(weighted.mean(crosstab_2005_2022$weighted_mean_2022, crosstab_2005_2022$weighted_count_2022), 3), "people."
  ),
  p("Some of this effect may be compositional. The demographic makeup of Americans has changed in the last 17 years. Older people, on average, tend to live in smaller households, and this shift toward an older America could explain the reduction in household size. At the same time, there are countervailing forces: the population of Hispanic and Latino Americans has grown, and those individuals tend to live in larger households than average."),
  p("We're interested in understanding how Americans' housing behaviors may or may not reflect a housing supply shortage. If insufficient housing stock exists, then Americans may be forced to live in closer quarters than they would otherwise prefer. As suggested by Galster (citation), we calculate a simple counterfactual."),
  p("We divide Americans into fine-grained buckets based on their age, sex, race/ethnicity, geography, and birthplace (American-born or foreign-born). Within each of these buckets, we calculate average household size in both 2005 and 2022. We then calculate a simple counterfactual: "),
  p(em("Had average household sizes remained at their 2005 levels, but population demographics shifted to 2022 levels, what would we expect the average American household size to be?")),
  
  p("From this framing, we have 3 research questions:"),
  
  tags$ol(
    tags$li(
      strong("After adjusting for sociodemographic changes in the U.S. population, are households 
      today larger or smaller than they were in the year 2000?"),
      
      p("Preliminary answer: Households today are marginally larger, on the order 
      of about 3.37 persons per household, versus the 3.35 expected value. This 
      answer is sensitive to which controls are used, and more sensitivity testing 
      is needed to see to determine whether the result is robust to alternative
      bucketing of the data (e.g. categorizing income by quintile instead of chaining
      using CPI-U, bucketing age into 10-year, rather than 5-year groups, alternative
      methods of categorizing race and ethnicity).")
    ),
    tags$li(
      strong(
      "Do Americans today live in more or less crowded conditions than they did in 2000, 
      as measured by average persons per bedroom?"),
      p("Consistent with prior research, we find a strong result that American households
      live in increasingly spacious conditions. Extrapolating from 2000 trends, we expect 
      the typical American to live in a household with a density of 0.9 persons
      per bedroom. Instead, Americans today live in households with a density of roughly
      0.8 persons per bedroom [TODO: fact check these exact #s, I know they're close.]. 
      This large gap is quite robust to alternative specifications and would imply a 
      substantial adjustment in Americans' living situationss.")
    ),
    tags$li(
      strong("What are the primary sociodemographic factors driving changes in household 
      size and living conditions?"),
      p("So far, we find that population aging has been pulling household size averages
      down, as older Americans are more likely to live alone. An increase in the 
      Hispanic population of the U.S. has been working in the opposite direction,
      pulling averages up. Notably, however, Latino Americans today live in substantially
      smaller households than they did 20 years ago - that said, they still exceed 
      the U.S. average."),
      p("We also observe in our data signs consistent with the idea that Americans are
      having children later - the peak number of co-occupants for adults now occurs 
      around the age of 35, as opposed to the age of 30 two decades ago. This timing
      shift in fertility, however, doesn't appear to have strong effects on overall aggregates"),
      p("We're still in the process of examining other trends by education level, birthplace,
      income, and sex.")
    ),
    tags$li(
      strong("What do the findings on residential density - persons per household and
      persons per bedroom - imply about a housing surfeit or surplus in the United
      States?"),
      p("Our preliminary findings on the number of people per household suggest that 
        the U.S. would have needed to build at least half a million additional homes 
        between 2000 and 2019 to maintain the same average household size. This is 
        a lower-bound estimate; using alternative data heuristics (see Section X.X 
        of the methodology document), the true figure is likely closer to 750,000. 
        However, even this estimate is low compared to other studies, such as XXX by 
        [CITE], YYY by [CITE], and ZZZ by [CITE]."),
      p("However, our findings on average persons per bedroom indicate that there may 
        still be significant mismatches in the types of homes available versus the types 
        demanded. Although the number of persons per home is similar to expectations, 
        the lower-than-expected number of persons per bedroom suggests that Americans 
        today live in more spacious accommodations than they did in 2000. This is 
        consistent with other research showing that square footage per American has 
        risen over the same period [fact check and cite]."),
      p("Combined with the observation that - outside of the Great Recession - home prices
      have grown rapidly over the past 20 years, this raises the hypothesis that the 
      supply of homes in the United States has not been able to keep up with shifting
      consumer tastes for larger homes with more bedrooms."),
      p(" Expand more on this. Because homes have gotten bigger, so clearly some of that
        demand is 'quenched'. But apparently aggresive housing market dynmaics and rapidly
        rising prices, to me, suggest that the rate of homebuilding still has not matched 
        changing preferences. Or, another explanation is that "),
      p("TODO: come up with HYPOTHESES and tests for this!!1"),
      p("A possible next direction we could take this analysis is to examine the baseline 
        characteristics of homes in the U.S. and calculate how many additional homes 
        would need to be built to align with current accommodation preferences. It 
        is possible that this value would significantly exceed the 750,000 estimate 
        and align more closely with higher estimates from other researchers.")
    ),
    tags$li(
      strong("What is the geographic profile of these findings? Where have households
      and occupancy density grown or shrunk the most compared to counterfactual predictions,
      and what does this imply about housing supply in specific areas of the U.S.?"),
      p("Answer TBD - Lorae is putting together some chloropleth maps.")
    )
  ),
  
  tags$h2("2 Defining the Counterfactual", id = "02counterfactual"),
  
  tags$h3("2.1 Conceptual Explanation", id = "02.1"),
  
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
  
  tags$h3("2.2 Stylized Example", id = "02.2"),
  
  p("Table 1.1 presents hypothetical population proportions and average household sizes 
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
    17-year period. According to this calculation, had we been perfect at forecasting 
    demographic change in the year 2005, we would have expected that the average American 
    in 2022 would live in a 3.95-person household."
    )),
  
  p(strong("Table 1.1")),
  DTOutput("tab1.1"),
  
  p(HTML(
    "The <code>Difference from Counterfactual (2022)</code> column shows how much 
    each subgroup's contribution to the overall average household size deviates from the 
    expected value under the counterfactual scenario, where preferences remain constant at 
    2022 levels. The relatively small bottom-line difference between the 2022 counterfactual 
    and actual result belies large shifts in each population. Although Hispanic Americans 
    comprise a minority of the population, their shrinking average household size 
    more than compensated for the increasing household sizes among White Americans.")),
  
  tags$h3("2.3 Population Subgroups", id = "02.3"),
  
  p("To calculate a realistic counterfactual, we divide the population among the same 
    subgroups represented by Galster (YYYY) [ADD link!]. Our analysis includes categories
    for age, race/ethnicity, sex, geography (CPUMA 2000 - 2010), income, educational
    attainment, and birthplace (U.S. or foreign). Tables 1.XX - 1.XY outline the variables
    and their categories in detail."),
  
  p("TODO: add an expandable section here that outlines all the variables and the 
    ways they are encoded."),
  
  bsCollapsePanel(
    title = "Variable Encoding",
    style = "info",
    p("The population is divided into categories based on the following variables:"),
    tags$ul(
      tags$li("Age: Grouped into 5-year intervals."),
      tags$li("Race/Ethnicity: Categories include White, Hispanic, Black, Asian, and Other."),
      tags$li("Sex: Male and Female."),
      tags$li("Geography: CPUMA 2000 - 2010 classifications."),
      tags$li("Income: Adjusted for inflation and divided into quintiles."),
      tags$li("Educational Attainment: High school, some college, and college graduate."),
      tags$li("Birthplace: U.S.-born vs. foreign-born.")
    )
  ),
  
  tags$h3("2.4 Missing Data: Unsampled Groups", id = "02.4"),
  
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
  
  p("As we show below, scenarios 1, 2, and 4 are very straightforward to handle. Scenario
    3 requires more care is taken in producing a meaningful data interpolation. A detailed
    explanation of how each case is handled is provided in the expandable section below."),
  
  bsCollapsePanel(
    title = "Missing Data Scenarios",
    style = "info",
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
    
    p(strong("Table 1.2A")),
    DTOutput("tab1.2a"),
    
    tags$h4("Scenario 2: All groups are observed in period 0, but group A is not observed in period 1", id = "placeholder"),
    
    p("In 2019, no members of group A are surveyed, leaving their average household 
    size unmeasured. However, because group A's estimated size is 0, this missing 
    value does not affect the actual or counterfactual measurements, and group A 
    is simply excluded from observation. The remaining data for groups B and C form 
    a balanced panel. As in scenario 1, the difference between the counterfactual 
    household size and actual household size is exactly 0.5. Unlike scenario 1, 
    however, these measured values are slightly elevated - average household size 
    is 4.8 instead of 4.76, for example - due to the exclusion of group A."),
    
    p(strong("Table 1.2B")),
    DTOutput("tab1.2b"),
    
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
    
    p(strong("Table 1.2C")),
    DTOutput("tab1.2c"),
    
    p(strong("Table 1.2D")),
    DTOutput("tab1.2d"),
    
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
    
    p(strong("Table 1.2E")),
    DTOutput("tab1.2e"),
    
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
    and inflation adjusted income.]")
  ),
  
  tags$h2("3: Group-Level Averages as Regression Coefficients", id = "03regression"),
  
  p(strong("Note: this section is a slight aside - for a high-level picture of methodology,
           skip to section 4, below.")),
  
  p("Calculating group-level averages for household size is mathematically equivalent 
    to running a regression with interaction terms and no intercept, as demonstrated
    below. This connection provides flexibility for future analyses, such as estimating 
    broader trends by subgroup or testing national-level coefficients."),
  
  bsCollapsePanel(
    title = "Detailed Explanation",
    style = "info",
    
    p("Imagine a simplified America in the year 2000. Census data records a population 
    of 8 individuals organized into 3 households, as shown in Table 1.3."),
    
    p(strong("Table 1.3: 2000 Census Results")),
    DTOutput("tab1.3"),
    
    p("Our objective is to calculate baseline person-level household sizes within each 
    population subgroup of interest. In this stylized example, we calculate average 
    household sizes among Black men, Black women, White men, and White women."),
    
    aceEditor("codeblock01_code", mode = "r", theme = "chrome", readOnly = TRUE, height = "150px"),
    verbatimTextOutput("codeblock01"),
    
    p(strong("Table 1B: Condensed 2000 Census Results")),
    DTOutput("table1btab3"),
    
    p("In Galster's formulation, we calculate mean household size within 
            every possible subgroup in the year 2000. Here, in our simple example,
            we define subgroups for every unique possible combination of race and 
            sex. In our later analysis, we'll include other - potentially mutable -
            characteristics such as income level and educational attainment."),
    
    p("[NOTE: * add footnote about how ACS defines race and sex. Sex can only be M 
            and F. Perhaps for a subgroup of transgender individuals, sex may change
            (look into this). Race could also possibly change - cite studies on 
            increasing tendency of Latinos to label themselves as 'other race' or 
            simply regard Hispanic/Latino as a race instead of identifying as solely
            White.]
            "),
    
    
    p("We could arrive at the same result in a regression using the following
          formula:"),
    
    p(HTML("\\[\\text{Household Size}_i 
                 = \\beta_{1}(\\text{Black}_i)(\\text{Female}_i)
                 + \\beta_{2}(\\text{Black}_i)(\\text{Male}_i)
                 + \\beta_{3}(\\text{White}_i)(\\text{Female}_i)
                 + \\beta_{2}(\\text{White}_i)(\\text{Male}_i)
                 \\]")),
    
    p(HTML(
      "Where indicator variables such as \\(\\text{Black}_i\\) equal 1 if individual 
          \\(i\\) is Black, and 0 otherwise. In this simple notation, each of the
          \\(\\beta\\) coefficients represents the average household size within that
          specific group. Note the lack of a \\(\\beta_0\\) term, fixing the y-intercept
          at 0: Any nonzero value would produce an overdetermined regression and force
          one of the \\(\\beta_1\\), \\(\\beta_2\\),\\(\\beta_3\\), or\\(\\beta_4\\) 
          coefficients to equal 0.
          
          A more concise notation for the same 0-intercept regression could be written
          as follows:")),
    
    p(HTML("\\[\\text{Household Size}_i 
                 = \\beta_{rs}(\\text{Race}_i)(\\text{Sex}_i)
                 \\]")),
    p("Where each \\(\\beta_{rs}\\) coefficients represents one of averages 
            for the four unique combinations of race and sex. Observations are 
            measured at the person level for each \\(i\\) individual, as in the
            previous regression.
            
            Here, we run the regression in R again:"),
    
    aceEditor("codeblock02_code", mode = "r", theme = "chrome", readOnly = TRUE, height = "150px"),
    verbatimTextOutput("codeblock02"),
    
    p("In summary, group-level averages can be found by running a regression with
  interaction terms only and a 0-intercept. Knowledge of the relationship between
  this two apporaches could be useful in the future, if - for example - we decide to
  run a population-wide regression with a non-interacted term on an attribute such
  as race or age, for example. The coefficients on these universal terms may allow us
  to draw more general insights on the direction of change in household size within 
  certain population subgroups of interest.")

  ),

)