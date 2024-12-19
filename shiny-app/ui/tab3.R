# Define the UI
tab3_ui <- fluidPage(
  tags$h3("Introduction", id = "introtab3"),
  p("In this section, we run a detailed calculation to determine how many
            more (or fewer) housing units we'd need to match the average household 
            densities in the year 2000."
  ),
  p("Prior to producing the results, however, we'll walk through a conceptual
            outline that explains how the Galster (YYYY) proposed method relates to
            OLS regression techniques and shift-share analysis."
  ),
  tags$h3("1: Conceptual Explanation", id = "section1tab3"),
  p("Imagine a simple America with a population of 8 individuals. We have
          survey information about their sex and race. Table 1 shows a prospective 
          population in the year 2000."),
  
  DTOutput("table1tab3"),
  
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
  
  verbatimTextOutput("codeblock01"),
  
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
            pevious regression.
            
            Here, we run the regression in R again:"),
  
  verbatimTextOutput("codeblock02"),
  
  p("In summary, this methodology is functionally equivalent to running a
            0-intercept regression with only interaction terms. We now turn our 
            attention to actal data."),
  
  
  tags$h3("2: Application", id = "section2tab3"),
  
  p("The following data are actual breakdowns of household size, population, and
    number of households in 2005 and 2022 from the American Community Survey."),
  
  DTOutput("table1tab4"),
  
  p("Our goal is to calculate the size of the housing shortage or surfeit, after
    controlling for demographic changes between 2005 and 2022. Here, in this simple
    example, we control for any changes in the size of the male and female population.
    Calculating the counterfactual 2022 number of households should be easy, right?
    We'll simply take the population in each group in 2022, divide by the average 
    household size in 2005, and then compare the numbers we get to the number of 
    households measured in 2022."),
  
  p("Unfortunately, this approach does not work. Dividing the female population of
    167 million in 2022 by the 2005 average household size of 3.29 gives us a counterfactual
    population of roughly 51 million households: that's much lower than our measured 67
    million households with women in them in 2022."),
  
  p("wait- what am I even measuring in hh_2022?"),
  
  p("Above text: to be continued."),
  
  tags$h3("Mean household size: person-level versus household-level measurement", id = "section3tab3"),

  p(HTML(
    "In this project, we conceptualize two methods of measuring household size:
    at the <em>person-level</em> and at the <em>household-level</em>.")),
  p(HTML(
    "<strong>Household-level:</strong> For every 
    household \\( i \\) with number of members \\( m_i \\) in households \\( 1 \\) 
    through \\( H \\), the <em>household-level</em> mean household size is a simple
    average across households as individual observations:
    \\[ 
    \\text{(Mean HH Size)}_{\\text{household}} 
    = \\frac{\\sum_{i = 1}^H m_i}{H} 
    = \\frac{P}{H}
    \\]
    ")),
  
  p(HTML(
    "<strong>Person-level:</strong>  For every 
    person \\( j \\) in a population \\( P \\) living in a household of \\( m_j \\) 
    members \\( 1 \\), the <em>person-level</em> mean household size is a simple
    average across person-level observations:
    \\[ 
    \\text{(Mean HH Size)}_{\\text{person}} 
    = \\frac{\\sum_{j = 1}^P m_j}{P} 
    = \\frac{\\sum_{j = 1}^P m_j}{\\sum_{i = 1}^H m_i} 
    = \\frac{\\sum_{i = 1}^H m_i^2}{\\sum_{i = 1}^H m_i} 
    \\]
    ")),
  
  p(HTML(
  "In other words: The <em>person-level</em> household size is the expected size 
  of a household a random member of the U.S. population lives in. The 
  <em>household-level</em> household size is the expected number of members in a 
  randomly-selected household.
  ")),
  
  p(
  "Figure 3.1 depicts a stylized American population with of three households, 
  with four, two and one member each. 
  "),
  
  p(strong("Figure 3.1")),
  plotOutput("plot1tab3"),
  
  p(
  "The household-level average household size 
  is:
    \\[ \\frac{4 + 2 + 1}{3} = 2 \\tfrac{1}{3} \\]
  
  but the person-level average household size is:
    \\[ 
    \\frac{4+4+4+4 + 2+2 + 1}{4 + 2 + 1} 
    = \\frac{4^2 + 2^2 + 1^2}{7} 
    = 3
    \\]"),
  
  p("In general, the person-level household size can be rewritten as
    \\[ 
    \\text{(Mean HH Size)}_{\\text{person}} 
    = \\frac{\\sum_{j = 1}^P m_j}{P}
    = \\frac{\\sum_{i = 1}^H m_i^2}{P} 
    = \\frac{\\sum_{i = 1}^H m_i^2}{\\sum_{i = 1}^H m_i}  
    \\]
    In other words, the relationship be
    Which is remarkably similar to the formula for variance. The greater dispersion
    in the sizes of households, th
    "),
  
  p("Imagine a prospective U.S. population with 3 households, combined of males
    and females living together in the combination shown in Figure 1, below."),
  
  p("todo: universally name figures across all tabs."),
  

  

  
  p("")
  
  
)