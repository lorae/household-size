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

  
  
  tags$h3("2: Constructing a housing counterfactual", id = "section2tab3"),
  
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
  
  p("In general, if we're counting the full population, the person-level household 
  size can be rewritten as 
    \\[ 
    \\text{(Mean HH Size)}_{\\text{person}} 
    = \\frac{\\sum_{j = 1}^P m_j}{P}
    = \\frac{\\sum_{i = 1}^H m_i^2}{P} 
    = \\frac{\\sum_{i = 1}^H m_i^2}{\\sum_{i = 1}^H m_i}  
    \\]
    In other words, the person-level household size is the sum of squared household
    sizes, divided by the total population. The household-level household size is
    the total population divided by the number of households. And if the household-level
    and person-level household sizes are multiplied together, the result is very 
    close to the variance of household sizes:
    \\[
    \\text{(Mean HH Size)}_{\\text{person}} \\cdot \\text{(Mean HH Size)}_{\\text{household}}
    = \\frac{\\sum_{i = 1}^H m_i^2}{P} \\cdot \\frac{P}{H}
    = \\frac{\\sum_{i = 1}^H m_i^2}{H}
    \\]
    Which is the second raw moment of the dataset of household-level household size
    observations.
    "),
  
  p(strong("Why person-level household size and number of households are not 1:1")),
  
  p(HTML("As shown above, person-level household size is closely related to the distribution
    variance, and household-level household size is closely related to the distribution 
    mean. One value does not directly imply the other. This means that if we have person-level
    household size and number of individuals, it is <em>not possible</em> to determine the
    number of households that is implied.")),
  
  p("Given an average person-level household size and a population, the nubmer of households
    is not deterministic. Consider the following counterexample. There is a population of 6 individuals.
    The average person-level household size is 3. How many households are there in the population?"),
  
  p(strong("Figure 3.2")),
  plotOutput("plot2tab3"),
  
  p("Figure 3.2 shows two hypothetical Americas consistent with the summary statistics. 
  In scenario 1, average person-level household size is
    \\[ 
    \\frac{3+3+3 + 3+3+3}{6} = 3
    \\]
    While in scenario 2, average person-level household size is
    \\[ 
    \\frac{1 + 1 + 4+4+4+4}{6} = 3
    \\]
    "),

  p("How, then, with multiple household configurations, can we determine the size of a 
    housing shortage using actual and counterfactual measures of person-level household
    size? There are certain properties of the summary statistic we can exploit. For example,
    the most compact configuration consistent with a set of summary statistics is one
    where each household has as close to as many of the same members as possible. The
    least compact configuration is one where there are some number of 1-person households
    and one very large household with the rest of the population. Both can be consistent
    with a set of summary statistics, while the truth lies somewhere in the middle."),
  
  p("I've actually come up with some proofs of these properties, which I may later add
    to this."),
  
  p("At this point I could run a computer simulation. Start from low levels of P and show the possible
    person-level household sizes by running every possible partition of P and calculating its person-level
    and household-level household size. Perhaps map them on a scatter plot, or something."),
  
  p("Do not despair! Our aggregates can tell us something very interesting about total
  households in the United States! I have a postulate. I'll start by outlining it for 
    a simple case where the integers work, and then we'll go into a more complex, realistic
    case."),
  
  p("assertion: for every pairing of aggregate household size alpha and number of people N,
    there is a minimum number of households possible in the population that is bounded
    by N/alpha. In the case where alpha is a whole number and N is divisible by alpha,
    the valid configuration with the minimum number of households is one with exactly
    N/alpha households and each household has exactly alpha members."),
  
  p("Why does this matter? It matters SO much! Using this information, we can come up with
    a MINIMUM estimate on hte size of the housing shortage. This tells us, even in the most
    efficiently packed population matching these household characteristics, we would have
    had to build a minimum of X million homes. The number could be larger than that, but it
    cannot be smaller."),
  
  p("Let me give a numerical example. Suppose, as before, you have N = 6 total members in the 
    population with an average person level household size of alpha = 3. I assert that there 
    is no possible way you can configure the 6 people into fewer than 2 households, preserving
    that alpha = 3 (and is no greater)."),
  
  p("Let's suppose you have the configuration {1, 1, 4}. We measure our provisional alpha, which
    we'll call a. a = 1^2 + 1^2 + 4^2 / 6 = 18 / 6 = 3."),
  
  
  p(strong("Why use person-level metrics?")),
  
  p("Household-level average household sizes are what we're more familiar with:
    they're what is reported by [source], [source], and [source]. Why, then, use
    person-level metrics?"),
  
  p(HTML(
  "The problem lies in controlling for demographics. We're interested in how housing
    supply has surpassed or failed to keep up with the number of people in the country,
    while acknowledging that demographic shifts alone could account for changes in average
    household size without a supply shortage needed as an explanation. (For more
    information, please see the hypothetical example described in the \"Main\"
    tab of this document.) Hispanic individuals, for example, tend to live in 
    larger-than-average households, and the United States share of Hispanic individuals
    has grown in the last 20 years. But what constitutes a \"Hispanic\" household, 
    and how should we classify households with individuals of varying races, ethnicities,
    ages, and other potentially explanatory sociodemographic characteristics? Previous
    work [cite] has responded to this challenge by using the head of household's 
    characteristics to classify the household. But doing so may misclassify many
    people. Using the 2022 American Community Survey, 
    <a href=\"https://www.jchs.harvard.edu/blog/identifying-racial-and-ethnic-diversity-within-us-households\">Airgood-Obrycki and co-authors</a> find that over 10% of households contain at least 
    two adults of different races or ethnicities. \"Among these multi-race households\",
    they write, \"nearly half, 6.7 million (5.1 percent of all households), appear 
    in traditional statistics as white households despite having a person of color 
    present.\" These multi-race households differentiated themselves from single-race
    households along many dimensions related to housing: members of multi-race 
    households are younger, more likely to live with roommates, and more likely to
    be within a \"prime working age\" range of 25 to 54. And [If there's a source that says this:] 
    XX and YY show that heads of household are more likely than other adults in 
    the same residence to have aaa, bb, and ccc characteristics]."
  )),
  
  p("Another argument for using person-level characteristics is the circular thinking
    that arises from treating households as units. Our analysis does not claim to 
    be causal - nevertheless, we hope to uncover associations that may give clues
    into causal mechanisms underlying household formation (like a supply shortage).
    If we use household-level characteristics to predict subsequent household formation,
    we're endogenizing our inputs and outputs. Consider the set of households presented
    in Figure X."),

  p(strong("Merits of household-level classification")),
  
  p("Many of the abovementioned issues [except for the endogeneity? I'll have to
    think about it] can be allieviated by employing a smart household-level data 
    design. TAble 3.1 shows a prospective dataset aimed at
    constructing a 2022 counterfactual using 2005 data."),
  
  p("TODO: make a table and put it here"),
  
  p(HTML(
  "Each row of Table 3.1 represents a unique household as observed in 2005 and 
  identified using a <code>HHID</code>. Rather than indicating a particular race 
  or ethnicity for the entire household, we instead define an array of indicator 
  variables <code>contains_AAPI</code>, <code>contains_AIAN</code>, etc which are
  assigned <code>TRUE</code> if at least one household member belongs to its designated group.
  Age bucketed variables <code>num_0_4</code>, <code>num_5_9</code> are assigned
  integers that describe the number of individuals within the household falling within
  the designated category. Total household income is designated in the <code>hhincome</code>
  column."
  )),
  
  p("todo: universally name figures across all tabs."),
  

  

  
  p("")
  
  
)
