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
  
  DTOutput("table1tab4")
)