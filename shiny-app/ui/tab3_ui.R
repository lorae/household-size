# Define the UI
tab3_ui <- fluidPage(
  
  # Include custom CSS for styling the cell classes
  tags$head(tags$style(HTML("
    .tag { padding: 4px 8px; border-radius: 4px; }
    .num-high { background-color: #FFB6C1; } /* Light Pink */
    .num-med { background-color: #FFD700; } /* Gold */
    .num-low { background-color: #90EE90; } /* Light Green */
  "))),
  
  tags$h3("Counterfactuals", id = "results"),
  p("Table 3.1 shows counterfactual household sizes under a variety of controls.
    A 1 indicates that a control was used in a given counterfactual simulation, and
    a 0 indicates that the control was not used."),
  
  DTOutput("tab3.1"),
  
  p("Table 3.2 models occupancy per bedroom under an identical set of counterfactuals."),

  DTOutput("tab3.2"),
  
  tags$h3("State-level Dot Plots", id = "boxwhisker"),
  
  p("In IPUMS, the smallest geographic unit availabe that is consistent across the
    years we study is the CPUMA0010 region. The designation of a CPUMA0010 region is 
    decided at the state-level based on population counts to ensure data anonymity.
    As such, some less populated statess have only one CPUMA0010 region, while others
    have many."),
  
  p("Tables 3.3 and 3.4 display CPUMA-level actual versus counterfactual results. 
    The black vertical line at 0 represents a situation where a regions's average number
    of persons per household or persons per bedroom in 2019 is exactly equal to the counterfactual
    based in 2000. A value to the right of the solid black line indicates that the
    region's measured density in 2019 exceeds the counterfactual expectations. A value
    to the left of the line indicates that a region's measured density falls below 
    counterfactual expectations. Observations are at the CPUMA0010 level."),
  
  p("The dashed red and blue lines correspond to the weighted mean and weighted median
    total for the given state. Values are measured in persons
    per household (table 3.3) or persons per bedroom (table 3.4). Weights are given by CPUMA0010-level population in 2019.
    So, for example, in table 3.3, the weighted median of 0.049 for 
    Alabama indicates that on average, Alabama households had 0.049 more people than 
    expected from the counterfactual."),
  
  p("[TODO: add persons per household for each state as a column in the chart.]"),
  
  p(HTML("Entries in the table are softed alphabetically by default, but can be sorted 
    by ascending or descending values by clicking the chart headers <code>Weighted median</code>
    or <code>Weighted mean</code>. Values can be returned to alphabetical order by
    clicking on the <code>State</code> column name.")),
  
  p("Both tables display all 50 states and the District of Columbia."),
  
  tags$h4("Household size counterfactuals, by state"),
  
  p(strong("Table 3.3: Household size counterfactuals, by state and CPUMA0010")),
  
  p("The dot plots are cut off at +0.5 and -0.5. A very limited number of observations (roughly
    3 total) [TODO: get exact amount. Mark these dots as red on the dotplot] of >1000 
    CPUMA0010s [TODO: get exact number] are not displayed because they fall outside the (-0.5, 
    +0.5) range"),

  reactableOutput("tab3.3")
)
