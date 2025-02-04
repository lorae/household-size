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
  p("Figure 3.1 shows counterfactual household sizes under a variety of controls.
    A 1 indicates that a control was used in a given counterfactual simulation, and
    a 0 indicates that the control was not used."),
  
  DTOutput("fig3.1"),
  
  p("Figure 3.2 models occupancy per bedroom under an identical set of counterfactuals."),

  DTOutput("fig3.2"),
  
  tags$h3("State-level box and whiskers", id = "boxwhisker"),
  
  p("In this section, we'll put 50 box-and-whisker plots: one for each state."),
  
  reactableOutput("tab3.1")
)
