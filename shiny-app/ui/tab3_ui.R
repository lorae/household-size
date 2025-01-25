# Define the UI
tab3_ui <- fluidPage(
  tags$h3("Results", id = "results"),
  p("Figure 3.1 shows counterfactual household sizes under a variety of controls.
    A 1 indicates that a control was used in a given counterfactual simulation, and
    a 0 indicates that the control was not used."),
  
  DTOutput("fig3.1"),
  
  p("Figure 3.2 models occupancy per bedroom under an identical set of counterfactuals."),

  DTOutput("fig3.2"),
)
