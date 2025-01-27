# ui.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(shinyAce)
library(shinyBS)

# Load necessary data
load("data/all_tables.rda")
# Source needed helper functions
source("graphing-tools.R")

source("ui/tab1_ui.R")
source("ui/tab2_ui.R")
source("ui/tab3_ui.R")
source("ui/tab4_ui.R")

# Define UI
ui <- fluidPage(
  
  # Enable mathematical notation
  withMathJax(),
  
  tags$head(
    tags$style(HTML("
      .subentry {
        font-size: 0.9em; /* Smaller font size */
        margin-left: 15px; /* Indent to distinguish it visually */
      }
    "))
  ),
  
  # Sidebar for potential controls (tab-specific)
  div(class = "scroll-container",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Conditional content for "Main" tab
          conditionalPanel(
            condition = "input.tabs == 'Methodology'",
            p(a("1 Introduction", href = "#01introduction")),
            p(a("2 Defining the Counterfactual", href = "#02counterfactual")),
            p(a("2.1 Conceptual Explanation", href = "#02.1", class = "subentry")),
            p(a("2.2 Stylized Example", href = "#02.2", class = "subentry")),
            p(a("2.3 Population Subgroups", href = "#02.3", class = "subentry")),
            p(a("2.4 Missing Data", href = "#02.4", class = "subentry")),
            p(a("3: Group-Level Averages as Regression Coefficients", href = "#03regression")),
            p(a("4: 'Person-level' Versus 'Household-level' Averages", href = "#04averages"))
          ),
          
          # Conditional content for "Map" tab
          conditionalPanel(
            condition = "input.tabs == 'Map'",
            p("Map-specific content goes here."),
            actionButton("refresh_map", "Refresh Map")
          ),
          
          # Conditional content for "New Results" tab
          conditionalPanel(
            condition = "input.tabs == 'New Results'",
            p("This sidebar will eventually contain content."),
            numericInput("num_input", "Example Input:", value = 5, min = 1, max = 10)
          )
        ),
        
        # Main panel for displaying content
        mainPanel(
          tabsetPanel(
            id = "tabs",  # Add ID to track the active tab
            tabPanel("Methodology", tab1_ui), 
            tabPanel("Map", tab2_ui), 
            tabPanel("New Results", tab3_ui),
            tabPanel("Old Results", tab4_ui)
          )
        )
      ))
)
