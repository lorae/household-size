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

source("ui/tab1.R")
source("ui/tab2.R")
source("ui/tab3.R")
source("ui/tab4.R")

# Define UI
ui <- fluidPage(
  
  # Enable mathematical notation
  withMathJax(),
  
  # Sidebar for potential controls (tab-specific)
  div(class = "scroll-container",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Conditional content for "Main" tab
          conditionalPanel(
            condition = "input.tabs == 'Framing'",
            p(a("Introduction", href = "#intro")),
            p(a("Table 1: Hypothetical Example", href = "#table1")),
            p(a("Table 2: 2005-2022 Changes by Race and Age", href = "#table2")),
            p(a("Table 3: Contributions", href = "#table3"))
          ),
          
          # Conditional content for "Map" tab
          conditionalPanel(
            condition = "input.tabs == 'Map'",
            p("Map-specific content goes here."),
            actionButton("refresh_map", "Refresh Map")
          ),
          
          # Conditional content for "Counterfactual" tab
          conditionalPanel(
            condition = "input.tabs == 'Counterfactual'",
            p("Counterfactual-specific sidebar content."),
            numericInput("num_input", "Example Input:", value = 5, min = 1, max = 10)
          )
        ),
        
        # Main panel for displaying content
        mainPanel(
          tabsetPanel(
            id = "tabs",  # Add ID to track the active tab
            tabPanel("Framing", tab1_ui), 
            tabPanel("Map", tab2_ui), 
            tabPanel("Counterfactual", tab3_ui)
          )
        )
      )))

