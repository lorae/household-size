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
  
  # Sidebar for potential controls (add inputs here as needed)
  div(class = "scroll-container",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          p(a("Introduction", href = "#intro")),
          p(a("Table 1: Hypothetical Example", href = "#table1")),
          p(a("Table 2: 2005-2022 Changes by Race and Age", href = "#table2")),
          p(a("Table 3: Contributions", href = "#table3"))
        ),
        
        # Main panel for displaying content
        mainPanel(
          tabsetPanel(
            tabPanel("Main", tab1_ui), # end of tabPanel
            tabPanel("Map", tab2_ui), # end of tabPanel
            tabPanel("Counterfactual", tab3_ui) # end of tabPanel
          )
        )
)))
