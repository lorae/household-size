# app.R

library(shiny)

# Load necessary data
load("data/all_tables.rda")

# Load UI and server
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)

