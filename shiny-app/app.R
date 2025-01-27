# app.R

library(shiny)

# Source needed helper functions
source("graphing-tools.R")

# Load UI and server
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)

