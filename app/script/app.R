###################
# app.R
# 
# Main controller. 
# Used to import your ui and server components; initializes the app.
###################
library(shiny)
library(shinydashboard)
library(markdown)


#source('./global.R', encoding = "UTF-8")
source('./ui.R', encoding = "UTF-8")
source('./server.R', encoding = "UTF-8")


shinyApp(ui, server)
