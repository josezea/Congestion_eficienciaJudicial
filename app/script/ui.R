###################
# ui.R
# 
# Initializes the ui. 
# Used to load in your header, sidebar, and body components.
###################
source('./components/header.R', encoding = "UTF-8")
source('./components/sidebar.R', encoding = "UTF-8")
source('./components/body.R', encoding = "UTF-8")


ui <- dashboardPage(
  header = header,
  sidebar =  sidebar,
  body = body)
