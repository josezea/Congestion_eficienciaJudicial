###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("Distrito judicial", tabName = "distrito", icon = icon("balance-scale")),
    menuItem("Circuito", tabName = "circuito", icon = icon("university")),
    menuItem("Municipio", tabName = "municipios", icon = icon("gavel"))
    
  ),
  tags$style( type="text/css", "textarea {width:80%}","p.center{text-align:center;}"),
  tags$textarea(id="my_textarea",style="background-color:lightblue", rows=25,
                placeholder = HTML("W\u00E4hrung \u00C9  "), ""))
