###################
# header.R
# 
# Create the header for the ui.
###################
#header <- dashboardHeader(title = "Eficencia judicial")
header <- dashboardHeader(title = "Eficiencia - judicial",
                            tags$li(a(href = 'https://www.minjusticia.gov.co/',
                                      img(src = 'https://upload.wikimedia.org/wikipedia/commons/f/f5/Minjusticia_Colombia.svg',
                                          title = "Minjusticia", height = "50px", width = "200px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"
                            ))


