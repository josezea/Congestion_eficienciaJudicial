###################
# body.R
# 
# Create the body for the ui. 
# If you had multiple tabs, you could split those into their own
# components as well.
###################


body <- dashboardBody(
  
  tags$script(HTML('
                                            $(document).ready(function() {
                                            $("header").find("nav").append(\'<div class="myClass"> Text Here Text Here Text Here Text Here </div>\');
                                            })
                                            ')),
  
    tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
  ),


  
  
  tabItems(
    
    ########################
    # First tab content
    ########################
    tabItem(
      tabName = "distrito",
      fluidRow(
        
        # CONTROLS
        box(
          
          title = "Controls",
          
          # Choose a column
          selectInput(
            "columnChoice",
            "Choose a column:",
            choices = colnames(df),
            selected = "n"),
          
          sliderInput("slider", "Number of observations:", 1, 100, 50),
          
          # Create an eventReactive element
          actionButton(
            inputId = "submit",
            label = "Submit column")
          
        ),
        # PLOT THE THTINGS
        box( plotOutput("histPlot") )
      )
    ),
    
    ########################
    # Second tab content
    ########################
    tabItem(
      tabName = "circuito",
      fluidRow(
        column(
          width = 6,
          selectInput("especialidad_circuito", "Especialidad:",
                      c("Civil" = "civil",
                        "Familia" = "familia",
                        "Laboral" = "laboral",
                        "Penal" = "penal",
                        "Promiscuo" = "promiscuo"), width = '30%')),

         
     
    
        column(
          width = 6,
                         valueBox(
                 value = "1,345",
                 subtitle = "Numero de Unidades",
                 icon = icon("code"),
                 width = 6,
                 color = "red",
                 href = NULL),
               valueBox(
                 value = "2,345",
                 subtitle = "Numero de juzgados",
                 icon = icon("code"),
                 width = 6,
                 color = "red",
                 href = NULL)
        )
        
        
      
      ),
      fluidRow(
              
        tabBox(width = 12,
          title = "Análisis eficiencia e ineficiencia judicial",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset2", height = "400px",
          tabPanel("Resumen circuitos", 
                   DT::DTOutput("tabla_circuitos_especialidad")),
          tabPanel("Circuito seleccionado", 
                   selectInput("selDistrito_tabCircuito","Seleccione el distrito:", as.character(sort(unique(bind_rows(lista_circuitos)$Distrito))) ),
                   selectInput("selCircuito_tabCircuito", "Seleccione el circuito:", as.character(sort(unique(bind_rows(lista_circuitos)$Circuito))) ),
                   DT::DTOutput("tabla_circuitoEspecialidad_seleccionado")           
                   ), 
          tabPanel("Visualización indicadores de circuito", 
                   selectInput("Tipovisualizacion_tabCircuito", 
                    "Seleccione la visualización de su interés:", 
                    c("Número de juzgados de circuitos, egresos y eficiencia", 
                      "Comparación eficiencia",
                      "Descongestión e ineficiencia juzgados de circuitos",
                      "Descongestión en escenario ideal de eficiencia de los circuitos")),
                   plotly::plotlyOutput("visualizacion_tabCircuito")                   )
          )
          
      
      
        )
      
    ),
    
    ########################
    # Third tab content
    ########################
    tabItem(
      tabName = "municipios",
      fluidRow(
        column(
          width = 6,
          selectInput("especialidad_municipios", "Especialidad:",
                      c("Civil" = "civil",
                        #"Familia" = "familia",
                        "Laboral" = "laboral",
                        "Penal" = "penal",
                        "Promiscuo" = "promiscuo"), width = '30%')),
        
        
        
        
        column(
          width = 6,
          valueBox(
            value = "1,345",
            subtitle = "Numero de Unidades",
            icon = icon("code"),
            width = 6,
            color = "red",
            href = NULL),
          valueBox(
            value = "2,345",
            subtitle = "Numero de juzgados",
            icon = icon("code"),
            width = 6,
            color = "red",
            href = NULL)
        )
        
        
        
      ),
      fluidRow(
        
        tabBox(width = 12,
               title = "Análisis eficiencia e ineficiencia judicial",
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "tabset3", height = "400px",
               tabPanel("Resumen municipios", 
                        DT::DTOutput("tabla_municipios_especialidad")),
               tabPanel("Municipio seleccionado", 
                        selectInput("selDistrito_tabMunicipio","Seleccione el distrito:", as.character(sort(unique(municipio_civil$Distrito))) ),
                        selectInput("selCircuito_tabMunicipio", "Seleccione el circuito:", as.character(sort(unique(municipio_civil$Circuito))) ),
                        selectInput("selMunicipio_tabMunicipio", "Seleccione el municipio:", as.character(sort(unique(municipio_civil$municipio))) ),
                        
                        DT::DTOutput("tabla_municipioEspecialidad_seleccionado")           
               ), 
               tabPanel("Visualización indicadores a nivel municipal", 
                        selectInput("Tipovisualizacion_tabMunicipio", 
                                    "Seleccione la visualización de su interés:", 
                                    c("Número de juzgados de circuitos, egresos y eficiencia", 
                                      "Comparación eficiencia",
                                      "Descongestión e ineficiencia juzgados de circuitos",
                                      "Descongestión en escenario ideal de eficiencia de los circuitos")),
                        plotly::plotlyOutput("visualizacion_tabMunicipio")                   )
        )
        
        
        
      )
      
    )    
    
    
    
    
    
    
    
  )
  
  
  
)
