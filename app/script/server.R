###################
# server.R
# 
# For all your server needs 
###################

server <- function(input, output, session) {
  
  
  
###################  Circuitos ###########################################
 
  
  observe({ updateSelectInput(session,
                              inputId = "selDistrito_tabCircuito",
                              choices = sort(unique(lista_circuitos[[input$especialidad_circuito]]$Distrito[ lista_circuitos[[input$especialidad_circuito]]$Especialidad  %in% 
                                                                                                                            str_to_title(input$especialidad_circuito)]))
  )
  })
   
  observe({ updateSelectInput(session,
                              inputId = "selCircuito_tabCircuito",
                              choices = sort(unique(lista_circuitos[[input$especialidad_circuito]]$Circuito[ lista_circuitos[[input$especialidad_circuito]]$Especialidad  %in% 
                                                                                                                            str_to_title(input$especialidad_circuito) & lista_circuitos[[input$especialidad_circuito]]$Distrito %in% 
                                                                                 input$selDistrito_tabCircuito]))
  )
  })
  
  
  output$tabla_circuitos_especialidad <- DT::renderDT({
    lista_circuitos[[input$especialidad_circuito]] 
  })
  
  
  output$tabla_circuitoEspecialidad_seleccionado <- DT::renderDT({
    lista_circuitos[[input$especialidad_circuito]] %>% dplyr::filter(Distrito == input$selDistrito_tabCircuito &
                                                                    Circuito == input$selCircuito_tabCircuito) %>%
        select(-Distrito, -Circuito, -Especialidad,  -Grupo)
  })
  
  
  graficoInput <- reactive({
    switch(input$Tipovisualizacion_tabCircuito,
           "Número de juzgados de circuitos, egresos y eficiencia" = salida_circuito[[input$especialidad_circuito]]$graficoInteractivo_egresosVSunidades_comp,
           "Comparación eficiencia" = salida_circuito[[input$especialidad_circuito]]$graficoInteractivo_eficiencia,
           "Descongestión e ineficiencia juzgados de circuitos" = salida_circuito[[input$especialidad_circuito]]$graficoInteractivo_IndicadorDescongestion,
           "Descongestión en escenario ideal de eficiencia de los circuitos" = salida_circuito[[input$especialidad_circuito]]$graficoInteractivo_eficienciaIdeal
           )
  })
  
  output$visualizacion_tabCircuito <- plotly::renderPlotly({
    graficoInput()
    
  })
  

  
  
##################### Municipio ########################################################
  
  observe({ updateSelectInput(session,
                              inputId = "selDistrito_tabMunicipio",
                              choices = as.character(sort(unique(lista_municipios[[input$especialidad_municipios]]$Distrito[lista_municipios[[input$especialidad_municipios]]$Especialidad  %in% str_to_title(input$especialidad_municipios)])))
  )
  })
  
  observe({ updateSelectInput(session,
                              inputId = "selCircuito_tabMunicipio",
                              choices = as.character(sort(unique(lista_municipios[[input$especialidad_municipios]]$Circuito[lista_municipios[[input$especialidad_municipios]]$Especialidad  %in% str_to_title(input$especialidad_municipios) &  lista_municipios[[input$especialidad_municipios]]$Distrito %in% 
                                                                                                                   input$selDistrito_tabMunicipio])))
  )
  })
  
  observe({ updateSelectInput(session,
                              inputId = "selMunicipio_tabMunicipio",
                              choices = as.character(sort(unique(lista_municipios[[input$especialidad_municipios]]$municipio[
                                lista_municipios[[input$especialidad_municipios]]$Especialidad  %in% str_to_title(input$especialidad_municipios) & 
                                lista_municipios[[input$especialidad_municipios]]$Distrito %in% input$selDistrito_tabMunicipio & 
                                  lista_municipios[[input$especialidad_municipios]]$Circuito %in%  input$selCircuito_tabMunicipio ])))
  )
  })
  
  
  
  output$tabla_municipios_especialidad <- DT::renderDT({
    lista_municipios[[input$especialidad_municipios]] 
  })
  
  
  output$tabla_municipioEspecialidad_seleccionado <- DT::renderDT({
    lista_municipios[[input$especialidad_municipios]] %>% dplyr::filter(Distrito == input$selDistrito_tabMunicipio &
                                                               Circuito == input$selCircuito_tabMunicipio &
                                                               municipio == input$selMunicipio_tabMunicipio) %>%
      select(-Distrito, -Circuito, municipio, -Especialidad,  -Grupo)
  })
  
  
  graficoInputMun <- reactive({
    switch(input$Tipovisualizacion_tabMunicipio,
           "Número de juzgados de circuitos, egresos y eficiencia" = salida_municipio[[input$especialidad_municipios]]$graficoInteractivo_egresosVSunidades_comp,
           "Comparación eficiencia" = salida_municipio[[input$especialidad_municipios]]$graficoInteractivo_eficiencia,
           "Descongestión e ineficiencia juzgados de circuitos" = salida_municipio[[input$especialidad_municipios]]$graficoInteractivo_IndicadorDescongestion,
           "Descongestión en escenario ideal de eficiencia de los circuitos" = salida_municipio[[input$especialidad_municipios]]$graficoInteractivo_eficienciaIdeal
    )
  })
  
  output$visualizacion_tabMunicipio <- plotly::renderPlotly({
    graficoInputMun()
    
  })
  
    
  
}
