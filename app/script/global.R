###################
# global.R
# 
# Anything you want shared between your ui and server, define here.
###################
library(shinydashboard)
library(shiny)
library(dplyr)
library(DT)
library(stringr)

ruta <- "C:/Users/Home/Documents/Laboral 2021/Minjusticia/DEA_V3/app"
setwd(ruta)
setwd("data")

############################  CIRCUITOS  ##################################################
salida_circuito_civil <- readRDS("lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadCivil.rds")
circuito_civil <- salida_circuito_civil$dfRes_JuridCompet 
circuito_civil<- circuito_civil[c("Distrito", "Circuito", 
                                  "Especialidad",   
                                  "Ineficiencia", "Congestión", "Número de juzgados de circuito", 
                                  "Número ideal de juzgados de circuito", "Número de juzgados de circuito adicionales", "Grupo")]

names(circuito_civil) <- c("Distrito", "Circuito", 
                           "Especialidad",   
                           "Ineficiencia", "Congestión", "Núm. juzgados circuito", 
                           "Núm. ideal juzgados circuito", "Núm. juzgados circuito adicionales", "Grupo")
circuito_civil$Ineficiencia <- round(circuito_civil$Ineficiencia, 2)
circuito_civil$Congestión <- round(circuito_civil$Congestión, 2)


salida_circuito_familia <- readRDS("lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadFamilia.rds")
circuito_familia <- salida_circuito_familia$dfRes_JuridCompet 
circuito_familia <- circuito_familia[c("Distrito", "Circuito", 
                                   "Especialidad",   
                                   "Grupo","Ineficiencia", "Congestión", "Número de juzgados de circuito", 
                                   "Número ideal de juzgados de circuito", "Número de juzgados de circuito adicionales")]

circuito_familia$Ineficiencia <- round(circuito_familia$Ineficiencia, 2)
circuito_familia$Congestión <- round(circuito_familia$Congestión, 2)


salida_circuito_laboral <- readRDS("lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadLaboral.rds")
circuito_laboral <- salida_circuito_laboral$dfRes_JuridCompet 
circuito_laboral <- circuito_laboral[c("Distrito", "Circuito", 
                                       "Especialidad",   
                                       "Grupo","Ineficiencia", "Congestión", "Número de juzgados de circuito", 
                                       "Número ideal de juzgados de circuito", "Número de juzgados de circuito adicionales")]

circuito_laboral$Ineficiencia <- round(circuito_laboral$Ineficiencia, 2)
circuito_laboral$Congestión <- round(circuito_laboral$Congestión, 2)



salida_circuito_penal <- readRDS("lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPenal.rds")
circuito_penal <- salida_circuito_penal$dfRes_JuridCompet 
circuito_penal <- circuito_penal[c("Distrito", "Circuito", 
                                   "Especialidad",   
                                   "Grupo","Ineficiencia", "Congestión", "Número de juzgados de circuito", 
                                   "Número ideal de juzgados de circuito", "Número de juzgados de circuito adicionales")]

circuito_penal$Ineficiencia <- round(circuito_penal$Ineficiencia, 2)
circuito_penal$Congestión <- round(circuito_penal$Congestión, 2)


salida_circuito_promiscuo <- readRDS("lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo.rds")
circuito_promiscuo <- salida_circuito_promiscuo$dfRes_JuridCompet 
circuito_promiscuo <- circuito_promiscuo[c("Distrito", "Circuito", 
                                           "Especialidad",   
                                           "Grupo","Ineficiencia", "Congestión", "Número de juzgados de circuito", 
                                           "Número ideal de juzgados de circuito", "Número de juzgados de circuito adicionales")]


circuito_promiscuo$Ineficiencia <- round(circuito_promiscuo$Ineficiencia, 2)
circuito_promiscuo$Congestión <- round(circuito_promiscuo$Congestión, 2)


lista_circuitos <- list(circuito_civil, circuito_familia, circuito_laboral, circuito_penal,circuito_promiscuo)
names(lista_circuitos) <- c("civil", "familia", "laboral", "penal", "promiscuo")
  
# Arreglo de factores en  circuitos
lista_circuitos$civil$Circuito <- as.character(lista_circuitos$civil$Circuito)
lista_circuitos$familia$Circuito <- as.character(lista_circuitos$familia$Circuito)
lista_circuitos$laboral$Circuito <- as.character(lista_circuitos$laboral$Circuito)
lista_circuitos$penal$Circuito <- as.character(lista_circuitos$penal$Circuito)
lista_circuitos$civil$Circuito <- as.character(lista_circuitos$civil$Circuito)
lista_circuitos$promiscuo$Circuito <- as.character(lista_circuitos$promiscuo$Circuito)



# Lista de cada especialidad conlos gráficos
salida_circuito <- list(salida_circuito_civil, salida_circuito_familia, salida_circuito_laboral, salida_circuito_penal,
                              salida_circuito_promiscuo)

names(salida_circuito) <- c("civil", "familia", "laboral", "penal", "promiscuo")

############################  MUNICIPIOS  ##################################################
  salida_municipio_civil <- readRDS("lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadCivil.rds")
  municipio_civil <- salida_municipio_civil$dfRes_JuridCompet 
  municipio_civil <- municipio_civil[c("Distrito", "Circuito", "municipio",
                                    "Especialidad",   
                                    "Ineficiencia", "Congestión", "Número de juzgados municipales", 
                                    "Número ideal de juzgados municipales", 
                                    "Número de juzgados municipales adicionales", "Grupo")]
  
  municipio_civil$Ineficiencia <- round(municipio_civil$Ineficiencia, 2)
  municipio_civil$Congestión <- round(municipio_civil$Congestión, 2)
  
  
  # 
  # salida_circuito_familia <- readRDS("lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadFamilia.rds")
  # circuito_familia <- salida_circuito_familia$dfRes_JuridCompet 
  # circuito_familia <- circuito_familia[c("Distrito", "Circuito", 
  #                                        "Especialidad",   
  #                                        "Grupo","Ineficiencia", "Congestión", "Número de juzgados de circuito", 
  #                                        "Número ideal de juzgados de circuito", "Número de juzgados de circuito adicionales")]
  # 
  # circuito_familia$Ineficiencia <- round(circuito_familia$Ineficiencia, 2)
  # circuito_familia$Congestión <- round(circuito_familia$Congestión, 2)
  
  
  salida_municipio_laboral <- readRDS("lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadLaboral.rds")
  municipio_laboral <- salida_municipio_laboral$dfRes_JuridCompet 
  municipio_laboral <- municipio_laboral[c("Distrito", "Circuito", "municipio",
                                           "Especialidad",   
                                           "Ineficiencia", "Congestión", "Número de juzgados municipales", 
                                           "Número ideal de juzgados municipales", 
                                           "Número de juzgados municipales adicionales", "Grupo")]
  
  municipio_laboral$Ineficiencia <- round(municipio_laboral$Ineficiencia, 2)
  municipio_laboral$Congestión <- round(municipio_laboral$Congestión, 2)
  
  
  
  salida_municipio_penal <- readRDS("lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPenal.rds")
  municipio_penal <- salida_municipio_penal$dfRes_JuridCompet 
  municipio_penal <- municipio_penal[c("Distrito", "Circuito", "municipio",
                                       "Especialidad",   
                                       "Ineficiencia", "Congestión", "Número de juzgados municipales", 
                                       "Número ideal de juzgados municipales", 
                                       "Número de juzgados municipales adicionales", "Grupo")]
  
  municipio_penal$Ineficiencia <- round(municipio_penal$Ineficiencia, 2)
  municipio_penal$Congestión <- round(municipio_penal$Congestión, 2)
  
  
  salida_municipio_promiscuo <- readRDS("lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo.rds")
  municipio_promiscuo <- salida_municipio_promiscuo$dfRes_JuridCompet 
  municipio_promiscuo <- municipio_promiscuo[c("Distrito", "Circuito", "municipio",
                                               "Especialidad",   
                                               "Ineficiencia", "Congestión", "Número de juzgados municipales", 
                                               "Número ideal de juzgados municipales", 
                                               "Número de juzgados municipales adicionales", "Grupo")]
  
  
  municipio_promiscuo$Ineficiencia <- round(municipio_promiscuo$Ineficiencia, 2)
  municipio_promiscuo$Congestión <- round(municipio_promiscuo$Congestión, 2)
  
  
  lista_municipios <- list(municipio_civil, municipio_laboral, municipio_penal, 
                           municipio_promiscuo)
  names(lista_municipios) <- c("civil", "laboral", "penal", "promiscuo")
  
  
  
  
  # Arreglo de factores en  circuitos
  lista_municipios$civil$municipio <- as.character(lista_municipios$civil$municipio)
  lista_municipios$laboral$municipio <- as.character(lista_municipios$laboral$municipio)
  lista_municipios$penal$municipio <- as.character(lista_municipios$penal$municipio)
  lista_municipios$civil$municipio <- as.character(lista_municipios$civil$municipio)
  lista_municipios$promiscuo$municipio <- as.character(lista_municipios$promiscuo$municipio)
  

  
  salida_municipio <- list(salida_municipio_civil,  salida_municipio_laboral, salida_municipio_penal,
                          salida_municipio_promiscuo)
  
  names(salida_municipio) <- c("civil", "laboral", "penal", "promiscuo")  
  
# Importante no borrar  
setwd(ruta)
