
library(here)
library(dplyr)
ruta <- here::here()




################### Unificar datos de la misma JURISDICCIÓN y especialidad #####################
setwd("output")
setwd("1.EficienciasDescongestion")

# Cargar los archivos de R
archivos <- dir()
archivos <- archivos[grepl("\\.rds", archivos)]

for(i in 1:length(archivos)){
    assign(gsub("\\.rds", "", archivos[i]), readRDS(archivos[i]))
}

############################################### Distrito ####################################
datos_Distrito_Penal <- bind_rows(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPenal$dfRes_JuridCompet,
                                  lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadPenal$dfRes_JuridCompet,
                                  lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadPenal$dfRes_JuridCompet)

                                  
datos_Distrito_Civil <- bind_rows(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadCivil$dfRes_JuridCompet,
                                  lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadCivil$dfRes_JuridCompet,
                                  lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadCivil$dfRes_JuridCompet)

  
datos_Distrito_Familia <- bind_rows(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadFamilia$dfRes_JuridCompet,
                                    lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadFamilia$dfRes_JuridCompet,
                                    lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadFamilia$dfRes_JuridCompet)


datos_Distrito_Laboral <- bind_rows(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadLaboral$dfRes_JuridCompet,
                                    lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadLaboral$dfRes_JuridCompet,
                                    lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadLaboral$dfRes_JuridCompet)

  
  
datos_Distrito_Promiscuo <- bind_rows(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet,
                                      lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet,
                                      lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet)



############################################### Circuito ####################################
datos_Circuito_Penal <- bind_rows(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadPenal$dfRes_JuridCompet,
                                  lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadPenal$dfRes_JuridCompet,
                                  lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPenal$dfRes_JuridCompet)


datos_Circuito_Civil <- bind_rows(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadCivil$dfRes_JuridCompet,
                                  lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadCivil$dfRes_JuridCompet,
                                  lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadCivil$dfRes_JuridCompet)


datos_Circuito_Familia <- bind_rows(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadFamilia$dfRes_JuridCompet,
                                    lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadFamilia$dfRes_JuridCompet,
                                    lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadFamilia$dfRes_JuridCompet)


datos_Circuito_Laboral <- bind_rows(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadLaboral$dfRes_JuridCompet,
                                    lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadLaboral$dfRes_JuridCompet,
                                    lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadLaboral$dfRes_JuridCompet)



datos_Circuito_Promiscuo <- bind_rows(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet,
                                      lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet,
                                      lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet)



############################################### Municipio  ####################################
datos_Municipio_Penal <- bind_rows(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadPenal$dfRes_JuridCompet,
                                  lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadPenal$dfRes_JuridCompet,
                                  lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPenal$dfRes_JuridCompet)


datos_Municipio_Civil <- bind_rows(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadCivil$dfRes_JuridCompet,
                                  lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadCivil$dfRes_JuridCompet,
                                  lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadCivil$dfRes_JuridCompet)



datos_Municipio_Laboral <- bind_rows(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadLaboral$dfRes_JuridCompet,
                                    lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadLaboral$dfRes_JuridCompet,
                                    lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadLaboral$dfRes_JuridCompet)


datos_Municipio_Promiscuo <- bind_rows(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet,
                                      lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet,
                                      lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo$dfRes_JuridCompet)


################################ Integración de gráficos ########################################################

############################################### Distrito ####################################
graficos_Distrito_Penal <- c(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)],
                                  lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)],
                                  lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)])

names(graficos_Distrito_Penal) <- c(paste0(names(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2017),
paste0(names(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2018),
paste0(names(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2019))




graficos_Distrito_Civil <- c(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)],
                                  lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)],
                                  lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)])


names(graficos_Distrito_Civil) <- c(paste0(names(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2017),
                                    paste0(names(lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2018),
                                    paste0(names(lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2019))



graficos_Distrito_Familia <- c(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)],
                                    lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)],
                                    lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)])


names(graficos_Distrito_Familia) <- c(paste0(names(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)]), "_", 2017),
                                    paste0(names(lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)]), "_", 2018),
                                    paste0(names(lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)]), "_", 2019))



graficos_Distrito_Laboral <- c(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)],
                                    lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)],
                                    lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)])


names(graficos_Distrito_Laboral) <- c(paste0(names(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2017),
                                      paste0(names(lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2018),
                                      paste0(names(lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2019))



graficos_Distrito_Promiscuo <- c(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo[-1],
                                      lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo[-1],
                                      lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo[-1])


names(graficos_Distrito_Promiscuo) <- c(paste0(names(lista_year_2017_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2017),
                                      paste0(names(lista_year_2018_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2018),
                                      paste0(names(lista_year_2019_Distrito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2019))


############################################### Circuitos ####################################
graficos_Circuito_Penal <- c(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)],
                                  lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)],
                                  lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)])


names(graficos_Circuito_Penal) <- c(paste0(names(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2017),
                                        paste0(names(lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2018),
                                        paste0(names(lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2019))



graficos_Circuito_Civil <- c(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)],
                                  lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)],
                                  lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)])


names(graficos_Circuito_Civil) <- c(paste0(names(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2017),
                                        paste0(names(lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2018),
                                        paste0(names(lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2019))



graficos_Circuito_Familia <- c(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)],
                                    lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)],
                                    lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)])

names(graficos_Circuito_Familia) <- c(paste0(names(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)]), "_", 2017),
                                 paste0(names(lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)]), "_", 2018),
                                 paste0(names(lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadFamilia[-c(1,3)]), "_", 2019))



graficos_Circuito_Laboral <- c(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)],
                            lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)],
                            lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)])

names(graficos_Circuito_Laboral) <- c(paste0(names(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2017),
                                   paste0(names(lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2018),
                                   paste0(names(lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2019))


graficos_Circuito_Promiscuo <- c(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)],
                            lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)],
                            lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)])

names(graficos_Circuito_Promiscuo) <- c(paste0(names(lista_year_2017_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2017),
                                   paste0(names(lista_year_2018_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2018),
                                   paste0(names(lista_year_2019_Circuito_JuridiccionOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2019))



############################################### Municipios ####################################
graficos_Municipio_Penal <- c(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadPenal[-c(1,3)],
                          lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadPenal[-c(1,3)],
                          lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPenal[-c(1,3)])


names(graficos_Municipio_Penal) <- c(paste0(names(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2017),
                                 paste0(names(lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2018),
                                 paste0(names(lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPenal[-c(1,3)]), "_", 2019))



graficos_Municipio_Civil <- c(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadCivil[-c(1,3)],
                          lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadCivil[-c(1,3)],
                          lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadCivil[-c(1,3)])


names(graficos_Municipio_Civil) <- c(paste0(names(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2017),
                                 paste0(names(lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2018),
                                 paste0(names(lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadCivil[-c(1,3)]), "_", 2019))



graficos_Municipio_Laboral <- c(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadLaboral[-c(1,3)],
                            lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadLaboral[-c(1,3)],
                            lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadLaboral[-c(1,3)])

names(graficos_Municipio_Laboral) <- c(paste0(names(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2017),
                                   paste0(names(lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2018),
                                   paste0(names(lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadLaboral[-c(1,3)]), "_", 2019))


graficos_Municipio_Promiscuo <- c(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo[-c(1,3)],
                                lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo[-c(1,3)],
                                lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo[-c(1,3)])

names(graficos_Municipio_Promiscuo) <- c(paste0(names(lista_year_2017_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2017),
                                     paste0(names(lista_year_2018_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2018),
                                     paste0(names(lista_year_2019_Municipio_JuridicciónOrdinaria_EspecialidadPromiscuo[-c(1,3)]), "_", 2019))



# Exportación de datos
setwd(ruta)
setwd("output")
setwd("2.UnificacionSalidas")

# Exportación de gráficos