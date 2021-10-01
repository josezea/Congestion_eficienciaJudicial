##################### TIPO DE DESPACHO: Juzgado de Municipio ####################

library(dplyr) # Procesar datos
library(readxl) # Archivos de excel
library(reticulate) # Correr código en python
library(plyr, include.only = "mapvalues") # Recodificar variables
library(Benchmarking) # DEA: análisis envolvente de datos
library(ggplot2) # Gráficos
library(ggrepel) # Repelar los textos en gráficos
library(plotly)
library(ggrepel)
library(forcats)


#conda_list()

# name                                                                       python
# 1 anaconda3 C:\\Users\\Home\\anaconda3\\python.exe
# 2  r-miniconda                     C:\\Users\\Home\\AppData\\Local\\r-miniconda\\python.exe
# 3 r-reticulate C:\\Users\\Home\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe

#ruta <- "C:/Users/Home/Documents/Laboral 2021/Minjusticia/DEA_V3/data/"
ruta <- here::here()
setwd(ruta)

###################### Lectura datos ###########################################
setwd("data")
# 2017 - 2019
BD_procesos_Rama_Judicial_2017_2018 <- readRDS("BD_procesos_Rama_Judicial_2017_2018.rds")
# 2019
df_2019 <- readRDS("Consol_Tipo de Proceso_Ene_Dic_2019_Corte_Ene30_2020.rds")

############################### I. JURIDICCIÓN ORDINARIA  #################################

##### TIPO DE DESPACHO: Tribunal Superior #####
# df <- df_2019
# Anno <- "2019"
# vctr_especialidad = "Familia"

f_analisis <- function(df, Anno, vctr_especialidad){
   use_condaenv("anaconda3", required = TRUE)
  py_discover_config()
  
  kmeans_c <- import("k_means_constrained")
  np <- import("numpy")
  
  
  df_JuridiccCompetenEspecial <- df %>% dplyr::filter(JURISDICCIÓN == "Ordinaria" & 
                                                        `TIPO DE DESPACHO` == "Juzgado Municipal", 
                                                      ESPECIALIDAD == vctr_especialidad) %>%
    group_by(JURISDICCIÓN, 
             `TIPO DE DESPACHO`, DISTRITO, CIRCUITO, MUNICIPIO, ESPECIALIDAD) %>% 
    summarise(NUM_JUECES = n_distinct(`Cédula del Funcionario despacho`),
              NUM_UNIDADES = n_distinct(CÓDIGO),
              INGRESOS_EFEC = sum(`INGRESOS EFECTIVOS - RAMA JUDICIAL`),
              EGRESOS_EFEC = sum(`EGRESOS EFECTIVOS - DESPACHO`),
              TOT_INV_INIC = sum(`TOTAL INVENTARIO INICIAL`),
              TOT_INV_FINAL = sum(`TOTAL INVENTARIO FINAL`)) %>% ungroup() %>%
    mutate(Cargas = INGRESOS_EFEC +  TOT_INV_INIC, EgresosSobreCarga = EGRESOS_EFEC/ Cargas)
  
  df_JuridiccCompetenEspecial$EgresosSobreCarga <- ifelse(df_JuridiccCompetenEspecial$EgresosSobreCarga >= 1, 1, 
                                                          df_JuridiccCompetenEspecial$EgresosSobreCarga)
  
  
  ############################## Rea = r_to_py(as.matrix(df_JuridCompet[c("Cargas", "EGRESOS_EFEC")]))
  
  # Selecciono un conjunto de datos reducido (cargas y egresos efectivos)
  dfRed_JuridiccCompetenEspecial <- df_JuridiccCompetenEspecial %>% select(Cargas, EGRESOS_EFEC)
  
  # Convierto la tabla e un arreglo de python para poder realizar un agrupamiento de las Unidades con resticciones en el tamaño de los grupos
  pyarr_clasifica <- r_to_py(as.matrix(dfRed_JuridiccCompetenEspecial)) # Función de Python
  
  # Parámetros para la clasficiación
  # Cálculo una cantidad plausible de grupos para conformar agrupaciones homogeneas de las unidades
  n_cluster <- as.integer(nclass.scott(dfRed_JuridiccCompetenEspecial$Cargas))
  
  
  # para determinar el número máximo de unidades que puede haber en un cluster, 
  # supongamos que todos los cluster (a excepción de uno de ellos) tiene unicamente tres unidades
  # N_unidades(25) - (MinTam(3) * (ncluster(5) - 1))
  
  clf = kmeans_c$KMeansConstrained(n_clusters=n_cluster, size_min=3L, 
                                   size_max= as.integer(nrow(df_JuridiccCompetenEspecial) - (n_cluster - 1) * 3), 
                                   random_state=0L)
  clasifica <- as.numeric(clf$fit_predict(pyarr_clasifica))
  
  # Ordenar las clases de acuerdo al promedio de la carga
  
  df_JuridiccCompetenEspecial$clasifica <- mapvalues(clasifica, from = order(tapply(df_JuridiccCompetenEspecial$Cargas, clasifica, mean), decreasing = T)-1, 
                                                     seq_along(order(tapply(df_JuridiccCompetenEspecial$Cargas, clasifica, mean), decreasing = T)))
  
  rm(pyarr_clasifica); rm(clf); rm(clasifica)
  
  # Análisis envolvente de datos (DEA): encontrar medidas de eficiencia de las unidades
  
  inputs <- as.matrix(df_JuridiccCompetenEspecial[["NUM_UNIDADES"]]) 
  outputs <- as.matrix(df_JuridiccCompetenEspecial[["EGRESOS_EFEC"]])
  
  
  # Calcular en cada estrato las eficiencias
  
  f_eficiencia <- function(df = df_JuridCompet, str_var_input = "NUM_UNIDADES", str_var_output = "EGRESOS_EFEC"){
    inputs <- as.matrix(df[[str_var_input]]) 
    outputs <- as.matrix(df[[str_var_output]])
    dea <- dea(inputs, outputs, RTS ="crs",ORIENTATION = "out",SLACK = TRUE, DUAL = TRUE)
    1 / dea$eff
  }
  
  # f_eficiencia(df_JuridCompet, str_var_input = "NUM_JUECES", str_var_output = "EGRESOS_EFEC")
  # f_eficiencia(df_JuridCompet)
  
  
  # Recorrer en cada grupo 
  df_JuridCompet <- df_JuridiccCompetenEspecial %>% arrange(clasifica, DISTRITO )
  vctr_clasifica <- unique(df_JuridCompet$clasifica)
  ls_JuridCompet <- vector(mode = "list", length = length(vctr_clasifica))
  
  for(i in seq_along(vctr_clasifica)){
    df_red <- df_JuridCompet %>% filter(clasifica == vctr_clasifica[i])
    df_red$eficiencia <- f_eficiencia(df_red)
    ls_JuridCompet[[i]] <- df_red
  }
  
  
  dfRes_JuridCompet <- bind_rows(ls_JuridCompet)
  
  
  dfRes_JuridCompet$clasifica <- factor(dfRes_JuridCompet$clasifica)
  dfRes_JuridCompet$ineficiencia <- 1 - dfRes_JuridCompet$eficiencia
  dfRes_JuridCompet$congestion <- 1 - dfRes_JuridCompet$EgresosSobreCarga
  

  
  dfRes_JuridCompet$EGRESOS_EFEC_IDEALES <- round(dfRes_JuridCompet$EGRESOS_EFEC * (1 / dfRes_JuridCompet$eficiencia))
  dfRes_JuridCompet$EgresosIdealSobreCarga <- round(dfRes_JuridCompet$EGRESOS_EFEC_IDEALES / dfRes_JuridCompet$Cargas,2)
  dfRes_JuridCompet$EgresosIdealSobreCarga <- ifelse(dfRes_JuridCompet$EgresosIdealSobreCarga >= 1, 1, dfRes_JuridCompet$EgresosIdealSobreCarga)
  dfRes_JuridCompet$Congestion_escenarioIdeal <- 1 -  dfRes_JuridCompet$EgresosIdealSobreCarga
  
  
  # Sacar el personal necesario
  dfRes_JuridCompet$NUMUNIDADES_IDEAL <- round(dfRes_JuridCompet$NUM_UNIDADES / dfRes_JuridCompet$EgresosIdealSobreCarga)
  dfRes_JuridCompet$NUMUNIDADES_ADICIONALES <-   dfRes_JuridCompet$NUMUNIDADES_IDEAL  - dfRes_JuridCompet$NUM_UNIDADES 
  
  # Interactivo
  
  
  
  dfRes_JuridCompet$DISTCIRCMUNEFIC <- paste0("D: ", dfRes_JuridCompet$DISTRITO, 
                                              " - C: ", dfRes_JuridCompet$CIRCUITO, 
                                              " - M: ",  dfRes_JuridCompet$MUNICIPIO, ", ",
                                              round(dfRes_JuridCompet$eficiencia, 2))
  
  #dfRes_JuridCompet$CIRCUITOMUNIC_EFICIENCIA <- paste0(dfRes_JuridCompet$DISTCIRCMUNEFIC, " (Eficiencia = ", round(dfRes_JuridCompet$eficiencia, 2), ")")
  graficoInteractivo_egresosVSunidades_comp <- plot_ly(data = dfRes_JuridCompet, x = ~NUM_UNIDADES, 
                                                       y = ~EGRESOS_EFEC, color = ~factor(clasifica), size = ~eficiencia,
                                                       text = ~DISTCIRCMUNEFIC) %>% layout(tickformat = "digit" , separators= ".",
                                                                                               title = paste0("Análisis juzgados municipales (Juridicción: ordinaria - Especialidad: ", 
                                                                                                              tolower(vctr_especialidad), ")"),
                                                                                               xaxis = list(title = "Número de unidades", titlefont = list(family = "Courier New, monospace", 
                                                                                                                                                           size = 18, color = "#7f7f7f"), tickformat = "digits"),
                                                                                               yaxis = list(title = "Egresos efectivos", titlefont = list(family = "Courier New, monospace",  size = 18, color = "#7f7f7f")),
                                                                                               legend = list(title = list(text = "Grupo")))

  
 
   dfResIncom_JuridCompet <-  dfRes_JuridCompet %>% filter(clasifica != 1)
  # dfResIncom_JuridCompet$MUNICIPIO_EFICIENCIA <- paste0(dfResIncom_JuridCompet$DISTCIRCMUN, " (Eficiencia = ", 
  #                                                      round(dfResIncom_JuridCompet$eficiencia, 2), ")")
  # 
  graficoInteractivo_egresosVSunidades_incomp <- plot_ly(data = dfResIncom_JuridCompet, x = ~NUM_UNIDADES, 
                                                         y = ~EGRESOS_EFEC, color = ~factor(clasifica), size = ~eficiencia,
                                                         text = ~DISTCIRCMUNEFIC) %>% layout(tickformat = "digit" , separators= ".",
                                                                                                 title = paste0("Análisis juzgados municipales (Juridicción: ordinaria - Especialidad: ", 
                                                                                                                tolower(vctr_especialidad), ")", "(Sin grupo 1)"),
                                                                                                 xaxis = list(title = "Número de unidades", titlefont = list(family = "Courier New, monospace", 
                                                                                                                                                             size = 18, color = "#7f7f7f"), tickformat = "digits"),
                                                                                                 yaxis = list(title = "Egresos efectivos", titlefont = list(family = "Courier New, monospace",  size = 18, color = "#7f7f7f")),
                                                                                                 legend = list(title = list(text = "Grupo")))
  
  
  
  
  dfRes_JuridCompet <- dfRes_JuridCompet %>% arrange(desc(clasifica), desc(eficiencia))
  
  dfRes_JuridCompet$MUNICIPIO <- fct_rev(forcats::fct_inorder(dfRes_JuridCompet$MUNICIPIO)) 
  # dfRes_JuridCompet$MUNICIPIO_EFICIENCIA <- paste0(dfRes_JuridCompet$DISTCIRCMUN, " (Eficiencia = ", 
  #                                                 round(dfRes_JuridCompet$eficiencia, 2), ")")
  
  graficoInteractivo_eficiencia <- plot_ly(data = dfRes_JuridCompet, x = ~MUNICIPIO, 
                                           y = ~eficiencia,
                                           color = ~factor(clasifica), 
                                           text = ~DISTCIRCMUNEFIC,
                                           type = 'bar') %>% 
    layout(tickformat = "digit" , separators= ".", 
           title = paste0("Análisis juzgados municipales (Juridicción: ordinaria - Especialidad: ", 
                          tolower(vctr_especialidad), ")"),
           xaxis = list(title = "Municipios", titlefont = list(family = "Courier New, monospace", 
                                                              size = 18, color = "#7f7f7f"), tickformat = "digits"),
           yaxis = list(title = "Eficiencia judicial", titlefont = list(family = "Courier New, monospace",  
                                                                        size = 18, color = "#7f7f7f")),
           legend = list(title = list(text = "Grupo")))

  
  
  dfRes_JuridCompet$DescongEfic <- paste0("(", dfRes_JuridCompet$DISTCIRCMUNEFIC, 
                                          ", Descongestión = ",
                                          round(dfRes_JuridCompet$EgresosSobreCarga, 2),
                                          ", Eficiencia = ", 
                                          round(dfRes_JuridCompet$eficiencia, 2), ")")
  
  graficoInteractivo_DescongEfic <- plot_ly(data = dfRes_JuridCompet, x = ~EgresosSobreCarga, 
                                            y = ~eficiencia,
                                            color = ~factor(clasifica), 
                                            text = ~DescongEfic) %>% 
    layout(tickformat = "digit" , separators= ".", 
           title = paste0("Análisis juzgados municipales (Juridicción: ordinaria - Especialidad: ", 
                          tolower(vctr_especialidad), ")"),
           xaxis = list(title = "Descongestión", titlefont = list(family = "Courier New, monospace", 
                                                                  size = 18, color = "#7f7f7f"), tickformat = "digits"),
           yaxis = list(title = "Eficiencia judicial", titlefont = list(family = "Courier New, monospace",  
                                                                        size = 18, color = "#7f7f7f")),
           legend = list(title = list(text = "Grupo")))
  dfRes_JuridCompet$DescongEfic <- NULL
  
  

  dfRes_JuridCompet <- dfRes_JuridCompet %>% arrange(desc(clasifica), desc(EgresosIdealSobreCarga))
  #dfRes_JuridCompet$DescongEfic <- fct_rev(forcats::fct_inorder(dfRes_JuridCompet$DescongEfic)) 

  dfRes_JuridCompet$MunicEficideal <- paste0("D: ", dfRes_JuridCompet$DISTRITO, 
                                             " - C: ", dfRes_JuridCompet$CIRCUITO, 
                                             " - M: ",  dfRes_JuridCompet$MUNICIPIO, ", ", 
                                          ", Congestión (escenario ideal) = ",
                                          round(dfRes_JuridCompet$EgresosIdealSobreCarga, 2))
  
  graficoInteractivo_eficienciaIdeal <- plot_ly(data = dfRes_JuridCompet, x = ~MUNICIPIO, 
                                                y = ~EgresosIdealSobreCarga,
                                                color = ~factor(clasifica), 
                                                text = ~MunicEficideal,
                                                type = 'bar') %>% 
    layout(tickformat = "digit" , separators= ".", 
           title = paste0("Análisis juzgados municipales (Juridicción: ordinaria - Especialidad: ", 
                          tolower(vctr_especialidad), ")"),
           xaxis = list(title = "Municipios", titlefont = list(family = "Courier New, monospace", 
                                                              size = 18, color = "#7f7f7f"), tickformat = "digits"),
           yaxis = list(title = "Descongestión escenario ideal", titlefont = list(family = "Courier New, monospace",  
                                                                                  size = 18, color = "#7f7f7f")),
           legend = list(title = list(text = "Grupo")))
  dfRes_JuridCompet$MunicEficideal <- NULL
  
  
  # Arreglar la tabla a publicar de datos
  dfRes_JuridCompet$CIRCUITOMUNIC_EFICIENCIA <- NULL
  dfRes_JuridCompet$DISTCIRCMUNEFIC <- NULL
  dfRes_JuridCompet$EGRESOS_EFEC_IDEALES <- NULL
  dfRes_JuridCompet$EgresosIdealSobreCarga <- NULL
  
  
  names(dfRes_JuridCompet) <- c("Juridicción", "Tipo de despacho", "Distrito", "Circuito", "municipio", "Especialidad",
                                "Número de jueces", "Número de juzgados municipales", "Ingresos efectivos", "Egresos Efectivos", "Total de inventario inicial", "Total de inventario final", 
                                "Cargas", "Descongestión", "Grupo", "Eficiencia", "Ineficiencia", "Congestión", "Congestión escenario ideal", 
                                "Número ideal de juzgados municipales", "Número de juzgados municipales adicionales")
  
  # Resumen  
  
  
  
  # Exportar tabla y visualizar Mapa del prototipo
  setwd(ruta)
  setwd("output/1.EficienciasDescongestion")
  
  
  lista <- list("dfRes_JuridCompet" = dfRes_JuridCompet, 
                "graficoInteractivo_egresosVSunidades_comp" = graficoInteractivo_egresosVSunidades_comp,
                "graficoInteractivo_egresosVSunidades_incomp" = graficoInteractivo_egresosVSunidades_incomp, 
                "graficoInteractivo_eficiencia" = graficoInteractivo_eficiencia,  
                "graficoInteractivo_IndicadorDescongestion" = graficoInteractivo_DescongEfic,
                "graficoInteractivo_eficienciaIdeal" = graficoInteractivo_eficienciaIdeal)
  
  carpeta <- paste0("year_", Anno, "_Municipio_","Juridicción", unique(dfRes_JuridCompet$Juridicción), "_", "Especialidad", 
                    unique(dfRes_JuridCompet$Especialidad))
  
  # if(!exists(carpeta)) {
  #   dir.create(carpeta)
  #   setwd(carpeta)
  # } else {
  #   setwd(carpeta)  
  # } 
  
  saveRDS(lista, paste0("lista_", carpeta, ".rds"))
  writexl::write_xlsx(lista$dfRes_JuridCompet, paste0("lista_", carpeta, ".xlsx"))
  
  return(lista)
}

analisis_penal_2017 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2017", "Penal")
analisis_civil_2017 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2017", "Civil")
#analisis_familia_2017 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2017", "Familia")
analisis_laboral_2017 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2017", "Laboral")
analisis_promiscuo_2017 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2017", "Promiscuo")

analisis_penal_2018 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2018", "Penal")
analisis_civil_2018 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2018", "Civil")
#analisis_familia_2018 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2018", "Familia")
analisis_laboral_2018 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2018", "Laboral")
analisis_promiscuo_2018 <- f_analisis(BD_procesos_Rama_Judicial_2017_2018, "2018", "Promiscuo")


analisis_penal_2019 <- f_analisis(df_2019, "2019", "Penal")
analisis_civil_2019 <- f_analisis(df_2019, "2019", "Civil")
#analisis_familia_2019 <- f_analisis(df_2019, "2019", "Familia")
analisis_laboral_2019 <- f_analisis(df_2019, "2019", "Laboral")
analisis_promiscuo_2019 <- f_analisis(df_2019, "2019", "Promiscuo")



analisis_penal_2019 <- f_analisis(df_2019, "2019", "Penal")
analisis_civil_2019 <- f_analisis(df_2019, "2019", "Civil")
#analisis_familia_2019 <- f_analisis(df_2019, "2019", "Familia")
analisis_laboral_2019 <- f_analisis(df_2019, "2019", "Laboral")
analisis_promiscuo_2019 <- f_analisis(df_2019, "2019", "Promiscuo")


