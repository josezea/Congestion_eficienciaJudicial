library(stringdist)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(stringi)


setwd("C:/Users/Home/Documents/Laboral 2021/Minjusticia/Mapa Judicial Colombiano/salidas")
dir()
mapajudicial <- readRDS("MAPAJUDICIAL.rds")


ruta <- "C:/Users/Home/Documents/Laboral 2021/Minjusticia/DEA_V3/data/"
setwd(ruta)

vctr_municipio <- c("CONSEJO SECCIONAL", "DISTRITO", "CIRCUITO", "MUNICIPIO")
df_2019 <- readRDS("Consol_Tipo de Proceso_Ene_Dic_2019_Corte_Ene30_2020.rds")
df_2019 <- filter(df_2019, JURISDICCIÓN  == "Ordinaria")

#table(df_2019$`TIPO DE DESPACHO`)

########################Juzgados de circuitos ################################

# Se identifican los municipios de influencia de los juzgados de circuito para el año 2019
df_2019_juzgadoCircuito <- filter(df_2019, `TIPO DE DESPACHO` %in% 
                                    c("Juzgado de Circuito"))

# Cruzar con el mapa jucicial colombiano (consultal para posteriormente ajustar la tabla df_2019 para maximizar los cruces)
df_2019_DistritoCircuito <- unique(as.data.frame(df_2019_juzgadoCircuito)[c("DISTRITO", "CIRCUITO")])

# Eliminar cundinamarca y antioquia de los circuitos
df_2019_DistritoCircuito <- filter(df_2019_DistritoCircuito, !(CIRCUITO %in% c("Antioquia",  "Cundinamarca")), )


################### Evaluar la homologación de circuitos y distritos con el mapa judicial ################
prueba_cruceMapaJucicial2019 <- stringdist_left_join(df_2019_DistritoCircuito, mapajudicial,
                          by  = c("DISTRITO" = "nom_distrito", 
                                  "CIRCUITO" = "nom_circuito"),
                          ignore_case = T, method = "jw", max_dist = 0.15)

df_2019_DistritoCircuito <- filter(df_2019_DistritoCircuito, 
                                   !(CIRCUITO %in% c("Antioquia",  "Cundinamarca")), )

# Nombres a cambiar para maximizar cruces
df_nombres_cambiar <- prueba_cruceMapaJucicial2019[prueba_cruceMapaJucicial2019$`Código Departamento`  %>% is.na() &
                               !is.na(prueba_cruceMapaJucicial2019$DISTRITO) ,][,1:2]

# Reescribir los nombres del mapa 
# <chr>         <chr>              
# 2 Bogotá        Bogotá             
# 3 Cartagena     Mompós             
# 4 Cundinamarca  Cundinamarca       
# 5 Villavicencio Puerto López       
# 6 Pasto         La Cruz            
# 7 Cúcuta        Cúcuta             
# 8 Cúcuta        Los Patios         
# 9 Cúcuta        Ocaña              
# 10 Pereira       Apía               
# 11 Pereira       Santa Rosa de Cabal
# 12 Bucaramanga   Málaga             
# 13 Sincelejo     Majagual           
# 14 San Andrés    San Andrés         
# 15 Villavicencio Puerto Inírida     
# 16 Villavicencio Mitú               
# 17 Villavicencio Puerto Carreño  

df_nombres_cambiar$DISTRITO_CORREGIDO <- c("BOGOTA", "CARTAGENA",  "VILLAVICENCIO", 
                                           "PASTO", "CUCUTA", "CUCUTA", "CUCUTA", "PEREIRA", 
                                           "PEREIRA", 
                                           "BUCARAMANGA", "SINCELEJO", "ARCH SAN ANDRES",
                                           "VILLAVICENCIO", "VILLAVICENCIO", 
                                           "VILLAVICENCIO")

df_nombres_cambiar$CIRCUITO_CORREGIDO  <- c("BOGOTA D.C", "MOMPOX", "PTO LOPEZ", "DE LA CRUZ", "CUCUTA", "LOS PATIOS",
"OCAÑA", "APIA", "STA ROSA DE CABAL", "MALAGA", "SUCRE", "SAN ANDRES", "INIRIDA",
"MITU", "PTO CARREÑO")

df_2019_DistritoCircuito$temp <- 1

df_2019_DistritoCircuito2 <- left_join(df_2019_DistritoCircuito, df_nombres_cambiar) 
                  by = c("DISTRITO", "CIRCUITO")

df_2019_DistritoCircuito2$temp <- NULL                    

df_2019_DistritoCircuito2$DISTRITO_ORIG <- df_2019_DistritoCircuito2$DISTRITO 

df_2019_DistritoCircuito2$DISTRITO <- ifelse(!is.na(df_2019_DistritoCircuito2$DISTRITO_CORREGIDO), 
df_2019_DistritoCircuito2$DISTRITO_CORREGIDO, df_2019_DistritoCircuito2$DISTRITO)


df_2019_DistritoCircuito2$CIRCUITO_ORIG <- df_2019_DistritoCircuito2$CIRCUITO 

df_2019_DistritoCircuito2$CIRCUITO <- ifelse(!is.na(df_2019_DistritoCircuito2$CIRCUITO_CORREGIDO), 
                                             df_2019_DistritoCircuito2$CIRCUITO_CORREGIDO, df_2019_DistritoCircuito2$CIRCUITO)
df_2019_DistritoCircuito2$DISTRITO_CORREGIDO <- NULL
df_2019_DistritoCircuito2$CIRCUITO_CORREGIDO <- NULL

############# Volver a hacer la integración con el mapa judicial ######################

prueba2_cruceMapaJucicial2019 <- stringdist_left_join(df_2019_DistritoCircuito2, mapajudicial,
                                                     by  = c("DISTRITO" = "nom_distrito", 
                                                             "CIRCUITO" = "nom_circuito"),
                                                     ignore_case = T, method = "jw", max_dist = 0.15)

df_homologadoDistritoCircuito <- prueba2_cruceMapaJucicial2019[-926,]


# Ya se tienen todos los distritos con sus municipios de influencia

# Eliminar errores del record linkage
table(duplicated(df_homologadoDistritoCircuito$`Código Municipio`))

consulta_dup1 <- df_homologadoDistritoCircuito[duplicated(df_homologadoDistritoCircuito$`Código Municipio`) |
                                                 duplicated(df_homologadoDistritoCircuito$`Código Municipio`, 
                                                            fromLast = T),]

df_homologadoDistritoCircuito <-  df_homologadoDistritoCircuito %>% filter(!(DISTRITO == "Sincelejo" & 
                                                                        CIRCUITO == "Sincelejo" & nom_mpio == "GALERAS"))

df_homologadoDistritoCircuito <-  df_homologadoDistritoCircuito %>% filter(!(DISTRITO == "Sincelejo" & 
                                                                               CIRCUITO == "Sincelejo" & 
                                                                               nom_mpio == "SAN BENITO ABAD"))

table(duplicated(df_homologadoDistritoCircuito$`Código Municipio`))

df_homologadoDistritoCircuito <- df_homologadoDistritoCircuito[c("DISTRITO", "CIRCUITO", "DISTRITO_ORIG", "CIRCUITO_ORIG",
  "nom_mpio", "Código Municipio")]


# Agregar tres casos que no aparecen en el mapa judicial
# DISTRITO CIRCUITO nom_mpio   ´Código Municipio´
# Cartagena Simití norosi 13490
# Popayán Popayán sotara 19760
# Montería Montelíbano san jose de ure 23682
# Montería Chinú tuchin 23815

# df_2019_DistritoCircuito

df_homologadoDistritoCircuito <- rbind(df_homologadoDistritoCircuito, 
                                       data.frame(DISTRITO = c("Cartagena", "Popayán", "Montería", "Montería"), 
           CIRCUITO = c("Simití", "Popayán", "Montelibano", "Chinú"),
           DISTRITO_ORIG = c("Cartagena", "Popayán", "Montería", "Montería"),
           CIRCUITO_ORIG = c("Simití", "Popayán", "Montelíbano", "Chinú"),
           nom_mpio = c("norosi", "sotara", "san jose de ure", "tuchin"),
           "Código Municipio" = c("13490", "19760", "23682", "23815"), 
           check.names = F))

setwd("C:/Users/Home/Documents/Laboral 2021/Minjusticia/DEA_V3/output/homologacion_mapaJudicial")
saveRDS(df_homologadoDistritoCircuito, "df_homologadoDistritoCircuito.rds")




###############################  Homologación de municipios #######################

# Estrategia: homologarla directamente con la divipola
ruta <- "C:/Users/Home/Documents/Laboral 2021/Minjusticia/DEA_V3/data/"
setwd(ruta)

vctr_municipio <- c("CONSEJO SECCIONAL", "DISTRITO", "CIRCUITO", "MUNICIPIO")
df_2019 <- readRDS("Consol_Tipo de Proceso_Ene_Dic_2019_Corte_Ene30_2020.rds")
df_2019 <- filter(df_2019, JURISDICCIÓN  == "Ordinaria")

#table(df_2019$`TIPO DE DESPACHO`)

########################Juzgados de circuitos ################################

# Se identifican los municipios de influencia de los juzgados de circuito para el año 2019
df_2019_juzgadoCircuito <- filter(df_2019, `TIPO DE DESPACHO` %in% 
                                    c("Juzgado Municipal"))


dfDespMunicip_distCircMpio2019 <- unique(as.data.frame(df_2019_juzgadoCircuito)[c("DISTRITO", 
                                                                                  "CIRCUITO", "MUNICIPIO")])


# Archivo de mapa judicial adaptado a los municipios que aparecen
# df_homologadoDistritoCircuito
df_homologadoDistritoCircuito$MUNICIPIO <- df_homologadoDistritoCircuito$nom_mpio
df_homologadoDistritoCircuito$MUNICIPIO_ORIGIN <- df_homologadoDistritoCircuito$MUNICIPIO

df_homologadoDistritoCircuito$MUNICIPIO <- tolower(stri_trans_general(df_homologadoDistritoCircuito$MUNICIPIO,"Latin-ASCII"))

dfDespMunicip_distCircMpio2019$MUNICIPIO_ORIG <- dfDespMunicip_distCircMpio2019$MUNICIPIO
dfDespMunicip_distCircMpio2019$MUNICIPIO <- tolower(stri_trans_general(dfDespMunicip_distCircMpio2019$MUNICIPIO,"Latin-ASCII"))

# Identificado código de municipios (los que no se logren idenificar si hacen en la siuiente etapa)
df_homologadoA <- stringdist_right_join(df_homologadoDistritoCircuito, dfDespMunicip_distCircMpio2019,
                                ignore_case = T, method = "jw", p = 0,
                                max_dist = 0.10, by =  c("DISTRITO", "CIRCUITO", "MUNICIPIO")) 


df_homologadoA$temp <- paste0(df_homologadoA$DISTRITO.y, "_", df_homologadoA$CIRCUITO.y, "_", df_homologadoA$MUNICIPIO.y)
table(duplicated(df_homologadoA$temp))
consulta_dup <- df_homologadoA[duplicated(df_homologadoA$temp) | duplicated(df_homologadoA$temp, fromLast = T),] 


df_homologadoA <- df_homologadoA %>% filter(temp != "Quibdó_Riosucio_riosucio" | `Código Municipio` !=  "27086")
df_homologadoA <- df_homologadoA %>%  filter(temp != "Pamplona_Pamplona_pamplona" | `Código Municipio` !=  "54520")
df_homologadoA <- df_homologadoA %>%  filter(temp != "Pamplona_Pamplona_pamplonita" | `Código Municipio` !=  "54518")

df_homologadoA$temp <- NULL

# Arreglar manualmente con editData la tabla

df_revisarA <- filter(df_homologadoA, is.na(MUNICIPIO.x)) %>% 
                select(`Código Municipio`, MUNICIPIO.x, DISTRITO.y,
                       CIRCUITO.y, MUNICIPIO.y, DISTRITO_ORIG , CIRCUITO_ORIG, MUNICIPIO_ORIG  )




setwd("C:/Users/Home/Documents/Laboral 2021/Minjusticia/DEA_V3/output/homologacion_mapaJudicial")
#saveRDS(df_MpiosArregManualmente, "df_MpiosArregManualmente.rds")
df_MpiosArregManualmente <- readRDS("df_MpiosArregManualmente.rds")
df_MpiosArregManualmente <- df_MpiosArregManualmente[-c(16,26,32,33),]
#df_MpiosArregManualmente$MUNICIPIO_ORIG <- df_revisarA$MUNICIPIO_ORIG  


df_homologadoMunicipioA <-  df_homologadoA %>%
  filter(!is.na(`Código Municipio`)) %>%
    select(DISTRITO = DISTRITO_ORIG,
         CIRCUITO = CIRCUITO_ORIG,
         MUNICIPIO = MUNICIPIO_ORIG,
         `Código Municipio`)


df_homologadoMunicipioB <-  df_MpiosArregManualmente %>%
                                             select(DISTRITO = DISTRITO.y,
                                                   CIRCUITO = CIRCUITO.y,
                                                   MUNICIPIO =  MUNICIPIO_ORIG,
                                                   `Código Municipio`)


df_homologadoMunicipio <- bind_rows(df_homologadoMunicipioA, df_homologadoMunicipioB)
saveRDS(df_homologadoMunicipio, "df_homologadoDistritoCircuitoMunicipio.rds")




table(df_homologadoMunicipio$`Código Municipio` %in% 
df_homologadoDistritoCircuito$`Código Municipio`)

#df_homologadoMunicipio[!df_homologadoMunicipio$`Código Municipio` %in% 
#  df_homologadoDistritoCircuito$`Código Municipio`,]

table(df_homologadoDistritoCircuito$`Código Municipio` %in% 
        df_homologadoMunicipio$`Código Municipio`)


df_homologadoDistritoCircuito[!df_homologadoDistritoCircuito$`Código Municipio` %in% 
  df_homologadoMunicipio$`Código Municipio`,]
