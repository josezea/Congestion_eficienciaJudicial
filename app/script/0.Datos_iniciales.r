library(readxl)

# Disposición de datos originales en R
ruta <- here::here()
setwd("data")
setwd("originales")
df <- read_excel("Consol_Tipo de Proceso_Ene_Dic_2019_Corte_Ene30_2020.xlsx", sheet = "Consolidado")
setwd("data")
saveRDS(df, "Consol_Tipo de Proceso_Ene_Dic_2019_Corte_Ene30_2020.rds")


