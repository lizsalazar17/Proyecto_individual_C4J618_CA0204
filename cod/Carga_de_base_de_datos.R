#Carga de librerías

library(tidyverse)
library(readxl)

#Veamos la base de datos inicial 
data <- read_xlsx("gatos_petfinder_ny (2).xlsx")

View(data)

head(data)

#Número de filas
  nrow(data)
#Número de columnas
  ncol(data)
  
#Filtro de datos sin contar los gatos cachorros. 
  
  data.filtrada1 <- data %>% 
    filter(data$Categoria_Edad != "Cachorro")
  
  nrow(data.filtrada1)
  
#Implementación de columnas de Genotipos O y D intuidos de los datos de Naranja y Dilucion
  
  