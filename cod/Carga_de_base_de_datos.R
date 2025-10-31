#Carga de librerías

library(tidyverse)
library(readxl)

#Veamos la base de datos inicial 
data <- read_xlsx("data/gatos_petfinder_ny (2).xlsx")

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
  
  df.filtrada2 <- data.filtrada1 %>% 
    select(ID, Sexo, Naranja, Dilucion)
  
  #Para el genotipo O ocupamos ver si el gato porta Naranja y si es Macho o Hembra
  
  mapear.alelo.o <- function(naranja, sexo){
    ifelse(naranja == "Sí", 
        ifelse(sexo == "Macho", "x^Oy", "x^Ox^O"),
        ifelse(sexo == "Macho", "x^oy", "x^ox^o")
        )
  }
  #Para el genotipo D vamos analizar la dilución de cada gato
  
  mapear.alelo.d <- function(dilucion){
    ifelse(dilucion == "No", "D_", "dd")
  }
  
  #Ahora creamos el dataframe final con las columnas de genotipo incluido
  df.final <- df.filtrada2 %>% 
    mutate(
      Genotipo_O = mapear.alelo.o(Naranja, Sexo),
      Genotipo_D = mapear.alelo.d(Dilucion)
    )
  
head(df.final, 20)
  
  
  