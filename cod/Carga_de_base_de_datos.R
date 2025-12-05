#Carga de librerías

library(tidyverse)
library(dplyr)
library(readxl)

#Veamos la base de datos inicial 
data <- read_xlsx("data/gatos_petfinder_ny (2).xlsx")

View(data)

head(data)
names(data)

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
    select(ID, Sexo, Naranja, Dilucion, Color_Base)
  
  #Para el genotipo O ocupamos ver si el gato porta Naranja y si es Macho o Hembra, y además, si es Calicó
  
  mapear.alelo.o <- function(naranja, sexo, color_base){
    
    es_calico <- stringr::str_detect(
      color_base,
      stringr::regex("calico|tortie|tortoiseshell|torbie|dilute calico", ignore_case = TRUE)
    )
    
    case_when(
      
      sexo == "Macho" & !es_calico & naranja == "Sí" ~ "x^Oy",
      sexo == "Macho" & !es_calico & naranja != "Sí" ~ "x^oy",

      sexo == "Macho" & es_calico ~ NA_character_,

      sexo == "Hembra" & es_calico ~ "x^Ox^o",
      
      sexo == "Hembra" & !es_calico & naranja == "Sí" ~ "x^Ox^O",
      
      sexo == "Hembra" & !es_calico & naranja != "Sí" ~ "x^ox^o",
      
      TRUE ~ NA_character_
    )
  }
  
  
  #Para el genotipo D vamos analizar la dilución de cada gato
  
  mapear.alelo.d <- function(dilucion){
    ifelse(dilucion == "No", "D_", "dd")
  }
  
  #Ahora creamos el dataframe final con las columnas de genotipo incluido
  df.final <- df.filtrada2 %>% 
    mutate(
      Genotipo_O = mapear.alelo.o(Naranja, Sexo, Color_Base),
      Genotipo_D = mapear.alelo.d(Dilucion)
    )
  
head(df.final, 20)

df.final <- 
df.final %>%
    filter(!is.na(Genotipo_O))

df.final %>% count(Genotipo_O)

  write_csv(df.final, "gatos_petfinder_ny_final")
  
  #Para ver un resumen de los datos que analizaremos
  
  df.final %>%
    count(Genotipo_O) %>%
    ggplot(aes(x = Genotipo_O, y = n, fill = Genotipo_O)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = -0.4) +
    labs(
      title = "Frecuencia de genotipos del gen O (color naranja)",
      x = "Genotipo",
      y = "Frecuencia observada"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  