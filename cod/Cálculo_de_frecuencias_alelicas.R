  

#Cargamos librerías

library(tidyverse)

# Iniciamos nuestra base de datos final
df <- df.final
#head(df,20)

# Frecuencias alelicas para el genotipo 0 (color naranja)

#Separemos Machos y Hembras
df.machos <-  df %>% 
  filter(Sexo == "Macho")

df.hembras <- df %>% 
  filter(Sexo=="Hembra")

#head(df.machos)
#head(df.hembras)

#Veamos el total de alelos O en los machos
total.machos.O <- sum(df.machos$Genotipo_O == "x^Oy")

total.alelos.machos <- nrow(df.machos)

#Loa alelos donde se presenta x^Oy^O lo multiplicamos por 2 ya que se encuentran 2 alelos del gen O
total.hembras.O.O <- sum(df.hembras$Genotipo_O == "x^Oy^O") * 2
total.hembras.O.o <- sum(df.hembras$Genotipo_O == "x^Ox^o")
total.alelos.hembras <- nrow(df.hembras)

total.hembras.O <- total.hembras.O.O + total.hembras.O.o

total.alelos.X <- total.alelos.machos + (total.alelos.hembras * 2)
total.alelos.O <- total.machos.O + total.hembras.O

#Para ver la frecuencia alelica obtenida

frecuencia.O <- (total.alelos.O / total.alelos.X ) 

frecuencia.o <- 1 - frecuencia.O

frecuencia.O
frecuencia.o

