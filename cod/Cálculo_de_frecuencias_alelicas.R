  

#Cargamos librerías

library(tidyverse)
library(ggplot2)

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
total.machos.o <- sum(df.machos$Genotipo_O == "x^oy")

total.alelos.machos <- nrow(df.machos)

#Loa alelos donde se presenta x^Oy^O lo multiplicamos por 2 ya que se encuentran 2 alelos del gen O
total.hembras.O.O <- sum(df.hembras$Genotipo_O == "x^Ox^O") * 2
total.hembras.O.o <- sum(df.hembras$Genotipo_O == "x^Ox^o")
total.hembras.o.o <- sum(df.hembras$Genotipo_O == "x^ox^o")
total.alelos.hembras <- nrow(df.hembras)

total.hembras.O <- total.hembras.O.O + total.hembras.O.o

total.alelos.X <- total.alelos.machos + (total.alelos.hembras * 2)
total.alelos.O <- total.machos.O + total.hembras.O

#Para ver la frecuencia alelica obtenida

frecuencia.O <- (total.alelos.O / total.alelos.X ) 

frecuencia.o <- 1 - frecuencia.O

frecuencia.O
frecuencia.o

#Veamos el equilibrio del gen o 
#Note que como los machos solo necesitan un cromosoma X, que es el que hereda el gen del color naranja, la probabilidad sería nada más p + q = 1. 
#Por lo tanto, se toma la variable frecuencia. O = p y la variable frecuencia.o = q. 

#Entonces, para machos se esperaría: 

machos.esperado.O <- total.alelos.machos * frecuencia.O

machos.esperado.o <- total.alelos.machos * frecuencia.o

machos.esperado.O

#Para el caso de la hembras, si necesitamos usar la ecuación completa de Handy-Weinberg donde la probabilidad se vería así: 
#p ^ 2 + 2pq + q ^2
#Entonces, el valor esperado de las hembras con el alelo O es de: 

hembras.esperado.OO <- total.alelos.hembras * (frecuencia.O) * (frecuencia.O)

hembras.esperado.oo <- total.alelos.hembras * (frecuencia.o) * (frecuencia.o)

hembras.esperado.Oo <- total.alelos.hembras * 2 * (frecuencia.O) * (frecuencia.o)

#Comparemos el resultado con lo observado antes

total.hembras.O.o
hembras.esperado.Oo

total.hembras.o.o
hembras.esperado.oo

total.hembras.O.O
hembras.esperado.OO

total.machos.O
machos.esperado.O

total.machos.o
machos.esperado.o

#Utilicemos chi cuadrado para ver que tanto cambio el valor esperado con el valor observado. 


chi.cuadrado <- function(valor.esperado, valor.observado){ 
      sum((valor.observado - valor.esperado)^2/valor.esperado) 
  } 
#Para calcular el chi cuadrado de los machos 

vector.esperado.m<- c(machos.esperado.O, machos.esperado.o)

vector.observado.m <- c(total.machos.O, total.machos.o) 

chi.machos <- chi.cuadrado(vector.esperado.m, vector.observado.m) 

chi.machos

p.val.m <- 1 - pchisq(chi.machos, 1)

p.val.m 
#Para calcular el chi cuadrado de las hembras 
vector.esperado.h <- c(hembras.esperado.OO, hembras.esperado.Oo, hembras.esperado.oo)

vector.observado.h <- c(total.hembras.O.O, total.hembras.O.o, total.hembras.o.o) 

chi.hembras <- chi.cuadrado(vector.esperado.h, vector.observado.h) 

chi.hembras

p.val.h <- 1 - pchisq(chi.hembras, 2) 
p.val.h

#Veamos los resultados de manera más ordenada
resultados.equilibrio <- tibble(
  Categoria = c("X^OY", "X^oY", "X^OX^O", "X^OX^o", "X^oX^o"),
  Observado = c(total.machos.O, total.machos.o,
                total.hembras.O.O, total.hembras.O.o, total.hembras.o.o),
  Esperado = c(machos.esperado.O, machos.esperado.o,
               hembras.esperado.OO, hembras.esperado.Oo, hembras.esperado.oo),
  Diferencia = Observado - Esperado
)

resultados.equilibrio


resultados.long <- resultados.equilibrio |>
  pivot_longer(
    cols = c(Observado, Esperado),
    names_to = "Tipo",
    values_to = "Frecuencia"
  )

ggplot(resultados.long, aes(x = Categoria, y = Frecuencia, fill = Tipo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Frecuencia, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(
    title = "Genotipos observados vs esperados bajo Hardy–Weinberg",
    x = "Genotipo",
    y = "Frecuencia"
  ) +
  theme_minimal()





































































