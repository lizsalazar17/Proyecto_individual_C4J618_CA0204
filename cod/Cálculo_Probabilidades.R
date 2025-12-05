#Vamos a definir una simulación teórica siguiendo el equilibrio de Handy Weinberg encontrado anteriormente.

library(tidyverse)
library(ggplot2)

#Definamos p y q, donde p es la probabilidad de que el gen porte el color naranja y q de que no. 

p <- frecuencia.O
q <- 1 - frecuencia.O


#Probabilidad para las hembras (ya que poseen dos alelos)

p.O.O <- p^2
p.O.o <- 2* p * q
p.o.o <- q^2


#Armaremos 100 parejas y cada pareja tendrá 100 crías para ver a qué convergería normalmente el genotipo de distintos gatos. 

n.parejas <- 100

n.crias <- 100

#Iniciamos una función general que nos ayude a simular una población completa

simulacion.teorica <- function(n.parejas, n.crias, p, q){
  
  #Generamos machos simulados
  
  machos.simulados = sample(
    x = c("x^Oy", "x^oy"), 
    size = n.parejas, 
    replace = TRUE, 
    prob = c(p,q)
  )
  
  #Generamos hembras simuladas
  hembras.simuladas = sample(
    x = c("x^Ox^O", "x^Ox^o", "x^ox^o"),
    size = n.parejas, 
    replace = TRUE,
    prob = c(p.O.O, p.O.o, p.o.o)
  )
  
  #Ahora cruzamos las parejas
  
  total.crias = character(0)
  
  for (i in seq_len(n.parejas)) {
    crias.i = cruce.gen.O(machos.simulados[i], hembras.simuladas[i], n.crias)
    total.crias = c(total.crias, crias.i)
  }
  
  tibble(Genotipo = total.crias) |>
    count(Genotipo, name = "Frecuencia") |>
    mutate(Proporcion = Frecuencia / sum(Frecuencia))
}
  
resultados.teorico <- simulacion.teorica(n.parejas, n.crias, p, q)

resultados.teorico

#Observemos nuestros resultados con un gráfico

ggplot(resultados.teorico, aes(x = Genotipo, y = Proporcion, fill = Genotipo)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Proporcion*100, 1), "%")),
            vjust = -0.3, size = 3) +
  labs(
    title = "Simulación teórica del gen O",
    subtitle = "Distribución de genotipos bajo p y q estimados",
    x = "Genotipo",
    y = "Proporción"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))



#Para ver la simulación empírica, es decir, usando los datos reales recolectados

simulacion.empirica <- function(df, n.parejas, n.crias){

  machos.reales = df |> filter(Sexo == "Macho") |> pull(Genotipo_O)
  hembras.reales = df |> filter(Sexo == "Hembra") |> pull(Genotipo_O)
  
  # por si la base tiene menos de lo que pedimos
  n.parejas.real = min(length(machos.reales), length(hembras.reales), n.parejas)
  
  
  padres <- sample(machos.reales, n.parejas.real, replace = TRUE)
  madres <- sample(hembras.reales, n.parejas.real, replace = TRUE)
  
  total.crias <- character(0)
  
  for (i in seq_len(n.parejas.real)) {
    crias.i <- cruce.gen.O(padres[i], madres[i], n.crias)
    total.crias <- c(total.crias, crias.i)
  }
  
  tibble(Genotipo = total.crias) |>
    count(Genotipo, name = "Frecuencia") |>
    mutate(Proporcion = Frecuencia / sum(Frecuencia))
}

# Vamos los resultados de la simulación empírica
resultados.empiricas <- simulacion.empirica(df.final, n.parejas, n.crias)
resultados.empiricas

#Observemos estos resultados en un gráfico

ggplot(resultados.empiricas, aes(x = Genotipo, y = Proporcion, fill = Genotipo)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Proporcion*100, 1), "%")),
            vjust = -0.3, size = 3) +
  labs(
    title = "Simulación empírica del gen O",
    subtitle = "Distribución de genotipos bajo p y q estimados",
    x = "Genotipo",
    y = "Proporción"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

#Comparemos los resultados teóricos con los empiricos 

resultados.comparados <- bind_rows(
  resultados.teorico |> mutate(Tipo = "Teórico"),
  resultados.empiricas |> mutate(Tipo = "Empírico")
)

ggplot(resultados.comparados, aes(x = Genotipo, y = Proporcion, fill = Tipo)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = paste0(round(Proporcion * 100, 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Comparación entre simulación teórica y empírica del gen O",
    x = "Genotipo",
    y = "Proporción"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

