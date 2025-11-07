#Vamos a definir una simulación teórica siguiendo el equilibrio de Handy Weinberg encontrado anteriormente.

library(tidyverse)
library(ggpplot2)

#Definamos p y q, donde p es la probabilidad de que el gen porte el color naranja y q de que no. 

p <- frecuencia.O
q <- 1 - frecuencia.o


#Armaremos 100 parejas y cada pareja tendrá 100 crías para ver a qué convergería normalmente el genotipo de distintos gatos. 

n.parejas <- 100

n.crias <- 100

#Iniciamos una función general que nos ayude a simular una población completa

simulacion.teorica <- function(n_parejas, n_crias, p, q){
  
  #Generamos machos simulados
  
  machos.simulados <- sample(
    x = c("x^Oy", "x^oy"), 
    tamanio = n_parejas, 
    replace = TRUE, 
    probabilidad = c(p,q)
  )
  
  #Probabilidad para las hembras (ya que poseen dos alelos)
  
  p.O.O <- p^2
  p.O.o <- 2* p * q
  p.o.o <- q^2
  
  
  
  #Generamos hembras simuladas
  hembras.simuladas <- sample(
    x = c("x^Ox^O", "x^Ox^o", "x^ox^o"),
    tamanio = n_parejas, 
    replace = TRUE,
    prob = c(p.O.O, p.O.o, p.o.o)
  )
  
  #Ahora cruzamos las parejas
  
}