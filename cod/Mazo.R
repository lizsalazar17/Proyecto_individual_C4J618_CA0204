#Primero, descarguemos las librerías necesarias

library(dplyr)



#Vamos a generar el mazo. Sabemos que hay 52 cartas en total, con 4 palos diferentes: Corazón, Diamante, Pica y Treból.
#Cada carta va desde el númwro 1 al 10, y luego están las cartas nombradas J, Q y K. Vamos a enumerar cada carta del 1 al 52. 

#Para obtener su valor tenemos el siguiente código. 
obtener.valor <- function(carta){
  if(carta > 13){
    return(carta - 13)
  }
  return(carta)
}
#Por ejemplo, si queremos saber el valor de la carta enumerada como 14 (que es la As de Diamante) obtendríamos 1. 
obtener.valor(14)

#Para obtener el palo tenemos el siguiente método: 
obtener.palo <- function(carta){
  palo = ""
  case_when
  if(carta > 0 & carta < 14){
    return("corazones")
  }else if (carta > 13 & carta < 27){
    return()
  }
}