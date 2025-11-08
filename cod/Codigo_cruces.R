#Hagamos una función que nos ayuda ver la posible descendencia entre dos gatos

#Esta función tomará los geneotipos del padre y la madre, y la cantidad de crías que tendrán. 
cruce.gen.O <- function(padre.genotipo, madre.genotipo, n.gatitos){
  #Para filtrar los alelos del padre
  alelo.padre = if (grepl("x\\^O", padre.genotipo)) "O" else "o"
  
  madre.sin.prefijo = sub("^x\\^", "", madre.genotipo)
  
  alelos.madre = strsplit(madre.sin.prefijo, "x\\^")[[1]]
  
  
  descendencia = character(n.gatitos)
  
    for (i in seq_len(n.gatitos)) {
    #Sabemos que la madre aporta uno de sus dos cromosomas X
    
      alelo.madre = sample(alelos.madre, 1)
    
      #La probabilidad de que sea hembra o macho depende de un 50%
    
      sexo = sample(c("Macho", "Hembra"), 1)
      if (sexo == "Macho"){
      #El padre le hereda el cromosoma Y al hijo independientemente de que genotipo tenga.
      descendencia[i] = paste0("x^", alelo.madre, "y")
      } else {
      
      #usamos sort para ordenar el alelo del padre primero y luego el de la madre
      alelos.h <- sort(c(alelo.padre, alelo.madre), decreasing = TRUE)
      descendencia[i] = paste0("x^", alelos.h[1], "x^", alelos.h[2])
      
      }
      
    }
  
  descendencia
    
  }
  