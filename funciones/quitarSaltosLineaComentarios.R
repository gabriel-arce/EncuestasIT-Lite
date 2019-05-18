quitarSaltosLineaComentarios <- function(txtdata){
    
    # vamos a retornar una lista de lineas de texto
    retorno <- character(0)
    
    i <- 2 ## Desde la linea 2, ya que la primera es el encabezado
    iret <- 1
    
    hasta <- length(txtdata) # hasta el final del archivo
    
    retorno[1] <- txtdata[1] # nos copiamos el encabezado
     
    while(i <= hasta){
        
        
        # si empieza con "numeros,numeros-" entonces es una linea correcta. 
        if(grepl("^[[:digit:]]*,[[:digit:]]*-", txtdata[i]))
        {   
            iret <- iret + 1
            retorno[iret] <- txtdata[i]
        }
        else
        {
            # si no es una linea incorrecta (pq hay un salto de linea)
            # así que la ubicamos a continuación de la anterior
            retorno[iret] <- paste(retorno[iret], txtdata[i])
        }
            
        i <- i + 1
    }
    
    retorno
}