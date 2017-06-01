estado <- "TX"
resultado <- "falla"
num <- 4

rankhospital <- function(estado, resultado, num){
    
    #Lectura de datos
    setwd("~/Actuaria/Progra 3 R")
    leer <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    est <- factor(leer$State, ordered = T)
    
    #Revisión de la validez de estado y resultado
    if(! (estado %in% est)){
        stop("Estado inválido")
    } 
    
    if (!((resultado == "ataque") | (resultado == "falla") | (resultado == "neumonia"))){
        stop("Resultado inválido")
    }
    
    #Hospital con el puesto ado de la tasa más baja de mortalidad
    if (resultado == "ataque"){
        columna = 11
    }
    if (resultado == "falla"){
        columna = 17
    }
    if (resultado == "neumonia"){
        columna = 23
    }
    
    est1 <- leer[grep(estado, leer$State), ] #Grep busca coincidencias entre estado y est
    
    leer[ ,columna] <- suppressWarnings(as.numeric(leer[ ,columna]))
    r <- data.frame(est1$Hospital.Name, est1$State, 
                    suppressWarnings(as.numeric(est1[,columna])))
    r <- r[order(r$est1.Hospital.Name), ]
    r <- r[order(r$suppressWarnings.as.numeric.est1...columna...), ]

    
    if (num == "mejor"){
        hs <- r[1,1]
    } else if(num == "peor"){
        hs <- r[nrow(r),1]
    } else {
        hs <- r[num, 1]
    }
    as.character(hs)
}

rankhospital("TX", "falla", 4)
rankhospital("MD", "ataque", "peor")
rankhospital("MN", "ataque", 5000)
