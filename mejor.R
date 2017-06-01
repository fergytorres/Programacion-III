setwd("~/Actuaria/Progra 3 R")
leer <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
View(leer)
ncol(leer)
names(leer)
#11, 17, 23

mejor <- function(estado, resultado){
    
    #Lectura de datos
    setwd("~/Actuaria/Progra 3 R")
    leer <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    est <- factor(leer$State, ordered = T)
    
    #Revisión de la validez de estado y resultado
    if(! (estado %in% est)){
        stop("Estado inválido")
    } 
    
    if (!((resultado == "ataque") | (resultado == "falla") | 
          (resultado == "neumonia"))){
        stop("Resultado inválido")
    }
    
    #Hospital con tasa de mortalidad más baja
    
    if (resultado == "ataque"){
        columna = 11
    }
    if (resultado == "falla"){
        columna = 17
    }
    if (resultado == "neumonia"){
        columna = 23
    }
    
    est1 <- leer[grep(estado, est), ] #Grep busca coincidencias entre estado y est
    leer[ ,columna] <- suppressWarnings(as.numeric(leer[ ,columna]))
    r <- data.frame(est1$Hospital.Name, est1$State, 
                    suppressWarnings(as.numeric(est1[, columna])))
    r <- r[order(r$suppressWarnings.as.numeric.est1...columna...), ]
    x <- as.character(r[1,1])
    x
}
mejor("TX", "ataque")
mejor("TX", "falla")
mejor("MD", "ataque")
mejor("MD", "neumonia")
mejor("BB", "ataque")
mejor("NY", "atakue")