rankingcompleto <- function(resultado, num = "mejor" ){
    
    #Lectura de datos
    setwd("~/Actuaria/Progra 3 R")
    leer <- read.csv("outcome-of-care-measures.csv")
    
    #Revisión de la validez de estado y resultado
    if (!((resultado == "ataque") | (resultado == "falla") | (resultado == "neumonia"))){
        stop("Resultado inválido")
    }
    
    #Para cada estado, encuentra el hospital con la posición dada
    if (resultado == "ataque"){
        columna = 11
    }
    if (resultado == "falla"){
        columna = 17
    }
    if (resultado == "neumonia"){
        columna = 23
    }
    
    leer[ ,columna] <- suppressWarnings(as.numeric(levels(leer[ ,columna])
                                                   [leer[, columna]]))
    leer[ ,2] <- as.character(leer[ ,2])
    x <- vector()
    est <- levels(leer[, 7])
    
    for(k in 1:length(est)) {
        est1 <- leer[grep(est[k], leer$State), ]
        r <- est1[order(est1[ , columna], est1[ ,2], na.last = NA), ]
        
        h <- if (num == "mejor"){
            r[1,2]
        } else if(num == "peor"){
            r[nrow(r),2]
        } else {
            r[num, 2]
        }
        x <- append(x, c(h, est[k]))
    }
    
    x <- as.data.frame(matrix(x, length(est), 2, byrow = TRUE))
    colnames(x) <- c("Hospital", "Estado")
    row.names(x) <- est
    x
}

head(rankingcompleto("falla", 20), 10)
tail(rankingcompleto("neumonia", "peor"), 3) 
tail(rankingcompleto("falla"), 10) 
