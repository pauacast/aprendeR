meu<-list('a'= 1:10, b= matrix(1:6, nrow=2) )
lapply(meu, mean)
meu[[1]]<-replace(meu[[1]], 2, NA)
lapply(meu, mean)
#primera manera discrimar NA
meu[[1]] <- meu[[1]][!is.na(meu[[1]])]
lapply(meu, mean)
#segunda manera especificar en funcion mean
replace(meu[[1]], 2, NA)
mean(meu[[1]], na.rm=FALSE)
#nota que ambos arrojan mismo resultado 5.888889
frase<- c('hola','como','estas')
frase('primer'='hola', 'segundo'='como', 'tercer'='estas')
grep('a', frase)
#encontrar columna por especificacion
frase[grep('a',frase)]
#encontrar columna por numero
frase[[1]]
#encontar columna por nombre
frase['primer']