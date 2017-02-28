
intsall.packages('dplyr')
library(dplyr)
#Dame un data frame con la media de millas por galón por cada tipo de transmisión.
mtcars %>% group_by(am) %>% summarise(millaspromesdio = mean(mpg))
#Saca la desviación estándar de la potencia (caballos de fuerza), para los distintos números de cilindros.
mtcars %>% group_by(cyl) %>% summarise(desviacionpornumerodecilindro= sd(hp))
#Crea una nueva variable que sea la multiplicación de qsec y disp.
mtcars %>%  mutate (summarise(despalazmiento_por_milla= qsec * disp ))
#Muestrame los 10 valores más altos de la variable hp, para los coches de transmisión manual.
tail(sort(mtcars$hp[mtcars$am ==1]), n=10)
#¿Por qué mtcars %>% select('hp') no funciona? ¿Qué tendrías que hacer para que funcionara?
#según tengo entendido el problema es que select() puede trabajar con argumentos tipo data por tanto cuando ponemos 'hp' lo entiende como un character 
#por tanto, para que funcione tenemos que poner
mtcars %>% select(hp)