#antes de empezar nos aseguramos de que tengamos instalados los paquetes de pew.R y los corremos
#ejercicio 1
setwd("C:/Users/Paulina/git/git/aprendeR/02_manipulacion")
source('pew.R')
kable(raw)
raw %>%
  gather(key = income, 
         value = frequency, -religion) %>%
  head(15) %>%
  kable()
#r nos marca un error que se debe a que no tengo instalado el paquete donde viene la función kable por lo que hay que instalar knitr y CARGARLO
install.packages('knitr')
library(knitr)
#volvemos a correr la función anterior
kable(raw)
raw %>%
  gather(key = income, 
         value = frequency, -religion) %>%
  head(15) %>%
  kable()
#ya funciona! además qué bonita se ve la tabla así
#ejercicio 2
misdatos_vertical <- data.frame(region = sort(rep(LETTERS[1:10], 3)),
                                type = rep(c('indice_desarrollo',
                                             'indice_marginacion',
                                             'indice_seguridad'), 10),
                                value = rnorm(30))

kable(misdatos_vertical)
#para separar los indices de la columna type usamos funcion spread
misdatosseparados<- spread(misdatos_vertical, type, value)
kable(misdatosseparados)
#que bonito es lo bonito
#tercer ejercicio
demandados <- c('Walmart',
                'Oxxo',
                'Electra',
                'Elmex Superior')
demandados2 <- c('Quien resulte responsable',
                 'Manpower',
                 'Compis de Outsourcing, S.A')
demandados3 <- c('IMSS', 'SAR', 'INFONAVIT', 'Otro compi')
datos_calculadora <- data.frame(id_exp = paste0('2_', 
                                                sample(1:200, 20),
                                                '_2017'),
                                d_1 = sample(demandados, 20, replace = T),
                                d_2 = sample(demandados2, 20, replace = T),
                                d_3 = sample(demandados3, 20, replace = T)) 
datos_calculadora %>%
  head(20) %>%
  kable()

