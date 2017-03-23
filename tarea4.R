setwd("C:/Users/Paulina/git/git/aprendeR")
#cargamos las librerías importantes
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
limpia_fechas <- function(date){
  fecha <- as.Date(as.numeric(date), origin="1899-12-30")
  fecha[grepl("\\/", date)] <- dmy(date[grepl("\\/", date)])
  fecha
} 
#obre esta función : a lo que entiendo el dmy es necesario para transformar la información que encuentra el grepl siguiendo el patrón "\\/" en la columna "fecha" (que reúne en un vector de elementos al que llama date 
#en información tipo "Date" o POIXct. Esta transformación es necesaria para poder trabajar con la función as_Date ya que, como argumento sólo puede tomar datos de esta naturaleza
#as_Date toma entonces, la información del vector date y lo transforma para que esté en el formato Date 
#Resumiendo las funciones que utilizamos hacen transformaciones para que los datos que están en el excel (o donde apliquemos la función) que cumpla con las condiciones del greplsean transformados a tipo fecha y sean separadas por guiones


f <- read.csv('base_iniciales.csv', as.is = T) %>%
  dplyr::select(., clave:dummy_nulidad, fecha_termino:liq_total_tope) %>%
  mutate_each(., funs(limpia_fechas), 
              contains("fecha"), -contains("vac"), -contains("ag"))
#sobre esta función: lo que hace es seleccionar de la base que estamos trabajando las columnas desde clave hasta dummy_nulidad y desde fecha_termino hasta liq_total_tope y a éstas aplicarles la funcion que construimos anteriormente
#esto es posible gracias a funs que permite que se realice la funcion limpia_fechas dentro de la función mutate_each
#es necesario especificar que sólo queremos que esta transformación se realice en lascolumnas "fecha" como fecha de despido y de entrada y no en vacaciones ni aguinaldo

#ya que entendimos esto podemos continuar...

#Ejercicio Codemanda IMSS
#primero generamos un data.frame que contenga sólo las columnas que nos interesan
datos<- data.frame(f$id_exp,f$nombre_d1,f$nombre_d2,f$nombre_d3,f$nombre_d4,f$nombre_d5,f$nombre_d6)
View(datos)
#para colapsar columnas de demandados
datos1<-gather(datos,key=nombre_dem, value=nom_dem, -f.id_exp)%>% group_by(nom_dem)
#generamos una función que nos arroje 1 o 0 si demandan a alguien del SARIMSSINFO
funcion = function(x){
  dummy <- ifelse(x %in% c("INSTITUTO MEXICANO DEL SEGURO SOCIAL", "IMSS", "INFONAVIT", "COMISION NACIONAL PARA EL AHORRO PARA EL RETIRO" , "INSTITUTO DEL FONDO NACIONAL DE LA VIVIENDA PARA LOS TRABAJADORES"), 1, 0)
dummy}
#con la función transmute generamos una nueva columna que podemos guardar como objeto
nuevosdatos <- transmute ( datos1, nuevacolumna = funcion(nom_dem))
#hacemos un data frame que contenga el id y el dummy unicamente
ejercicio1<-data.frame( datos1$f.id_exp,nuevosdatos$nuevacolumna )
#filtramos para ver qué expedientes demandan al SARIMSSINFO
ejercicio1 %>% filter(nuevosdatos.nuevacolumna==1) %>% View()

#para pegar la columna nueva de dummy a la anterior podemos, entre otras cosas, generar un nuevo data.frame
tablaextra<- data.frame(datos1,ejercicio1$nuevosdatos.nuevacolumna)
View(tablaextra)
#pregunta: ¿Cómo colapsar los id ?

#ejercicio 2
#Primero transformamos todas las variables a numéricas
datos %>% select(starts_with("c_", "monto", "suedo")) %>%funs(as_numeric)
f %>% select(starts_with("monto"))
columalimp<- data.frame(startsWith(f,"monto"),starts_with(f,"c_"),starts_with(f,"sueldo"),starts_with(f,"liq"))
tablita[is.na(tablita)] <-0



#ejercicio3
#razón c_total y min_ley
mutate(f,razon= c_total/min_ley)

#ejercicio4
df <- df %>%  mutate(monto_hextra_sem = na_function(otra_function(monto_hextra_sem))) %>%
  mutate(monto_vac = na_function(otra_function(monto_vac))) %>%
  mutate(monto_ag = na_function(otra_function(monto_ag))) %>%
 mutate(monto_indem = na_function(otra_function(monto_indem))) %>%
 mutate(monto_sal_caidos = na_function(otra_function(monto_sal_caidos))) %>%
   mutate(monto_prima_antig = na_function(otra_function(monto_prima_antig))) %>%
 mutate(monto_prima_vac = na_function(otra_function(monto_prima_vac))) %>%
 mutate(monto_hextra_total = na_function(otra_function(monto_hextra_total))) %>%
  mutate(monto_desc_sem= na_function(otra_function(monto_desc_sem))) %>%
  mutate(monto_desc_ob= na_function(otra_function(monto_desc_ob))) %>%
   mutate(monto_utilidades= na_function(otra_function(monto_utilidades)))

  +df %>% select(contains("monto")) %>%  View()

#ejercicio5
f$hextra_sem <- as.numeric(f$hextra_sem)
mutate(f,proporcion =  hextra_sem / c_total)


#ejercicio6 

descpachos<-data.frame(f$despacho_ac, f$id_exp)
descpachos%>% 
  +   +     gather(key= despcahos, value= nombre_despacho, -f.id_exp) %>% 
  +   +     group_by(nombre_despacho) %>%
  +   +     summarise(freq=n()) %>%
  +   +     arrange(-freq) %>% View()
#notése que hay un error con esta línea 

#ejercicio8
filter(f,accion_principal== "REINSTALACION")


#propuesta 2 ejercici1

datos$f.nombre_d1<- as.character (datos$f.nombre_d1)
datos$f.nombre_d2<- as.character (datos$f.nombre_d2)
datos$f.nombre_d3<- as.character (datos$f.nombre_d3)
datos$f.nombre_d4<- as.character (datos$f.nombre_d4)
datos$f.nombre_d5<- as.character (datos$f.nombre_d5)
datos$f.nombre_d6<- as.character (datos$f.nombre_d6)
datos<- data.frame(f$id_exp,f$nombre_d1,f$nombre_d2,f$nombre_d3,f$nombre_d4,f$nombre_d5,f$nombre_d6)
View(datos) 
datos %>% as.integer
(starts_with("f.nombre"))%>% mutate_at(vars(starts_with("f.nombre")),funcion)
datos1<- datos %>% mutate_if(is.character ,funcion)
datos %>% group_by(f.id_exp)%>% suma<- mutate_at(vars(starts_with("f.nombre")), sum) %>% ifelse(suma>0 , 1,0)
#nótese que no funciona esta línea no sé por qué :(