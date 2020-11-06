library(httr)
library(jsonlite)
#library(tidyverse)
#library(DT)
#httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))


#RUCS
#IDEA: QUE EL USUARIO PONGA "INGRESE RUC" Y SALGA EL RESULTADO
#INGRESE RUC
RUC=20555589574
RUC_str<-as.character(RUC)
#Usamos paste para concatenar strings, con "" para evitar espacio:

url1<-paste("https://api.sunat.cloud/ruc/",RUC_str,sep = "", collapse = NULL)
url1
res <- GET(url1)   #saldrá en formato unicode, necesitamos pasar a texto al leer en Json
#res
data<-fromJSON(content(res, type="text", encoding = "UTF-8"))  #llevamos a texto para manipular con Json
#data    #exploremos
#names(data)  #veamos qué variales podrían interesar


razon_social<-data[["razon_social"]]
empleados<-data[["empleados"]]       #[] dentro pues es como un diccionario
fecha_inscripcion<-data[["fecha_inscripcion"]]
representante_legal<-data[["representante_legal"]]  #Primero el diccionarion general, si creo todo junto no saldrÃÂ¡
representante_legal_name<-representante_legal[["1"]][["nombre"]]  #Ver las indicaciones de view para encontrar el cÃÂ³digo bien
trabajadores_agosto<-empleados[["2020-08"]][["trabajadores"]]

x <- data.frame("Nombre de la empresa"=razon_social, "Nombre del representante legal(agosto)" = representante_legal_name,
                "Trabajadores(agosto-2020)" = trabajadores_agosto,"fecha de inscripciÃ³n"=fecha_inscripcion)
view(x)

#Function
sunat<- function(x){
RUC_str<-as.character(x)
url1<-paste("https://api.sunat.cloud/ruc/",RUC_str,sep = "", collapse = NULL)
url1
res<- GET(url1)
data<-fromJSON(content(res, type="text", encoding = "UTF-8"))
razon_social<-data[["razon_social"]]
empleados<-data[["empleados"]]       
fecha_inscripcion<-data[["fecha_inscripcion"]]
representante_legal<-data[["representante_legal"]]  
representante_legal_name<-representante_legal[[1]][["nombre"]]  
trabajadores_agosto<-empleados[["2020-08"]][["trabajadores"]]
x <- data.frame("Nombre de la empresa"=razon_social, "Nombre del representante legal(agosto)" = representante_legal_name,
                "Trabajadores(agosto-2020)" = trabajadores_agosto,"fecha de inscripci�n"=fecha_inscripcion)
#x #para ver de una vez
#view(x)
}

trabajadores<-function(X){
a<-sunat(20555589574)
b<-a[1,3]
as.numeric(b)
}

#Testeo:
trabajadores(20555589574)
