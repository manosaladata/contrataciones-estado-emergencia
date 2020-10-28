library(httr)
library(jsonlite)
#library(tidyverse)
#library(DT)

#RUCS
#IDEA: QUE EL USUARIO PONGA "INGRESE RUC" Y SALGA EL RESULTADO
#INGRESE RUC
RUC=20555589574
RUC_str<-as.character(RUC)
#Usamos paste para concatenar strings, con "" para evitar espacio:

url1<-paste("https://api.sunat.cloud/ruc/",RUC_str,sep = "", collapse = NULL)
res <- GET(url1)   #saldrá en formato unicode, necesitamos pasar a texto al leer en Json
#res
data<-fromJSON(content(res, type="text", encoding = "UTF-8"))  #llevamos a texto para manipular con Json
#data    #exploremos
#names(data)  #veamos qué variales podrían interesar


razon_social<-data[["razon_social"]]
empleados<-data[["empleados"]]       #[] dentro pues es como un diccionario
fecha_inscripción<-data[["fecha_inscripcion"]]
representante_legal<-data[["representante_legal"]]  #Primero el diccionarion general, si creo todo junto no saldrá
representante_legal_name<-representante_legal[[1]][["nombre"]]  #Ver las indicaciones de view para encontrar el código bien, DNI es el elemento 1
trabajadores_agosto<-empleados[["2020-08"]][["trabajadores"]]

x <- data.frame("Nombre de la empresa"=razon_social, "Nombre del representante legal(agosto)" = representante_legal_name,
                "Trabajadores(agosto-2020)" = trabajadores_agosto,"fecha de inscripción"=fecha_inscripción)
view(x)
