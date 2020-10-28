#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

url1<-"https://api.sunat.cloud/ruc/20419385442"
res <- GET(url)
res    #Está en formato unicode
#Transformando a formato Json, para ello primero pasaremos del unicode a un vector de caracteres
#rawToChar convierte bytes sin formato (raw) en una sola cadena 
#de caracteres o en un vector de caracteres de bytes individuales

#rawToChar(res$content) 

#Ahora que tenemos caracteres, ya podemos leer con Jason:
#data <- fromJSON(rawToChar(res$content))
library(tidyverse)
data2<-fromJSON(content(res, type="text", encoding = "UTF-8"))  #Pasamos de texto a formato Json
response <- content(res, as = "text", encoding = "UTF-8")
df <- fromJSON(response, flatten = TRUE) %>% 
  data.frame




#names(data)       #Nos da el nombre de los keys.
names(data2)

empleados2<-data2[["empleados"]]

View(empleados2)
empleados2$trabajadores
#ANÁLISIS
#Por ejemplo tengo interés en los empleados del ruc 20419385442
empleados<-data$empleados

