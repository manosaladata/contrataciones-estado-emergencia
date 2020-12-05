# Proveedores del estado

library(xml2)  #Para leer html.
library(rvest)
library(RSelenium)  #Pra trabajar con páginas dinámicas
library(wdman)    #Navegador fantasma, permite usar rsDriver
library(robotstxt)
library(tidyverse)
#library(strip)

#---- Reselenium y Rvest #----
RUCs<-c("20523717759","20555589574","20419385442","20338570041","20293847038")

UrlMadre<-paste0("https://apps.osce.gob.pe/perfilprov-ui/estadistica/",RUCs[2],"#inhabMJ")

paths_allowed(paths = c(UrlMadre)) # TRUE

options(encoding = "utf-8")

#----Parte Rselenium #----

#Abrimos una sesion en la web

# Ejecutamos el servidor phantomjs -creamos un navegador fantasma

server<-phantomjs(port=5015L) # cambiar el puerto si se opera en otra laptop, es conveniente.
#Abrimos el navegador (con RSelenium)
Browser <- remoteDriver(browserName = "phantomjs", port=5015L) #browserName = "edge"
Browser$open()
#Navegar la p?¡gina web que guardamos
Browser$navigate(paste(UrlMadre))#,RUCs[1],"#sanciones"))
Browser$screenshot(display=TRUE)

# Decirle que act?ºe sobre la pÃ¡gina

Pagina_actual<-Browser$getPageSource()

Pagina<-read_html(Pagina_actual[[1]])  #Por el "xml2"
Pagina


#---- Rvest # ----

#FILTRANDO
PaginaRUC<-read_html(Pagina_actual[[1]])


#TRABAJANDO CON SANCIONES DEL TCE
pag_text<-PaginaRUC%>%
  html_nodes(css= ".data-container")%>% 
  html_text()

pag_text

sanciones<-str_match(pag_text, "Sanciones del TCE (\\s*(.*?)\\s*)El proveedor") #tidyverse
sanciones<-strsplit(sanciones, "") 
sanciones <-as.numeric(sanciones[[2]][2])
sanciones


#TRABAJANDO CON LAS PENAS
penalidades<-str_match(pag_text, "Penalidades (\\s*(.*?)\\s*)El proveedor") #tidyverse
penalidades<-strsplit(penalidades, "") 
penalidades<-as.numeric(penalidades[[2]][2])
penalidades


#INHABILITACI?N JUDICIAL
inhabilit_j<-str_match(pag_text, "mandato judicial (\\s*(.*?)\\s*)El proveedor")
inhabilit_j<-strsplit(inhabilit_j, "") 
inhabilit_j<-as.numeric(inhabilit_j[[2]][2])
inhabilit_j
#INHABILITACI?N ADMINISTRATIVA

inhabilit_ad<-str_match(pag_text, "administrativa (\\s*(.*?)\\s*)El proveedor")
inhabilit_ad<-strsplit(inhabilit_ad, "") 
inhabilit_ad<-as.numeric(inhabilit_ad[[2]][2])
inhabilit_ad


#https://apps.osce.gob.pe/perfilprov-ui/ficha/20523717759
#https://apps.osce.gob.pe/perfilprov-ui/ficha/20555589574
#https://apps.osce.gob.pe/perfilprov-ui/ficha/20419385442
#https://apps.osce.gob.pe/perfilprov-ui/ficha/20338570041
#https://apps.osce.gob.pe/perfilprov-ui/ficha/20293847038
#http://www.minem.gob.pe/_detalle.php?idSector=10&idTitular=7194&idMenu=sub294

#20523717759
#20555589574	
#20419385442
#20338570041
#20293847038

#cerrar la sesi?³n (Rselenium)

Browser$close()
server$stop()

