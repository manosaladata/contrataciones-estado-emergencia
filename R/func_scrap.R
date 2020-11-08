
library(tidyverse)  #Usaremos en tibble
library(readxl)
library(xml2)  #Para leer html.
library(rvest)
library(RSelenium)  #Pra trabajar con páginas dinámicas
library(wdman)    #Navegador fantasma, permite usar rsDriver
library(robotstxt)
#library(strip)
library(gdata)

# local.xls<-download.file("https://www.datosabiertos.gob.pe/sites/default/files/CONOSCE_CONTRATACIONDIRECTA.xlsx","destinationFileName.csv")

RUCs<-c("20523717759","20555589574","20419385442","20338570041","20293847038")

#WEB SCRAPING
server<-phantomjs(port=5015L)
scrap<-function(ruc) {a<-ruc
a<-"20555589574"
UrlMadre<-paste0("https://apps.osce.gob.pe/perfilprov-ui/estadistica/",a,"#inhabMJ")
paths_allowed(paths = c(UrlMadre)) # TRUE
options(encoding = "utf-8")
Browser <- remoteDriver(browserName = "phantomjs", port=5015L) #browserName = "edge"
Browser$open()
Browser$navigate(paste(UrlMadre))#,RUCs[1],"#sanciones"))
Browser$screenshot(display=TRUE)
# Sys.sleep(2)
Pagina_actual<-Browser$getPageSource()
Pagina<-read_html(Pagina_actual[[1]])  #Por el "xml2"
PaginaRUC<-read_html(Pagina_actual[[1]])
pag_text<-PaginaRUC%>%
  html_nodes(css= ".data-container")%>% 
  html_text()
pag_text
}

#DEFINIMOS FUNCIONES
sanciones<- function(ruc){
  text<-scrap(ruc)
  sanciones<-str_match(text, "Sanciones del TCE (\\s*(.*?)\\s*)El proveedor") #tidyverse
  sanciones<-strsplit(sanciones, "") 
  sanciones <-as.numeric(sanciones[[2]][2])
  sanciones}

penalidades<- function(ruc){
  text<-scrap(ruc)
  penalidades<-str_match(text, "Penalidades (\\s*(.*?)\\s*)El proveedor") #tidyverse
  penalidades<-strsplit(penalidades, "") 
  penalidades<-as.numeric(penalidades[[2]][2])
  penalidades}

#TESTEO
a<-sanciones(RUCs[1])
a

b<-penalidades(RUCs[1])
b

c<-data.frame("Sanciones"=a, "Penalidades"=b)
c

