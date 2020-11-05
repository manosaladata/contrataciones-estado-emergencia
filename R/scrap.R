# Proveedores del estado

library(xml2)  #Para leer html.
library(rvest)
library(RSelenium)  #Pra trabajar con p√°ginas din√°micas
library(wdman)    #Navegador fantasma, permite usar rsDriver
library(robotstxt)
#library(strip)

#---- Reselenium y Rvest #----

UrlMadre<-"https://apps.osce.gob.pe/perfilprov-ui/estadistica/20523717759#inhabMJ"

paths_allowed(paths = c(UrlMadre)) # TRUE

options(encoding = "utf-8")

#----Parte Rselenium #----

#Abrimos una sesion en la web

# Ejecutamos el servidor phantomjs -creamos un navegador fantasma

RUCs<-c("20523717759","20555589574","20419385442","20338570041","20293847038")

server<-phantomjs(port=5015L) # cambiar el puerto si se opera en otra laptop, es conveniente.
#Abrimos el navegador (con RSelenium)
Browser <- remoteDriver(browserName = "phantomjs", port=5015L) #browserName = "edge"
Browser$open()
#Navegar la p√¬°gina web que guardamos
Browser$navigate(paste(UrlMadre,RUCs[1],"#sanciones"))
Browser$screenshot(display=TRUE)

# Decirle que act√¬∫e sobre la p√É¬°gina

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

text_list<-strsplit(pag_text, "")
text_list
sanciones_TCE_num_str<-text_list[[1]][63]
sanciones_TCE_num_str
sanciones<-as.integer(sanciones_TCE_num_str)
sanciones

#TRABAJANDO CON LAS PENAS
PaginaRUC<-read_html(Pagina_actual[[1]])

penalidades_text<-PaginaRUC%>%
html_node(xpath = '//*[@id="penalidades"]/h6')%>%
html_text()


penalidades_list<-strsplit(penalidades_text, "")
penalidades_num_str<-penalidades_list[[1]][14]
penalidades<-as.integer(penalidades_num_str)
penalidades

#INHABILITACI”N JUDICIAL
#text_list lee solo hasta 1000 coordenadas
text_list_sec_part<-text_list[[1]][988:1500]
text_list_sec_part
inhab_ju_num_str<-text_list_sec_part[153]
inhab_ju_num_str
inhab_ju<-as.integer(inhab_ju_num_str)
inhab_ju

#INHABILITACI”N ADMINISTRATIVA
text_list_sec_part
inhab_ad_num_str<-text_list_sec_part[436]
inhab_ad_num_str
inhab_ad<-as.integer(inhab_num_str)
inhab_ad

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

#cerrar la sesi√¬≥n (Rselenium)

Browser$close()
server$stop()

