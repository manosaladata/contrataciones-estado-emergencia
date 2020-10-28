library(rvest)


## Web scraping con Selector Gadget 
#######################################


# Ubicamos una URL que deseamos escrapear
URL <- "https://apps.osce.gob.pe/perfilprov-ui/"


# (1) Descargar toda la web: read_html()
osce <- read_html(URL)
osce

nombres<-osce%>%
  html_nodes(".tile__proveedor-name")%>%    #el . indica que es una clase de nombre "list.price..."
  html_text()
