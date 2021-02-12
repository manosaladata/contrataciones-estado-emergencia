#' mod1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import leaflet
#' @import tmap



library(sf)    #Permite relación geos
library(tidyverse)
library(ggrepel)
library(tmap)
library(readxl)
library(leaflet)
library(here)
library(tmaptools)


#https://www.geogpsperu.com/2018/02/limite-departamental-politico-shapefile.html

#####Directorio y Data##############################################
#setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")
#setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Min_scripts/minvers/R")

#contr_direc <- read_excel("Data/CONOSCE_CONTRATACIONDIRECTA.xlsx")
contr_direc <- read_excel(here("R", "Data","CONOSCE_CONTRATACIONDIRECTA.xlsx"))
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)
options(scipen=999)                                 



################Usamos el .shp#############################################


#departamentos<-st_read("Data/DEPARTAMENTOS.shp")            ###OJO, SE REQUIEREN TODOS LOS ARCHIVOS
departamentos<-st_read(here("R","Data","DEPARTAMENTOS.shp"))  
ggplot(data = departamentos) +
  geom_sf()

ggplot(data = departamentos %>%
         filter(DEPARTAMEN=="LIMA")) +
  geom_sf()



########COLOCANDO NOMBRES##########
departamentos <- departamentos %>% mutate(centroid = map(geometry, st_centroid), 
                                          coords = map(centroid,st_coordinates), 
                                          coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords,2))





#Veamos los montos por departamentos
zonas<- select(contr_direc, "ENTIDAD_DEPARTAMENTO","MONTO_SOLES_EN_MILLONES")
names(zonas)[1]<-"DEPARTAMEN"


zonas <- group_by(zonas, DEPARTAMEN)
zonas<-summarise(zonas,MONTO=sum(MONTO_SOLES_EN_MILLONES),numero=n())
zonas<-arrange(zonas,desc(numero))
zonas_numero<-arrange(zonas,desc(numero))
df_zonas<-as.data.frame(zonas)
df_zonas_nolima<-df_zonas[-1,]
##########UNIMOS PARA MONTOS Y NÚMEROS###################
departamentos_montos <- departamentos%>% 
  left_join(zonas)

departamentos_numero <- departamentos%>%
  left_join(zonas_numero)


map_mon<-tm_shape(departamentos_montos) +
  tmap_options(bg.color = "green",inner.margins = c(0.1,0.1, 0.02,0.01)) +   #ubicamos a la leyenda
  tm_text('DEPARTAMEN',
          size = 0.5,
          fontface = 2,
          fontfamily = 'Tw Cen MT Condensed')+
  #tm_polygons("MONTO",palette = "viridis")+ #Greens
  tm_polygons("MONTO", title = "Millones de soles", style = "fixed",
              breaks = c(0, 30, 50, 80, 100, 1200, Inf),
              #textNA = "Lima", 
              colorNA = "green",   # <-------- color for NA values
              palette = "viridis")+
  tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "top"))  +
  #tm_borders(col = "black")+
  tm_layout(frame=FALSE,      #Sacamos el recuadro        
            main.title = 'MONTOS POR DEPARTAMENTO(mill)',
            main.title.size = 0.8,
            fontface = 2,
            fontfamily = 'Tw Cen MT Condensed',
            main.title.position = c(0.12,0.5)) +
  tm_scale_bar(size = 0.4,
               width = 0.21,
               color.dark = 'White',
               color.light = 'black',
               position = c(0.5,0.03))

map_mon<-tmap_leaflet(map_mon)

###########Por número
map_num<-tm_shape(departamentos_numero ) +
  tmap_options(bg.color = "green",inner.margins = c(0.1,0.1, 0.02,0.01)) +   #ubicamos a la leyenda
  tm_text('DEPARTAMEN',
          size = 0.5,
          fontface = 2,
          fontfamily = 'Tw Cen MT Condensed')+
  #tm_polygons("MONTO",palette = "viridis")+ #Greens
  tm_polygons("numero", title = "Contratos", style = "fixed",
              breaks = c(0, 100, 150, 200, 250, 2000, Inf),
              #textNA = "Lima", 
              colorNA = "green",   # <-------- color for NA values
              palette = "viridis")+
  tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "top"))  +
  #tm_borders(col = "black")+
  tm_layout(frame=FALSE,      #Sacamos el recuadro        
            main.title = 'Por número de contratos',
            main.title.size = 0.8,
            fontface = 2,
            fontfamily = 'Tw Cen MT Condensed',
            main.title.position = c(0.12,0.5)) +
  tm_scale_bar(size = 0.4,
               width = 0.21,
               color.dark = 'White',
               color.light = 'black',
               position = c(0.5,0.03))

#save(map_num, file = "mapa.Rdata")


mod_mod1_ui <- function(id){
  ns <- NS(id)
  tagList(h2("MAPA"),
  leafletOutput(ns("map_mon")))
}
    
#' mod1 Server Function
#'
#' @noRd 
mod_mod1_server <- function(input, output, session){
  ns <- session$ns
  output$map_mon <- renderLeaflet({map_mon}) 
 
}

# mapUI<-function(id) {leafletOutput(NS(id,"map_mon"))}
# 
# mapServer<-function(id){
#   moduleServer(id, function(input, output, session) {
#     output$map_mon <- renderLeaflet({map_mon})})}

## To be copied in the UI
# mod_mod1_ui("mod1_ui_1")
    
## To be copied in the server
# callModule(mod_mod1_server, "mod1_ui_1")
 
