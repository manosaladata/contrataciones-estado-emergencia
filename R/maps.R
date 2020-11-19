library(sf)    #Permite relación geos
library(tidyverse)
library(ggrepel)
library(tmap)
#https://www.geogpsperu.com/2018/02/limite-departamental-politico-shapefile.html
#####Directorio y data##############################################
setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data/Mapas_depar")

contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#contr_direc<-na.omit(contr_direc)                       No usar, sino se irÃ¡ casi toda la base se elimina.
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)
options(scipen=999)                                  #Evita que salga en notaciÃ³n cientÃ­fica (exponencial).
sapply(contr_direc, class)                          #Analizamos la clase de cada columna. Ojo: FECHACONVOCATOERIA ESTÃ EN POSIXct y POSIxt q es formato de fecha

#############################################################


departamentos<-st_read("DEPARTAMENTOS.shp")            ###OJO, SE REQUIEREN TODOS LOS ARCHIVOS
ggplot(data = departamentos) +
  geom_sf()

ggplot(data = departamentos %>%
         filter(DEPARTAMEN=="LIMA")) +
  geom_sf()



########COLOCANDO NOMBRES##########
departamentos <- departamentos %>% mutate(centroid = map(geometry, st_centroid), 
                                          coords = map(centroid,st_coordinates), 
                                          coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords,2))

ggplot(data = departamentos) +
  geom_sf(fill="red", color="white")+ 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = DEPARTAMEN), size = 2.25) 







#Veamos los montos por departamentos
zonas<- select(contr_direc, "ENTIDAD_DEPARTAMENTO","MONTO_SOLES_EN_MILLONES")
names(zonas)[1]<-"DEPARTAMEN"


zonas <- group_by(zonas, DEPARTAMEN)
zonas<-summarise(zonas,MONTO=sum(MONTO_SOLES_EN_MILLONES),numero=n())
zonas<-arrange(zonas,desc(numero))
zonas_numero<-arrange(zonas,desc(numero))
  

##########UNIMOS PARA MONTOS Y NÚMEROS###################
departamentos_montos <- departamentos%>% 
  left_join(zonas)

departamentos_numero <- departamentos%>% #Juntamos ambas bases de datos
  left_join(zonas_numero)
#################################################

# ggplot(departamentos_montos) +
#   geom_sf(aes(fill = MONTO))+
#   labs(title = "MONTOS ADJUDICADOS EN CONTRATACIONES DIRECTAS POR DEPARTAMENTO",
#        caption = "Fuente: DATOS ABIERTOS
#        Elaboración propia",
#        x="Longitud",
#        y="Latitud")+
#   scale_fill_continuous(guide_legend(title = "Montos adjudicados"))
#   #+geom_text_repel(mapping = aes(coords_x, coords_y, label = DEPARTAMEN), size = 2.25)

tm_shape(departamentos_montos) +
  tmap_options(inner.margins = c(0.1,0.1, 0.02,0.01)) +   #ubicamos a la leyenda
  tm_text('DEPARTAMEN',
          size = 0.5,
          fontface = 2,
          fontfamily = 'Tw Cen MT Condensed')+
  tm_polygons("MONTO",palette = "viridis")+ #Greens
  tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "top"))  +
  #tm_borders(col = "black")+
  tm_layout(frame=FALSE,      #Sacamos el recuadro        
            main.title = 'MONTOS POR DEPARTAMENTO',
            main.title.size = 1.2,
            fontface = 2,
            fontfamily = 'Tw Cen MT Condensed',
            main.title.position = c(0.2,0.5)) +
  tm_scale_bar(size = 0.4,
               width = 0.21,
               color.dark = 'White',
               color.light = 'black',
               position = c(0.5,0.03))
  #+tm_compass(position = c(0.05,0.85))
             
# tm_shape(departamentos_montos) +
#   tmap_options(inner.margins = c(0.1,0.1, 0.02,0.01)) +   #ubicamos a la leyenda
#   tm_text('DEPARTAMEN',
#           size = 0.5,
#           fontface = 2,
#           fontfamily = 'Tw Cen MT Condensed')+
#   tm_polygons("numero",palette = "viridis")+ #Greens
#   tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
#              color.dark = "gray60", text.color = "gray60",
#              position = c("left", "top"))  +
#   #tm_borders(col = "black")+
#   tm_layout(frame=FALSE,      #Sacamos el recuadro        
#             main.title = 'CONTRATOS POR DEPARTAMENTO',
#             main.title.size = 1.2,
#             fontface = 2,
#             fontfamily = 'Tw Cen MT Condensed',
#             main.title.position = c(0.2,0.5)) +
#   tm_scale_bar(size = 0.4,
#                width = 0.21,
#                color.dark = 'White',
#                color.light = 'black',
#                position = c(0.5,0.03))
# #+tm_compass(position = c(0.05,0.85))



