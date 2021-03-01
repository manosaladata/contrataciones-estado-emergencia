## code to prepare `DATASET` dataset goes here
#En dataraw, como el nombre indica generamos la data
packs=c("opencontracts","tidyverse","formattable","readxl","data.table","rsconnect","tmap",
        "leaflet")
invisible(lapply(packs,library,character=T))
data120<-read.csv("DATA_CONSOLIDADA_120.csv", sep= ";")
datatotal<-read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")

tabla120<-as.datatable({formattable(data120, align =c("c","c","c","c","c","c","c","c"), list( ###con align alineamos: ",align =c("l","c","c","c","c", "c", "c", "c", "r")"
  `Contratos`= formatter("span", style =  ~ formattable::style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                         ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
  `Sanciones`= formatter("span", style = ~ formattable::style(color = ifelse(`Sanciones` ==0 , "green","red"),font.weight = "bold")),
  `Penalidades`= formatter("span", style =  ~ formattable::style(color = ifelse(`Penalidades` ==0 , "green","red"),font.weight = "bold"))
  ,`Monto` = color_bar("green")

))

})

usethis::use_data(tabla120, overwrite = TRUE)
# save(data120, file = "data120.RData")
#save(datatotal, file = "datatotal.RData")


# library(sf)    #Permite relación geos
# library(tidyverse)
# library(ggrepel)
# library(tmap)
# library(readxl)
# library(leaflet)
# library("leaflet.extras")
#
# #https://www.geogpsperu.com/2018/02/limite-departamental-politico-shapefile.html
#
# #####Directorio y Data##############################################
# #setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")
#
# contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
# contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
# names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
# names(contr_direc)
# options(scipen=999)
#
#
#
# ################Usamos el .shp#############################################
#
#
# departamentos<-st_read("DEPARTAMENTOS.shp")            ###OJO, SE REQUIEREN TODOS LOS ARCHIVOS
#
# ggplot(data = departamentos) +
#   geom_sf()
#
# ggplot(data = departamentos %>%
#          filter(DEPARTAMEN=="LIMA")) +
#   geom_sf()
#
#
#
# ########COLOCANDO NOMBRES##########
# departamentos <- departamentos %>% mutate(centroid = map(geometry, st_centroid),
#                                           coords = map(centroid,st_coordinates),
#                                           coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords,2))
#
#
#
#
#
# #Veamos los montos por departamentos
# zonas<- select(contr_direc, "ENTIDAD_DEPARTAMENTO","MONTO_SOLES_EN_MILLONES")
# names(zonas)[1]<-"DEPARTAMEN"
#
#
# zonas <- group_by(zonas, DEPARTAMEN)
# zonas<-summarise(zonas,MONTO=sum(MONTO_SOLES_EN_MILLONES),numero=n())
# zonas<-arrange(zonas,desc(numero))
# zonas_numero<-arrange(zonas,desc(numero))
# df_zonas<-as.data.frame(zonas)
# df_zonas_nolima<-df_zonas[-1,]
# ##########UNIMOS PARA MONTOS Y NÚMEROS###################
# departamentos_montos <- departamentos%>%
#   left_join(zonas)
#
# departamentos_numero <- departamentos%>%
#   left_join(zonas_numero)
#
#
# map_mon<-tm_shape(departamentos_montos) +
#   tmap_options(bg.color = "green",inner.margins = c(0.1,0.1, 0.02,0.01)) +   #ubicamos a la leyenda
#   tm_text('DEPARTAMEN',
#           size = 0.5,
#           fontface = 2,
#           fontfamily = 'Tw Cen MT Condensed')+
#   #tm_polygons("MONTO",palette = "viridis")+ #Greens
#   tm_polygons("MONTO", title = "Millones de soles", style = "fixed",
#               breaks = c(0, 30, 50, 80, 100, 1200, Inf),
#               #textNA = "Lima",
#               colorNA = "green",   # <-------- color for NA values
#               palette = "viridis")+
#   tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
#              color.dark = "gray60", text.color = "gray60",
#              position = c("left", "top"))  +
#   #tm_borders(col = "black")+
#   tm_layout(frame=FALSE,      #Sacamos el recuadro
#             main.title = 'MONTOS POR DEPARTAMENTO(mill)',
#             main.title.size = 0.8,
#             fontface = 2,
#             fontfamily = 'Tw Cen MT Condensed',
#             main.title.position = c(0.12,0.5)) +
#   tm_scale_bar(size = 0.4,
#                width = 0.21,
#                color.dark = 'White',
#                color.light = 'black',
#                position = c(0.5,0.03))
#
# map_mon<-tmap_leaflet(map_mon)
#
# #USAR:
# # addProviderTiles(map_mon, providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE),
# #                  ) %>%
# #   addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE))
#
# park_card <- function (park_Name, park_Code, park_State, park_Acres, park_Latitude, park_Longitude) {
#
#   card_content <- paste0("<style>div.leaflet-popup-content {width:auto !important;}</style>",
#                          "<link rel='stylesheet' href='https://use.fontawesome.com/releases/v5.7.1/css/all.css' integrity='sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr' crossorigin='anonymous'>",
#                          "<table style='width:100%;'>",
#                          "<tr>",
#                          "<th><b><h2 style='text-align: left;'>",park_Name,"</h2></b></th>",
#                          "<th><img style = 'border:1px solid black;' src='https://www.crwflags.com/art/states/",park_State,".gif' alt='flag' title='Flag of ",state.name[match(park_State,state.abb)]," ' width=80></th>",
#                          "</tr>",
#                          "</table>",
#                          "<div class='flip-card'>",
#                          "<div class='flip-card-inner'>",
#                          "<div class='flip-card-front'>",
#                          "<table style='width:100%;'>",
#                          "<tr>",
#                          "<td colspan='2'>",park_image(park_Name),"</td>",
#                          "</tr>",
#                          "<tr>",
#                          "<td style='padding: 5px;'><h4><b>Code: </b>",park_Code,"</h4></td>",
#                          "<td style='padding: 5px;'><h4><b>Acres: </b>",format(park_Acres, big.mark = ' '),"</h4></td>",
#                          "</tr>",
#                          "<tr>",
#                          "<td style='padding: 5px;'><h4><b>Latitude: </b>",park_Latitude,"</h4></td>",
#                          "<td style='padding: 5px;'><h4><b>Longitude: </b>",park_Longitude,"</h4></td>",
#                          "</tr>",
#                          "</table>",
#                          "</div>",
#                          "<div class='flip-card-back'>",
#                          "<h3>Media links</h3> ",
#                          "<hr>",
#                          "<table style='width:80%;'>",
#                          "<tr>",
#                          "<td style='text-align: left; padding-left: 25px;'><h4>Official page:</h4></td>",
#                          "<td><a style='color:white;' href='https://www.nps.gov/",park_Code,"/index.htm' target='_blank'><i class='fas fa-globe fa-2x'></i></a></td>",
#                          "</tr>",
#                          "<tr>",
#                          "<td style='text-align: left; padding-left: 25px;'><h4>Wikipedia page:<h4></td>",
#                          "<td><a style='color:white' href='https://en.wikipedia.org/wiki/",park_Name,"' target='_blank'><i class='fab fa-wikipedia-w fa-2x'></i></td></p>",
#                          "</tr>",
#                          "<tr>",
#                          "<td style='text-align: left; padding-left: 25px;'><h4>Pictures:<h4></td>",
#                          "<td><a style='color:white' href='https://www.google.com/search?tbm=isch&q=",park_Name,"&tbs=isz:m' target='_blank'><i class='fas fa-images fa-2x'></i></a></td>",
#                          "</tr>",
#                          "<tr>",
#                          "<td style='text-align: left; padding-left: 25px;'><h4>Youtube videos:<h4></td>",
#                          "<td><a style='color:white' href='https://www.youtube.com/results?search_query=",park_Name,"' target='_blank'><i class='fab fa-youtube fa-2x'></i></td>",
#                          "</tr>",
#                          "</table>",
#                          "</div>",
#                          "</div>",
#                          "</div>"
#   )
#
#   return(card_content)
#
# }
#
#
# map_mon<-addProviderTiles(map_mon,providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
#   addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
#   addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
#   addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
#   addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
#   addFullscreenControl() %>%
#   addLayersControl(
#     baseGroups = c("Stamen Watercolor","Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery"),
#     position = c("topleft"),
#     options = layersControlOptions(collapsed = TRUE)
#   )
# # code to load the park card once the click event on a marker is intercepted
#
# usethis::use_data(data120, overwrite = TRUE)  #creará una carpeta llamada "data" y guardará el .Rdata
# usethis::use_data(map_mon, overwrite = TRUE)

