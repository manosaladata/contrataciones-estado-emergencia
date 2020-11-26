library(readxl)
library(correlation)
library(tidyverse)
library(esquisse)                        #GrÃ¡ficos simples sin cÃ³digo.
library(plotly)

setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")


#setwd("D:/ABCN/Github/contrataciones-estado-emergencia/data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#contr_direc<-na.omit(contr_direc)                       No usar, sino se irÃ¡ casi toda la base se elimina.
names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)
options(scipen=999)                                  #Evita que salga en notaciÃ³n cientÃ­fica (exponencial).
sapply(contr_direc, class)  


######ANÁLISIS DE RUBROS###########
objetos<- select(contr_direc, "OBJETOCONTRACTUAL","PROVEEDOR","RUCPROVEEDOR", "FECHACONVOCATORIA",
                "TIPOPROVEEDOR","RUBROS","MONTO_SOLES_EN_MILLONES")

tipo_convo<-objetos %>%# 
  group_by(RUBROS) %>% 
  summarize(num_contr=n()) %>%
  arrange(num_contr) 

tipo_convo

fechas_convo<-objetos %>%# 
  group_by(FECHACONVOCATORIA) %>% 
  summarize(RUBROS,num_contr=n()) %>%
  arrange(FECHACONVOCATORIA)

rubros_funnel<-plot_ly()  %>%
  add_trace(
    type = "funnel",
    x = tipo_convo$num_contr,
    y = tipo_convo$RUBROS,
    #textposition = "inside",
    textinfo = "value+percent total",
    opacity = 0.65,
    marker = list(color = c("tan", "lightsalmon", "tan", "teal", "silver","silver","silver",
                            "silver","red","red","red","red"),
                  line = list(width = c(4, 2, 2, 3, 1, 1,1,1,1,1,1,1,1), color = c("wheat", "wheat", "wheat", "wheat", "wheat",
                                                                                   "wheat", "blue", "wheat", "wheat","wheat","wheat",
                                                                                   "wheat"))))%>%
  layout(yaxis = list(categoryarray = c("a","b","c",
                                        "d","e","f","g",
                                        "h","i",
                                        "j","k","l")))

rubros_funnel
