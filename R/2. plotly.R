
library(readxl)
library(tidyverse)
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
  summarize(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n()) %>%
  arrange(MONTOADJUDICADOSOLES) 

tipo_convo_n<-objetos %>%# 
  group_by(RUBROS) %>% 
  summarize(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n()) %>%
  arrange(num_contr) 
#view(tipo_convo)
#tipo_convo

fechas_convo<-objetos %>%# 
  group_by(FECHACONVOCATORIA) %>% 
  summarize(RUBROS,num_contr=n()) %>%
  arrange(FECHACONVOCATORIA)

rubros_funnel<-plot_ly()  %>%
  add_trace(
    type = "funnel",
    x = tipo_convo$MONTOADJUDICADOSOLES,
    y = tipo_convo$RUBROS,
    #textposition = "inside",
    textinfo = "value+percent total",
    opacity = 0.65,
    marker = list(color = c("tan", "lightsalmon", "tan", "teal", "silver","silver","silver",
                            "silver","red","red","red","red"),
                  line = list(width = c(4, 2, 2, 3, 1, 1,1,1,1,1,1,1,1), color = c("wheat", "wheat", "wheat", "wheat", "wheat",
                                                                                   "wheat", "blue", "wheat", "wheat","wheat","wheat",
                                                                                   "wheat"))))%>%
  layout(#title="RUBROS POR MONTO",
    yaxis = list(categoryarray = c("a","b","c",
                                        "d","e","f","g",
                                        "h","i",
                                        "j","k","l")))

rubros_funnel_n<-plot_ly()  %>%
  add_trace(
    type = "funnel",
    x = tipo_convo_n$num_contr,
    y = tipo_convo_n$RUBROS,
    #textposition = "inside",
    textinfo = "value+percent total",
    opacity = 0.65,
    marker = list(color = c("tan", "lightsalmon", "tan", "teal", "silver","silver","silver",
                            "silver","red","red","red","red"),
                  line = list(width = c(4, 2, 2, 3, 1, 1,1,1,1,1,1,1,1), color = c("wheat", "wheat", "wheat", "wheat", "wheat",
                                                                                   "wheat", "blue", "wheat", "wheat","wheat","wheat",
                                                                                   "wheat"))))%>%
  layout(#title="RUBROS POR MONTO",
    yaxis = list(categoryarray = c("a","b","c",
                                   "d","e","f","g",
                                   "h","i",
                                   "j","k","l")))

rubros_funnel
rubros_funnel_n
