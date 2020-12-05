

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
sapply(contr_direc, class)                          #Analizamos la clase de cada columna. Ojo: FECHACONVOCATOERIA ESTÃ EN POSIXct y POSIxt q es formato de fecha

boxplot(contr_direc$"MONTO_SOLES_EN_MILLONES")

#Veamos los montos por departamentos
zonas<- select(contr_direc, "ENTIDAD_DEPARTAMENTO","MONTO_SOLES_EN_MILLONES")

#Ordenamos de acuerdo al nÃºmero de contratos
zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n())%>%
  arrange(desc(num_contr))%>%                                    #No usar string, lo lee como tal. 
  View() # mira la data

ggplot(contr_direc, aes(x = ENTIDAD_DEPARTAMENTO)) + coord_flip()+ stat_count (width = 0.7)+ 
  geom_bar(size = 1) +
  theme_minimal()+
  labs(title = "Cantidad de contratos por departamento",
              x="Cantidad de contratos", y="Departamentos")

#Lima es el que tiene mÃ¡s contratos, seguido de Ancash, La Libertad, Cajamarca y San MartÃ­n.

#Ordenamos de acuerdo al monto 

monto_dep<-zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),numero=n())%>%
  arrange(desc(MONTOADJUDICADOSOLES))%>%
  as.data.frame()


  
montos_dep<-ggplot(monto_dep, aes(x =MONTOADJUDICADOSOLES, y=ENTIDAD_DEPARTAMENTO))+ 
  geom_bar(stat="identity", position="dodge")

ggplot(monto_dep, aes(x =MONTOADJUDICADOSOLES))
monto_dep[2,3]
#Lima es el que tiene un mayor monto en contrataciones, seguido de San MartÃ­n, Cuzco y Ancash.


#Veamos a los proveedores: 

contr_prove<- select(contr_direc, "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")
contr_prove %>% 
  group_by(PROVEEDOR,RUCPROVEEDOR)%>%                             #Agrupar de PROVEEDOR.Si pones solo 1 y haces el summarize con RUC, saldrÃ¡ por separado por cada RUC. Yo quiero todo junto.
  summarize(MONTOADJUDICADOSOLES = sum(MONTO_SOLES_EN_MILLONES), veces=n()) %>% 
  arrange(desc(MONTOADJUDICADOSOLES))%>%
  View()

#Solo el caso de proveedores personas naturales:
   #ORDEN POR NÃMERO DE CONTRATOS
contr_prove %>% 
  filter(TIPOPROVEEDOR %in% "Persona Natural") %>%
  group_by(PROVEEDOR,RUCPROVEEDOR)%>% 
  summarize(MONTOADJUDICADOSOLES = sum(MONTO_SOLES_EN_MILLONES), contratos=n()) %>% 
  arrange(desc(contratos))%>%
  View()


#Veamos los objetos del contrato:

objetos<- select(contr_direc, "OBJETOCONTRACTUAL","PROVEEDOR","RUCPROVEEDOR", "FECHACONVOCATORIA",
                 "TIPOPROVEEDOR","RUBROS","MONTO_SOLES_EN_MILLONES")
#names(contr_direc)

#POR RUBROS:
objetos %>%# 
  group_by(RUBROS) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n())%>%
  arrange(desc(num_contr))%>%                                    
  View() 

#Por el nÃºmero de contratos
objetos %>%# 
  group_by(OBJETOCONTRACTUAL) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n())%>%
  arrange(desc(num_contr))%>%                                    
  View() 

#Por los montos
objetos %>%# 
  group_by(OBJETOCONTRACTUAL) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n())%>%
  arrange(desc(MONTOADJUDICADOSOLES))%>%                                    
  View()

#Â¿QuÃ© tipo de objeto contrataba cada proveedor?
#Por el nÃºmero de contratos
objetos %>%# 
  group_by(PROVEEDOR,RUCPROVEEDOR,OBJETOCONTRACTUAL) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n())%>%
  arrange(desc(num_contr))%>%                                    
  View() 

#Por los montos
objetos %>%# 
  group_by(PROVEEDOR,RUCPROVEEDOR,OBJETOCONTRACTUAL) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n())%>%
  arrange(desc(MONTOADJUDICADOSOLES))%>%                                    
  View() 

#Por nÃºmero en el caso de PERSONAS NATURALES

objetos %>% 
  filter(TIPOPROVEEDOR %in% "Persona Natural") %>%
  group_by(PROVEEDOR,RUCPROVEEDOR,OBJETOCONTRACTUAL,)%>% 
  summarize(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n()) %>% 
  arrange(desc(num_contr))%>%
  View()

#Por fecha y tipo
tipo_convo<-objetos %>%# 
  group_by(RUBROS) %>% 
  summarize(num_contr=n()) %>%
  arrange(num_contr) 

tipo_convo

fechas_convo<-objetos %>%# 
  group_by(FECHACONVOCATORIA) %>% 
  summarize(RUBROS,num_contr=n()) %>%
  arrange(FECHACONVOCATORIA)

View(fechas_convo)
#1
fechas_convo_vehi<-objetos %>%#
  filter(RUBROS %in% "ADQUISICIÓN Y ALQUILER DE VEHÍCULOS")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_vehi=n()) %>%
  arrange(FECHACONVOCATORIA)
#2
fechas_convo_aloj<-objetos %>%#
  filter(RUBROS %in% "ALOJAMIENTO TEMPORAL Y ALIMENTACIÓN")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_aloj=n()) %>%
  arrange(FECHACONVOCATORIA)
#3
fechas_convo_vive<-objetos %>%#
  filter(RUBROS %in% "CANASTA DE VIVERES")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_vive=n()) %>%
  arrange(FECHACONVOCATORIA)
#4
fechas_convo_lim_fumi<-objetos %>%#
  filter(RUBROS %in% "LIMPIEZA Y FUMIGACIÓN" )%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_lim_fumi=n()) %>%
  arrange(FECHACONVOCATORIA)
#5
fechas_convo_equi_med<-objetos %>%#
  filter(RUBROS %in% "MATERIAL Y EQUIPO MEDICO" )%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_equi_med=n()) %>%
  arrange(FECHACONVOCATORIA)
#6.
fechas_convo_medi<-objetos %>%#
  filter(RUBROS %in% "MEDICAMENTOS ")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_medi=n()) %>%
  arrange(FECHACONVOCATORIA)

#7
fechas_obra_consult<-objetos %>%#
  filter(RUBROS %in% "OBRAS, CONSTRUCCIONES Y CONSULTORIAS ")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_consult=n()) %>%
  arrange(FECHACONVOCATORIA)

#8.
fechas_convo_otros_alim<-objetos %>%#
  filter(RUBROS %in% "OTROS ALIMENTOS")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(num_contr_otros_alim=n()) %>%
  arrange(FECHACONVOCATORIA)


#9
fechas_convo_otros_bie<-objetos %>%#
  filter(RUBROS %in% "OTROS BIENES")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(RUBROS,num_contr_otros_bien=n()) %>%
  arrange(FECHACONVOCATORIA)

#10

fechas_convo_otros_ser<-objetos %>%#
  filter(RUBROS %in% "OTROS SERVICIOS")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(RUBROS,num_contr_otros_ser=n()) %>%
  arrange(FECHACONVOCATORIA)
#11

fechas_convo_aseo_lim<-objetos %>%#
  filter(RUBROS %in% "PROD. ASEO Y LIMPIEZA")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(RUBROS,num_contr_aseo_lim=n()) %>%
  arrange(FECHACONVOCATORIA)

#12
fechas_convo_segu<-objetos %>%#
  filter(RUBROS %in% "SEGUROS")%>%
  group_by(FECHACONVOCATORIA) %>%
  summarize(RUBROS,num_contr_segu=n()) %>%
  arrange(FECHACONVOCATORIA)

ggplot(fechas_convo, aes(x = FECHACONVOCATORIA, y = num_contr)) + 
  geom_line(aes(color = RUBROS, linetype = RUBROS)) 

ggplot(tipo_convo,
       aes(x = factor(""), fill = RUBROS) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")


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




# ggplot(fechas_convo, aes(x=FECHACONVOCATORIA))+
#   geom_line(aes(y = fechas_convo_vehi), color = "darkred")+
#   geom_line(aes(y = fechas_convo_aloj), color = "red") 


  


boxplot(contr_direc$"MONTO_SOLES_EN_MILLONES")
#esquisser()
