

library(readxl)
library(correlation)
library(tidyverse)
library(esquisse)                        #GrÃ¡ficos simples sin cÃ³digo.
setwd("D:/GITHUB-PROYECTOS BEST/contrataciones-estado-emergencia/Data")

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

zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),numero=n())%>%
  arrange(desc(MONTOADJUDICADOSOLES))%>% 
  View() 

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

objetos<- select(contr_direc, "OBJETOCONTRACTUAL","PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")
#names(contr_direc)

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




boxplot(contr_direc$"MONTO_SOLES_EN_MILLONES")
#esquisser()
