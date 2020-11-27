library(readxl)
library(tidyverse)
library(formattable)
library(data.table)
#library(gridExtra)

#####################################
#En esta parte trabajaremos solo con la base de datos de CONOSCE
#Los resultados serán llamados al Shiny




#I.
##################CARGADO Y LIMPIEZA############################
##############################################################
#1.1. Directorio
setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")

#1.3. Cargamos la base de datos de CONOSCE
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
contr_direc[,28]<-sapply(contr_direc[,28],redondeo)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROVEEDOR"
# #names(contr_direc)
options(scipen=999) 

#1.3. Generamos algunas variables de interés
#EN GENERAL
entidaddt_mo<-select(contr_direc, "ENTIDAD", "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")%>%
  group_by(ENTIDAD,PROVEEDOR,RUCPROVEEDOR)%>%
  summarize(MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())%>%
  arrange(desc(MONTOADJSOLES)) #%>%

entidaddt_num<-select(contr_direc, "ENTIDAD", "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")%>%
  group_by(ENTIDAD,PROVEEDOR,RUCPROVEEDOR)%>%
  summarize(MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())%>%
  arrange(desc(Contratos)) 


#PERSONAS NATURALES
  
entidaddt_pnnum<-select(contr_direc, "ENTIDAD", "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")%>%
  filter(TIPOPROVEEDOR=="Persona Natural")%>%
  group_by(PROVEEDOR,RUCPROVEEDOR)%>%
  summarize(MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())%>%
  arrange(desc(Contratos)) #%>%
#entidad_num<-head(entidad_num,5)
  

view(entidad_mo)

#II
#####################GENERAMOS TABLAS CON DF Y FORMATTABLE###############


top_prov<-as.datatable(formattable(df, align =c("c","c","c","c","c","c","c","c"), list( 
  `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                         ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
  `Sanciones`= formatter("span", style = ~ style(color = ifelse(`Sanciones` ==0 , "green","red"),font.weight = "bold")),
  `Penalidades`= formatter("span", style = ~ style(color = ifelse(`Penalidades` ==0 , "green","red"),font.weight = "bold"))
  ,`Monto` = color_bar("green")
))) 

entidad_dt_mon<-as.datatable(formattable(entidaddt_mo, align =c("c","c","c","c"), list( 
  `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                         ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
  `MONTOADJSOLES` = color_bar("red")
)))

entidad_dt_num<-as.datatable(formattable(entidaddt_num, align =c("c","c","c","c"), list( 
  `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                         ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
  `MONTOADJSOLES` = color_bar("red")
)))

entidad_dt_num
