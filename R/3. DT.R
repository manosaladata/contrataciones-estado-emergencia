library(readxl)
library(tidyverse)
library(formattable)
library(data.table)


###############################################################
#En esta parte trabajaremos solo con la base de datos de CONOSCE
#################################################################

redondeo <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=2)))

#I.
##################CARGADO Y LIMPIEZA############################
##############################################################
#1.1. Directorio
setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")

#1.3. Cargamos la base de datos de CONOSCE
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROVEEDOR"
# #names(contr_direc)
options(scipen=999) 
zonas<- select(contr_direc, "ENTIDAD_DEPARTAMENTO","MONTO_SOLES_EN_MILLONES")
#1.2
#########DEPARTAMENTOS#########################
n_dep<-zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),numero=n())%>%
  arrange(desc(numero))%>%
  as.data.frame()
n_dep[,2]<-sapply(n_dep[,2],redondeo)

monto_dep<-zonas %>%
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTO=sum(MONTO_SOLES_EN_MILLONES))%>%
  arrange(desc(MONTO))%>%
  as.data.frame()

monto_dep[,2]<-sapply(monto_dep[,2],redondeo)
names(monto_dep)[1]="DEPARTAMENTO"
names(monto_dep)[2]="MONTO(millones)"


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




#II
#####################GENERAMOS TABLAS CON DF Y FORMATTABLE###############

# top_prov<-as.datatable(formattable(df, align =c("c","c","c","c","c","c","c","c"), list( 
#   `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
#                          ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
#   `Sanciones`= formatter("span", style = ~ style(color = ifelse(`Sanciones` ==0 , "green","red"),font.weight = "bold")),
#   `Penalidades`= formatter("span", style = ~ style(color = ifelse(`Penalidades` ==0 , "green","red"),font.weight = "bold"))
#   ,`Monto` = color_bar("green")
# ))) 

entidad_dt_mon<-as.datatable(formattable(entidaddt_mo, align =c("c","c","c","c"), list( 
  `Contratos`= formatter("span", style = ~ formattable::style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                         ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
  `MONTOADJSOLES` = color_bar("red")
)))
entidad_dt_mon
entidad_dt_num<-as.datatable(formattable(entidaddt_num, align =c("c","c","c","c"), list( 
  `Contratos`= formatter("span", style = ~ formattable::style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                         ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
  `MONTOADJSOLES` = color_bar("red")
)))


#III
##########LEYES

Leyes=c("TUO de la de la Ley N° 30225","TUO de la ley 27444","Reglamento de la Ley N° 30225",
        "Decreto de Urgencia 022-2020")%>%
  as.data.frame()

names(Leyes)="NORMATIVA PERTINENTE"

# Leyes=as.datatable(formattable(Leyes,align ="c",list(
#   `NORMATIVA PERTINENTE`=color_bar("skyblue")
#   )))





