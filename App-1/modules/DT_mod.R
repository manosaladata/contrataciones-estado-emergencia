library(readxl)
library(tidyverse)
library(formattable)
library(DT)


###############################################################
#En esta parte trabajaremos solo con la base de datos de CONOSCE
#################################################################
#redondeo <- function(x, k) as.numeric(trimws(format(round(x, 2), nsmall=2)))
redondeo <- function(x) as.numeric(round(x, 2))

#I.
##################CARGADO Y LIMPIEZA############################
##############################################################
#1.1. Directorio
# setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")

#1.3. Cargamos la base de datos de CONOSCE
contr_direc <- read_excel("Data/CONOSCE_CONTRATACIONDIRECTA.xlsx")
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
  summarize(`MONTO ADJUDICADO EN SOLES (millones)`= sum(MONTO_SOLES_EN_MILLONES), CONTRATOS=n())%>%
  arrange(desc(`MONTO ADJUDICADO EN SOLES (millones)`)) #%>%

entidaddt_mo[,4]<-sapply(entidaddt_mo[,4],redondeo)

entidaddt_num<-select(contr_direc, "ENTIDAD", "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")%>%
  group_by(ENTIDAD,PROVEEDOR,RUCPROVEEDOR)%>%
  summarize(`MONTO ADJUDICADO EN SOLES (millones)`= sum(MONTO_SOLES_EN_MILLONES), CONTRATOS=n())%>%
  arrange(desc(CONTRATOS)) 

entidaddt_num[,4]<-sapply(entidaddt_num[,4],redondeo)
#View(entidaddt_num)
#PERSONAS NATURALES



#II
#####################GENERAMOS TABLAS CON DF Y FORMATTABLE###############

# top_prov<-as.datatable(formattable(df, align =c("c","c","c","c","c","c","c","c"), list( 
#   `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
#                          ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
#   `Sanciones`= formatter("span", style = ~ style(color = ifelse(`Sanciones` ==0 , "green","red"),font.weight = "bold")),
#   `Penalidades`= formatter("span", style = ~ style(color = ifelse(`Penalidades` ==0 , "green","red"),font.weight = "bold"))
#   ,`Monto` = color_bar("green")
# ))) 

entidad_dt_mon<-as.datatable(formattable(entidaddt_mo, align =c("c","c","c","c","c"), list( 
  `CONTRATOS`= color_tile("white", "red"),
  `MONTO ADJUDICADO EN SOLES (millones)` = color_tile("white","darkgreen")
)))
entidad_dt_mon
entidad_dt_num<-as.datatable(formattable(entidaddt_num, align =c("c","c","c","c","c"), list( 
  `CONTRATOS`= color_tile("white","red"),
  `MONTO ADJUDICADO EN SOLES (millones)`= color_tile("white","darkgreen")
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

#IV- PROVEEDORES
##############
contr_prove<- select(contr_direc, "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")
contr_prove<-contr_prove %>% 
  group_by(PROVEEDOR,RUCPROVEEDOR,TIPOPROVEEDOR)%>%                             #Agrupar de PROVEEDOR.Si pones solo 1 y haces el summarize con RUC, saldrÃ¡ por separado por cada RUC. Yo quiero todo junto.
  summarize(`MONTO ADJUDICADO EN SOLES (millones)` = sum(MONTO_SOLES_EN_MILLONES), CONTRATOS=n()) %>% 
  arrange(desc(CONTRATOS))%>%
  as.data.frame()
#contr_prove[,4]

contr_prove[,4]<-sapply(contr_prove[,4], redondeo)

# contr_prove<-as.datatable(formattable(contr_prove, align =c("c","c","c","c","c"), list( 
#   `CONTRATOS`= formatter("span", style = ~ formattable::style(color = ifelse(`CONTRATOS` <= 3, "green","red"),font.weight = "bold"),
#                          ~ icontext(ifelse(`CONTRATOS` <= 3, "thumbs-up", "thumbs-down"),`CONTRATOS`)),
#   `MONTO ADJUDICADO EN SOLES` = color_bar("red")
# )))

contr_prove<-as.datatable(formattable(contr_prove, align =c("c","c","c","c","c"), list(
  `CONTRATOS`=color_tile("white", "red") ,
  `MONTO ADJUDICADO EN SOLES (millones)` = color_tile("white","darkgreen")   #no bar, distorsiona el sort
)))


contr_prove

#Persona natural:
per_nat<-select(contr_direc, "ENTIDAD", "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")%>%
  filter(TIPOPROVEEDOR=="Persona Natural")%>%
  group_by(PROVEEDOR,RUCPROVEEDOR)%>%
  summarize(`MONTO ADJUDICADO EN SOLES (millones)`= sum(MONTO_SOLES_EN_MILLONES), CONTRATOS=n())%>%
  arrange(desc(CONTRATOS)) #%>%

per_nat[,3]<-sapply(per_nat[,3],redondeo)

per_nat<-as.data.frame(per_nat)
#class(per_nat$CONTRATOS[1])
#per_nat$CONTRATOS<-as.numeric(per_nat$CONTRATOS)

# per_nat<-as.datatable(formattable(per_nat, align =c("c","c","c","c","c"), list( 
#   `CONTRATOS`= formatter("span", style = ~ formattable::style(color = ifelse(`CONTRATOS` <= 3, "green","red"),font.weight = "bold"),
#                          ~ icontext(ifelse(`CONTRATOS` <= 3, "thumbs-up", "thumbs-down"),`CONTRATOS`)),
#   `MONTO ADJUDICADO EN SOLES`= color_tile("white", "red")
# ))) #title es mejor que bar porque este arruina el sort manual.

#per_na

per_nat<-as.datatable(formattable(per_nat, align =c("c","c","c","c","c"), list(
  #`RUCPROVEEDOR`= color_tile("white", "green"),
  `CONTRATOS`= color_tile("white", "red"),
  `MONTO ADJUDICADO EN SOLES (millones)`= color_tile("white", "darkgreen")
)))

per_nat

#per_nat[,4]<-as.numeric(per_nat[, 4])    #formattable




monto_depUI<-function(id) {tagList(DTOutput(NS(id,"monto_dep")
))}

monto_depServer<-function(id){
  moduleServer(id, function(input, output, session) {
    output$monto_dep<-renderDT(monto_dep)
  })}

per_natUI<-function(id) {tagList(DTOutput(NS(id,"per_nat")
))}

per_natServer<-function(id){
  moduleServer(id, function(input, output, session) {
    output$per_nat<-renderDT(per_nat)
  })}


