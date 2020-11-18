##########
#DUDA
##########

library(jsonlite)
library(httr)
library(tidyverse)  
library(readxl)
httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

##########################################
############DATA INSUMO##################
##########################################

setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROV"
options(scipen=999)                     
proveedores<- select(contr_direc, "PROVEEDOR","ENTIDAD", "RUCPROV", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")
proveedores2_num<-group_by(proveedores,PROVEEDOR,RUCPROV)
proveedores2_num<-summarize(proveedores2_num,MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())
proveedores2_num<-arrange(proveedores2_num,desc(Contratos))
data<-as.data.frame(proveedores2_num[,2])
data1<-as.data.frame(proveedores2_num[c(1:200),2])
data2<-as.data.frame(proveedores2_num[c(201:400),2])
data3<-as.data.frame(proveedores2_num[c(401:600),2])
data4<-as.data.frame(proveedores2_num[c(601:800),2])
data5<-as.data.frame(proveedores2_num[c(801:1000),2])
data6<-as.data.frame(proveedores2_num[c(1001:1200),2])
data7<-as.data.frame(proveedores2_num[c(1201:1400),2])
data8<-as.data.frame(proveedores2_num[c(1401:1600),2])
data9<-as.data.frame(proveedores2_num[c(1601:1800),2])
data10<-as.data.frame(proveedores2_num[c(1801:2000),2])
data11<-as.data.frame(proveedores2_num[c(2001:2200),2])
data12<-as.data.frame(proveedores2_num[c(2201:2400),2])
data13<-as.data.frame(proveedores2_num[c(2401:2600),2])
data14<-as.data.frame(proveedores2_num[c(2601:2769),2]) #Aquí hay 169 datos (el primero se cuenta)
#data14

data
########################################
###########FUNCIONES###################
########################################
 
sunat<- function(ruc){
  if(nchar(ruc)!=11){a<-NA} 
  else{
  web<-paste("https://api.sunat.cloud/ruc/",ruc,sep = "", collapse = NULL)
  jsn<- GET(web)
  data<-fromJSON(content(jsn, type="text", encoding = "UTF-8"))}
}

razon_social<-function(ruc){
  data<-sunat(ruc)
  if(inherits(data, "try-error")){
    next} else{data}
  if (is.na(data)){a<-NA} else {
  b<-data[["razon_social"]]}}



trabajo<-function(ruc){
  data<-sunat(ruc)
  if (is.na(data)){a<-NA} else{
  empleados<-data[["empleados"]]
  trabajadores_agosto<-empleados[["2020-08"]][["trabajadores"]]
}}

fecha_inscripcion<-function(ruc){
  data<-sunat(ruc)
  if (is.na(data)){a<-NA} else{
  fecha_inscripcion<-data[["fecha_inscripcion"]]}}

representante<-function(ruc){
  data<-sunat(ruc)
  if (is.na(data)){a<-NA} else{   #Si no saldrá dimensión incorrecta, pues al leer como NA buscará el respresentante y no habrá
  representante_legal<-data[["representante_legal"]]
  representante_legal_name<-representante_legal[[1]][["nombre"]]
  }}


###############################################################
#############OBTENCIÓN DE DATOS DEL API DE SUNAT###############
###############################################################
#####SEPARAMOS LA DATA PARA QUE EVITAR PROBLEMAS DE INTERRUPCIÓN
#RAZÓN SOCIAL
# raz_soc1<-sapply(data1$RUCPROV, razon_social)#Es necesario usar $ con el nombre de la columna.
# raz_soc2<-sapply(data2$RUCPROV, razon_social)
# raz_soc3<-sapply(data3$RUCPROV, razon_social)
# raz_soc4<-sapply(data4$RUCPROV, razon_social)
# raz_soc5<-sapply(data5$RUCPROV, razon_social)
# raz_soc6<-sapply(data6$RUCPROV, razon_social)
# raz_soc7<-sapply(data7$RUCPROV, razon_social)
# raz_soc8<-sapply(data8$RUCPROV, razon_social)
# raz_soc9<-sapply(data9$RUCPROV, razon_social)
# raz_soc10<-sapply(data10$RUCPROV, razon_social)
# raz_soc11<-sapply(data11$RUCPROV, razon_social)
# raz_soc12<-sapply(data12$RUCPROV, razon_social)
# raz_soc13<-sapply(data13$RUCPROV, razon_social)
# raz_soc14<-sapply(data14$RUCPROV, razon_social)

###############################
###############TRABAJADORES###
##############################
trabajadores1<-sapply(data1$RUCPROV, trabajo)#Es necesario usar $ con el nombre de la columna.
trabajadores2<-sapply(data2$RUCPROV, trabajo)
trabajadores3<-sapply(data3$RUCPROV, trabajo)
trabajadores4<-sapply(data4$RUCPROV, trabajo)
trabajadores5<-sapply(data5$RUCPROV, trabajo)
trabajadores6<-sapply(data6$RUCPROV, trabajo)
trabajadores7<-sapply(data7$RUCPROV, trabajo)
trabajadores8<-sapply(data8$RUCPROV, trabajo)
trabajadores9<-sapply(data9$RUCPROV, trabajo)
trabajadores10<-sapply(data10$RUCPROV, trabajo)
trabajadores11<-sapply(data11$RUCPROV, trabajo)
trabajadores12<-sapply(data12$RUCPROV, trabajo)
trabajadores13<-sapply(data13$RUCPROV, trabajo)
trabajadores14<-sapply(data14$RUCPROV, trabajo)

#SI TRANSFORMAMOS DIRECTAMENTE A DATA FRAME SE PERDERÁN LOS NULL VALUES
#ENTONCES, PRIMERO CAMBIAMOS LOS VALORES NULL DE TODAS LAS LISTAS DE TRABAJADORES:
list2env(lapply(mget(paste0("trabajadores", 1:14)), function(x) {
  x[sapply(x, is.null)] <- "ND-ANUAL"
  x}), 
  .GlobalEnv)

#COMBINAMOS LISTAS
Trabajadores_list<-c(trabajadores1, trabajadores2, trabajadores3, trabajadores4, trabajadores5,
         trabajadores6,trabajadores7,trabajadores8,trabajadores9,trabajadores10,trabajadores11,
         trabajadores12,trabajadores13,trabajadores14)

#AHORA YA PODEMOS CONVERTIRLOS A DATA FRAME
Trabajadores<-data.frame(unlist(Trabajadores_list))
View(Trabajadores)
Trabajadores



##########################################
####FECHA DE INSCRIPCIÓN###################
##########################################

fecha_ins1<-sapply(data1$RUCPROV, fecha_inscripcion)#Es necesario usar $ con el nombre de la columna.
fecha_ins2<-sapply(data2$RUCPROV, fecha_inscripcion)
fecha_ins3<-sapply(data3$RUCPROV, fecha_inscripcion)
fecha_ins4<-sapply(data4$RUCPROV, fecha_inscripcion)
fecha_ins5<-sapply(data5$RUCPROV, fecha_inscripcion)
fecha_ins6<-sapply(data6$RUCPROV, fecha_inscripcion)
fecha_ins7<-sapply(data7$RUCPROV, fecha_inscripcion)
fecha_ins8<-sapply(data8$RUCPROV, fecha_inscripcion)
fecha_ins9<-sapply(data9$RUCPROV, fecha_inscripcion)
fecha_ins10<-sapply(data10$RUCPROV, fecha_inscripcion)
fecha_ins11<-sapply(data11$RUCPROV, fecha_inscripcion)
fecha_ins12<-sapply(data12$RUCPROV, fecha_inscripcion)
fecha_ins13<-sapply(data13$RUCPROV, fecha_inscripcion)
fecha_ins14<-sapply(data14$RUCPROV, fecha_inscripcion)
length(fecha_ins1)

#Unimos los characteres en orden
fechas_sum<-c(fecha_ins1,fecha_ins2,fecha_ins3,fecha_ins4,fecha_ins5,
              fecha_ins6,fecha_ins7,fecha_ins8,fecha_ins9,fecha_ins10,
              fecha_ins11,fecha_ins12,fecha_ins13,fecha_ins14)
length(fechas_sum)  #Comprobamos que tenga el ttal número

#CREAMOS UN DATA FRAME
#Unimos 

####CONVERTIMOS A DATAFRAME
fechas<-data.frame(fechas_sum)

############################################
##########UNIR VARIABLES CREADAS##########
#########################################
#Uniendo al data frame general:
#UNIMOS AL DATA FRAME GENERAL (no usaremos merge pues no hay columna común y los datos estan en orden)
total<-cbind(proveedores2_num,Trabajadores,fechas)
total<-total[,c(1,2,6,5,3,4)]   #Reordenamos
#names(total)
#CAMBIAMOS EL NE POR ND-MES (NO DECLARACIÓN EN EL ÚLTIMO MES DE ANÁLISIS)
names(total)<-c("PROVEEDOR","RUCPROV","FECHA DE INSCRIPCIÓN", "NUMTRABAJADORES_AGOSTO","MONTADJSOLES","Contratos")
#names(total)
total[total=="NE"] <-"ND-MES"
View(total)

#GUARDAMOS NUEVA DATA:
write.table(x=total, file="DATOS_api", sep= ";",row.names=FALSE)




