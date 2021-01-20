library(tidyverse)  
library(readxl)
setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")

########DATOS############################
#Usamos el workspace de apis para evitar el problema de las fechas:
load("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data/workspace_ruc_final.RData")
remove(list=setdiff(ls(), "total"))  #nos quedamos con la variable total
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#data_api <- read_excel("DATOS_api.xlsx")

#ORDENAMOS
data_api<-total
data_webs <- read_excel("DATOS_Webscr.xlsx")
names(data_webs)
data_api_120<-data_api[c(1:120),]
DATA_CONSOLIDADA_120<-cbind(data_api_120,data_webs[,5:6])
################################################
names(DATA_CONSOLIDADA_120)
#####################REVISIÓN#######################
View(DATA_CONSOLIDADA_120[sample(nrow(DATA_CONSOLIDADA_120), 10),]) 
#########TODOK OK AL 18/11/2020####################


#Exportamos (trabajaremos con el workspace)
#write.table(DATA_CONSOLIDADA_120, "DATA_CONSOLIDADA_120", sep= ";",row.names=FALSE)




#lapply(data.frame(unlist(DATA_CONSOLIDADA_520[3])), function(x)as.Date(as.numeric(x), origin = "1899-12-30"))
       #DATA_CONSOLIDADA_520$DATA_CONSOLIDADA_520$`FECHA DE INSCRIPCIÓN`<-lapply(DATA_CONSOLIDADA_520$`FECHA DE INSCRIPCIÓN`,  ##NO FUNCIONA
    #function(x)as.Date(as.numeric(x), origin = "1899-12-30"))




