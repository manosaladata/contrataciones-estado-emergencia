
#LIMA

library(tint)
library(knitr)
library(readxl)
library(correlation)
library(tidyverse)
library(esquisse)                        #Gráficos simples sin código.
library(DT)
setwd("D:/GITHUB-PROYECTOS BEST/contrataciones-estado-emergencia/Data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROV"
#names(contr_direc)
options(scipen=999)                                  #Evita que salga en notación científica (exponencial).
#sapply(contr_direc, class)                          #Analizamos la clase de cada columna. Ojo: FECHACONVOCATOERIA ESTÁ EN POSIXct y POSIxt q es formato de fecha

boxplot(contr_direc$"MONTO_SOLES_EN_MILLONES")

#ANÁLISIS DEPARTAMENTAL
#DE ACUERDO AL NÚMERO DE CONTRATOS
lima<-filter(contr_direc,ENTIDAD_DEPARTAMENTO=="LIMA")
#zonas_fil<- select(lima, "ENTIDAD_DEPARTAMENTO","MONTO_SOLES_EN_MILLONES")
zonas2<-group_by(lima,PROVEEDOR,RUCPROV)
zonas2<-summarise(zonas2,MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),num_contr=n())
zonas_num<-arrange(zonas2,desc(num_contr))
zonas_num<-head(zonas_num,5)
knitr::kable(
  zonas_num, caption = 'Departamentos de acuerdo al número de contratos.'
)
#library(paletteer)


head(zonas2)

b<-datatable(zonas2) %>%                          #definimos el datatable zonas2 que tiene los número de contratos
  formatStyle(columns="num_contr", 
              #color = 'red', 
              background = styleInterval(c(4.9), c("white", "orange")),
              color = styleInterval(c(4.9),c("black", 'red')))


print(b)
