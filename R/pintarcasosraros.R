
#LIMA

library(tint)
library(knitr)
library(readxl)
library(correlation)
library(tidyverse)
library(esquisse)                        #Gráficos simples sin código.
#install.packages("data.table")
library("data.table")
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
view(zonas2)
zonas_num<-arrange(zonas2,desc(num_contr))

install.packages("formattable")
library(formattable)

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"



formattable(zonas2, align =c("l","c","c","c","c", "c", "c", "c", "r"), list( ###con align alineamos
  `MONTOADJUDICADOSOLES`= color_tile(customGreen, customGreen0),
  `num_contr`= formatter("span", style = ~ style(color = ifelse(`num_contr` <= 3, "green","red")),
                    ~ icontext(ifelse(`num_contr` <= 3, "thumbs-up", "thumbs-down"),`num_contr`)))) ###(condición, dato)
  















#library(paletteer)


# head(zonas2)
# 
# b<-datatable(zonas2) %>%                          #definimos el datatable zonas2 que tiene los número de contratos
#   formatStyle(columns="num_contr", 
#               #color = 'red', 
#               background = styleInterval(c(4.9), c("white", "orange")),
#               color = styleInterval(c(4.9),c("black", 'red')))
# 
# 
# print(b)
# # 
