

library(readxl)
library(correlation)
library(tidyverse)
library(esquisse)                        #Gráficos simples sin código.
setwd("D:/GITHUB-PROYECTOS BEST/contrataciones-estado-emergencia/Data")

#setwd("D:/ABCN/Github/contrataciones-estado-emergencia/data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO SOLES EN MILLONES"
names(contr_direc)
options(scipen=999)                                  #Evita que salga en notación científica (exponencial).
sapply(contr_direc, class)                          #Analizamos la clase de cada columna. Ojo: FECHACONVOCATOERIA ESTÁ EN POSIXct y POSIxt q es formato de fecha

boxplot(contr_direc$"MONTO SOLES EN MILLONES")

#Veamos los montos por departamentos
zonas<- select(contr_direc, "ENTIDAD_DEPARTAMENTO","MONTO SOLES EN MILLONES")

#Ordenamos de acuerdo al número de contratos
zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise("MONTO TOTAL EN MILLONES DE SOLES"=sum(`MONTO SOLES EN MILLONES`),num_contr=n())%>%
  arrange(desc(num_contr))%>%                                    #No usar string, lo lee como tal. 
  View() # mira la data

ggplot(contr_direc, aes(x = ENTIDAD_DEPARTAMENTO)) + coord_flip()+ stat_count (width = 0.7)+ 
  geom_bar(size = 1) +
  theme_minimal()+
  labs(title = "Cantidad de contratos por departamento",
              x="Cantidad de contratos", y="Departamentos")

#Lima es el que tiene más contratos, seguido de Ancash, La Libertad, Cajamarca y San Martín.

#Ordenamos de acuerdo al monto 
zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTO_TOTAL_EN_MILLONES=sum(`MONTO SOLES EN MILLONES`),numero=n())%>%
  arrange(desc(MONTO_TOTAL_EN_MILLONES))%>% 
  View() 

#Lima es el que tiene un mayor monto en contrataciones, seguido de San Martín, Cuzco y Ancash.


boxplot(contr_direc$"MONTO SOLES EN MILLONES")
#esquisser()
