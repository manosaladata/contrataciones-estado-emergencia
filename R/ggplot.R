########GRÁFICOS CON GGPLOT##################
library(readxl)
library(correlation)
library(tidyverse)
library(esquisse)                        #GrÃ¡ficos simples sin cÃ³digo.
library(plotly)

#1. 
###########################################################
#########################DATA##############################
setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")


#setwd("D:/ABCN/Github/contrataciones-estado-emergencia/data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROVEEDOR"
options(scipen=999) 

#2. 
##################################################
###################FILTRACIÓN#####################
zonas<- select(contr_direc, "ENTIDAD_DEPARTAMENTO","MONTO_SOLES_EN_MILLONES")

#2.1
#########DEPARTAMENTOS#########################
#2.1.1 NÚMERO

n_dep<-zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),numero=n())%>%
  arrange(desc(numero))%>%
  as.data.frame()

num_dep<-ggplot(n_dep, aes(x =numero, y=ENTIDAD_DEPARTAMENTO))+ 
  geom_bar(stat="identity", position="dodge",fill="white",col="steelblue")+
  labs(title="NÚMERO DE CONTRATOS ADJUDICADOS POR DEPARTAMENTO", 
       x="Número de contratos", y="Departamentos", caption="Manos a la data")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#2.1.2 MONTO

monto_dep<-zonas %>%# 
  group_by(ENTIDAD_DEPARTAMENTO) %>% 
  summarise(MONTOADJUDICADOSOLES=sum(MONTO_SOLES_EN_MILLONES),numero=n())%>%
  arrange(desc(MONTOADJUDICADOSOLES))%>%
  as.data.frame()

montos_dep<-ggplot(monto_dep, aes(x =ENTIDAD_DEPARTAMENTO, y=MONTOADJUDICADOSOLES))+ 
  geom_bar(stat="identity", position="dodge", fill="white",col="steelblue")+
  labs(title="MONTOS DE CONTRATOS ADJUDICADOS POR DEPARTAMENTO", 
       subtitle="(en millones de soles)", y="Montos", x="Departamentos", caption="Manos a la data")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#3.1.  
########ENTIDADES TOP 10 HASTA INICIOS DE AGOSTO###################
#3.1.1 NÚMERO

entidad_n<- select(contr_direc, "ENTIDAD", "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")%>%
  group_by(ENTIDAD)%>%
  summarize(MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())%>%
  arrange(desc(Contratos))%>%
  head(10)

#PROYECTO ESPECIAL PARA LA PREPARACION Y DESARROLLO DE LOS XVIII JUEGOS PANAMERICANOS DEL 2019
#Nombre muy largo, arruina el gráfico, lo corregimos:
entidad_n[10,1]<-"PROYECTO -XVIII JUEGOS PANAMERICANOS DEL 2019"

entidad_num<-ggplot(entidad_n, aes(x=Contratos,y=ENTIDAD))+
  geom_bar(stat="identity", position="dodge", fill="white",col="steelblue")+
  labs(title="TOP 10 DE ENTIDADES CON MÁS CONTRATOS", 
       y="Entidad", x="Número de contratos", caption="Manos a la data")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#entidad_num

#3.1.2
entidad_mo<- select(contr_direc, "ENTIDAD", "PROVEEDOR","RUCPROVEEDOR", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")%>%
  group_by(ENTIDAD)%>%
  summarize(MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())%>%
  arrange(desc(MONTOADJSOLES))%>%
  head(10)

entidad_mont<-ggplot(entidad_mo, aes(x=MONTOADJSOLES,y=ENTIDAD))+
  geom_bar(stat="identity", position="dodge", fill="white",col="steelblue")+
  labs(title="TOP 10 DE ENTIDADES CON MAYOR MONTO DE CONTRATACIÓN", 
       y="Entidad", x="Monto contratado", caption="Manos a la data")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#entidad_mont
