#' ent_dt_mon UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 




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
contr_direc <- read_excel(here("R","Data","CONOSCE_CONTRATACIONDIRECTA.xlsx"))
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

entidad_dt_num<-as.datatable(formattable(entidaddt_num, align =c("c","c","c","c","c"), list( 
  `CONTRATOS`= color_tile("white","red"),
  `MONTO ADJUDICADO EN SOLES (millones)`= color_tile("white","darkgreen")
)))


mod_ent_dt_mon_ui <- function(id){
  ns <- NS(id)
  DT::dataTableOutput(ns("ent_num"))
}
    
#' ent_dt_mon Server Function
#'
#' @noRd 
mod_ent_dt_mon_server <- function(input, output, session){
  ns <- session$ns
  output$ent_num<-DT::renderDataTable(entidad_dt_num)
 
}
    
## To be copied in the UI
# mod_ent_dt_mon_ui("ent_dt_mon_ui_1")
    
## To be copied in the server
# callModule(mod_ent_dt_mon_server, "ent_dt_mon_ui_1")
 
