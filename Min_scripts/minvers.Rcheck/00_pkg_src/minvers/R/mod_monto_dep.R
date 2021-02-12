#' monto_dep UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

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



mod_monto_dep_ui <- function(id){
  ns <- NS(id)
  tagList(DT::dataTableOutput(ns("monto_dep"))
 
  )
}
    
#' monto_dep Server Function
#'
#' @noRd 
mod_monto_dep_server <- function(input, output, session){
  ns <- session$ns
  output$monto_dep<-DT::renderDataTable(monto_dep)
 
}
    
## To be copied in the UI
# mod_monto_dep_ui("monto_dep_ui_1")
    
## To be copied in the server
# callModule(mod_monto_dep_server, "monto_dep_ui_1")
 
