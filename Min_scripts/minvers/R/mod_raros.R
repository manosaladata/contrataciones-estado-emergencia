#' raros UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

library(readxl)
#library(leaflet)
library(here)
DATA_CONSOLIDADA_120 <- read.csv(here("R","Data",'DATA_CONSOLIDADA_120.csv'), sep = ";")

###############FUCIONES#########################
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
redondeo <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=2)))

df<-DATA_CONSOLIDADA_120
#names(df)
names(df)<-c("Proveedor","RUC","Inscripción","Trabajadores (octubre)","Monto en millones",
             "Contratos","Sanciones","Penalidades")

df[,5]<-sapply(df[,5],redondeo)


mod_raros_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("raros"))
 
  )
}
    
#' raros Server Function
#'
#' @noRd 
mod_raros_server <- function(input, output, session){
  ns <- session$ns
  output$raros <- DT::renderDataTable(as.datatable({formattable(df, align =c("c","c","c","c","c","c","c","c"), list( ###con align alineamos: ",align =c("l","c","c","c","c", "c", "c", "c", "r")"
    `Contratos`= color_tile("white", "red"),
    `Sanciones`= formatter("span", style = ~ formattable::style(color = ifelse(`Sanciones` ==0 , "black","red"),font.weight = "bold")),
    `Penalidades`= formatter("span", style =  ~ formattable::style(color = ifelse(`Penalidades` ==0 , "black","red"),font.weight = "bold"))
    ,`Monto en millones` = color_tile("white","darkgreen")
    
  )) 
    
  }))
 
}
    
## To be copied in the UI
# mod_raros_ui("raros_ui_1")
    
## To be copied in the server
# callModule(mod_raros_server, "raros_ui_1")
 
