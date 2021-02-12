#' mod UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'@import dplyr sf tmap leaflet tmaptools
#'
#' @importFrom shiny NS tagList 

library(sf)
library(tidyverse)
library(ggrepel)
library(tmap)
library(readxl)
library(leaflet)
library(here)
library(tmaptools)



load("mapa.Rdata")
map_mon



mod_mod_ui <- function(id){
  ns <- NS(id)
  tagList(h2("MAPA"),
          leafletOutput(ns("map_mon")))
 
  
}
    
#' mod Server Function
#'
#' @noRd 
mod_mod_server <- function(input, output, session){
  ns <- session$ns
  output$map_mon <- renderLeaflet({map_mon})
 
}
    
## To be copied in the UI
# mod_mod_ui("mod_ui_1")
    
## To be copied in the server
# callModule(mod_mod_server, "mod_ui_1")
 
































































































    