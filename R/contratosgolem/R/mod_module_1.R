#' module_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_module_1_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' module_1 Server Function
#'
#' @noRd 
mod_module_1_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_module_1_ui("module_1_ui_1")
    
## To be copied in the server
# callModule(mod_module_1_server, "module_1_ui_1")
 
