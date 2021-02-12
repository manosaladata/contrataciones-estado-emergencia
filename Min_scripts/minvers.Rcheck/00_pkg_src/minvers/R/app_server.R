#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_mod1_server, "mod1_ui_1")
  callModule(mod_DT_server, "DT_ui_1")
  callModule(mod_contr_prove_server, "contr_prove_ui_1")
  callModule(mod_monto_dep_server, "monto_dep_ui_1")
  callModule(mod_raros_server, "raros_ui_1")
  callModule(mod_plotly_server, "plotly_ui_1")
  callModule(mod_funnel_n_server, "funnel_n_ui_1")
  callModule(mod_ggplot_server, "ggplot_ui_1")
  callModule(mod_ggplot2_server, "ggplot2_ui_1")
  callModule(mod_ent_dt_mon_server, "ent_dt_mon_ui_1")
  output$num<-renderInfoBox({valueBox(count((contr_direc)[28]),"Contratos Analizados", 
                                      icon=icon("eye"),color="red")})
  output$monto<-renderInfoBox({valueBox(redondeo(sum((contr_direc)[28])),"Millones de Soles", 
                                        icon=icon("money"),color="green")})
  output$manos <- renderImage({
    return(list(src = "www/manos.png",contentType = "image/png",height = "200px"))
  }, deleteFile = FALSE)
  

  
  
}


