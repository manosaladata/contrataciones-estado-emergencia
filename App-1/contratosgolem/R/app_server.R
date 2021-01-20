#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_maps_module_server, "maps_module_ui_1")
  # # entmontServer("entmont")
  # # entnumServer("entnum")
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  # 
  # output$entidad_num<-renderPlot({entidad_num})
  # #output$entidad_mont<-renderPlot({entidad_mont})
  # 
  # 
  # 
  # 
  # 
  # output$num_contr<-renderPlot({
  #   hist(proveedores2_num$Contratos, main= "Distribución de contratos",
  #        xlab="contratos por proveedor",ylab="número de proveedores", col="purple", breaks=input$n)
  # })
  # 
  # 
  # # 
  # # output$info<-renderInfoBox({infoBox("Actualización","2 de diciembre",
  # #                                     icon=icon("bar-chart-o"))})
  # output$num<-renderInfoBox({valueBox(count((contr_direc)[28]),"Contratos Analizados", 
  #                                     icon=icon("eye"),color="red")})
  # output$monto<-renderInfoBox({valueBox(redondeo(sum((contr_direc)[28])),"Millones de Soles", 
  #                                       icon=icon("money"),color="green")})  
  # output$table_raros <- DT::renderDataTable(as.datatable({formattable(df, align =c("c","c","c","c","c","c","c","c"), list( ###con align alineamos: ",align =c("l","c","c","c","c", "c", "c", "c", "r")"
  #   `Contratos`= color_tile("white", "red"),
  #   `Sanciones`= formatter("span", style = ~ formattable::style(color = ifelse(`Sanciones` ==0 , "black","red"),font.weight = "bold")),
  #   `Penalidades`= formatter("span", style =  ~ formattable::style(color = ifelse(`Penalidades` ==0 , "black","red"),font.weight = "bold"))
  #   ,`Monto en millones` = color_tile("white","darkgreen")
  #   
  # )) 
  #   
  # }))
  # output$entidt_num<-DT::renderDataTable(entidad_dt_num)
  # #output$entidt_mon<-DT::renderDataTable(entidad_dt_mon)
  # output$contr_prove<-DT::renderDataTable(contr_prove)
  # #output$per_nat<-DT::renderDataTable(per_nat)
  # per_natServer("per_nat")
  # mapServer("map_mon")
  # # output$map_mon <- renderLeaflet({map_mon})   #Leaflet object
  # output$map_num <- renderLeaflet({map_num})
  # # output$rubros_funnel <- renderImage({
  # #   return(list(src = "rubros_funnel.svg",contentType = "image/svg+xml"))
  # # })
  # 
  # # getPage<-function() {
  # #   return(includeHTML("index.html"))
  # # }
  # # output$rubros_funnel<-renderUI({getPage()})
  # # output$rubros_funnel<- renderPlotly(({rubros_funnel}))
  # plotly1Server("rubros_funnel")
  # 
  # 
  # output$manos <- renderImage({
  #   return(list(src = "www/manos.png",contentType = "image/png",height = "200px"))
  # }, deleteFile = FALSE)
  # output$face <- renderImage({
  #   return(list(src = "www/face.png",contentType = "image/png",height = "30px"))
  # }, deleteFile = FALSE)
  # output$twr <- renderImage({
  #   return(list(src = "www/tw.png",contentType = "image/png",height = "30px"))
  # }, deleteFile = FALSE)
  # output$link <- renderImage({
  #   return(list(src = "www/linke.png",contentType = "image/png",height = "30px"))
  # }, deleteFile = FALSE)
  # monto_depServer("monto_dep")
  # output$funnel_n<-renderPlotly(({rubros_funnel_n}))
}

