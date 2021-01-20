server <- function(input
                   , output) {
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "CHARTS"){
      updateTabItems(session, "sidebarID", selected = "hiddenCharts")
    }
  })
  
  output$num_dep<-renderPlot({num_dep})
  output$mont_dep<-renderPlot({montos_dep})
  output$entidad_num<-renderPlot({entidad_num})
  output$entidad_mont<-renderPlot({entidad_mont})
  
  
  
  
  output$num_contr<-renderPlot({
    hist(proveedores2_num$Contratos, main= "Distribución de contratos",
         xlab="contratos por proveedor",ylab="número de proveedores", col="purple", breaks=input$n)
  })
  
  
  # 
  # output$info<-renderInfoBox({infoBox("Actualización","2 de diciembre",
  #                                     icon=icon("bar-chart-o"))})
  output$num<-renderInfoBox({valueBox(count((contr_direc)[28]),"Contratos Analizados", 
                                      icon=icon("eye"),color="red")})
  output$monto<-renderInfoBox({valueBox(redondeo(sum((contr_direc)[28])),"Millones de Soles", 
                                        icon=icon("money"),color="green")})  
  output$table_raros <- DT::renderDataTable(as.datatable({formattable(df, align =c("c","c","c","c","c","c","c","c"), list( ###con align alineamos: ",align =c("l","c","c","c","c", "c", "c", "c", "r")"
    `Contratos`= formatter("span", style =  ~ formattable::style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                           ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
    `Sanciones`= formatter("span", style = ~ formattable::style(color = ifelse(`Sanciones` ==0 , "green","red"),font.weight = "bold")),
    `Penalidades`= formatter("span", style =  ~ formattable::style(color = ifelse(`Penalidades` ==0 , "green","red"),font.weight = "bold"))
    ,`Monto` = color_bar("green")
    
  )) 
    
  }))
  output$entidt_num<-DT::renderDataTable(entidad_dt_num)
  output$entidt_mon<-DT::renderDataTable(entidad_dt_mon)
  output$contr_prove<-DT::renderDataTable(contr_prove)
  output$per_nat<-DT::renderDataTable(per_nat)
  
  output$map_mon <- renderLeaflet({map_mon})   #Leaflet object
  output$map_num <- renderLeaflet({map_num})
  output$rubros_funnel <- renderPlotly({rubros_funnel})
  output$manos <- renderImage({
    return(list(src = "manos.PNG",contentType = "image/png"))
  }, deleteFile = FALSE)
  output$facebook <- renderImage({
    return(list(src = "facebook.PNG",contentType = "image/png",height = "30px"))
  }, deleteFile = FALSE)
  output$twitter <- renderImage({
    return(list(src = "twitter.PNG",contentType = "image/png",height = "30px"))
  }, deleteFile = FALSE)
  output$linkedin <- renderImage({
    return(list(src = "linkedin.PNG",contentType = "image/png",height = "30px"))
  }, deleteFile = FALSE)
  output$monto_dep<-DT::renderDataTable(monto_dep)
  output$funnel_n<-renderPlotly(({rubros_funnel_n}))
  
  
}


shinyApp(ui = ui, server = server, options = list(height = 1080))


