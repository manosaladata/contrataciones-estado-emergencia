library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(formattable)
library(data.table)
library(gridExtra)
library(rsconnect)
library(tmap)
library(leaflet)
library(plotly)
library(DT)
library(ggrepel)


######MODULIZANDO#######
#Obs: El directirio será el del app, entonces, al hacer referencias en para leer documentos en los módulos
#se debe asumir que en dichos módulos el directorio es el del app también.
source("modules/maps_mod.R")
source("modules/plotly_mod.R")
source("modules/ggplot_mod.R")
source("modules/DT_mod.R")
#####################FUNCIÓN PARA INCLUIR Badget en menusubitems#############
menuSubItem2 <- function (text, tabName = NULL, href = NULL, newtab = TRUE, 
                          icon = shiny::icon("angle-double-right"), 
                          selected = NULL, badgeLabel = NULL, badgeColor = "green") {
  if (!is.null(href) && !is.null(tabName)) {
    stop("Can't specify both href and tabName")
  }
  isTabItem <- FALSE
  target <- NULL
  if (!is.null(badgeLabel)) {
    badgeTag <- tags$small(class = paste0("badge pull-right bg-", 
                                          badgeColor), badgeLabel)
  }
  else {
    badgeTag <- NULL
  }
  
  if (!is.null(tabName)) {
    shinydashboard:::validateTabName(tabName)
    isTabItem <- TRUE
    href <- paste0("#shiny-tab-", tabName)
  }
  else if (is.null(href)) {
    href <- "#"
  }
  else {
    if (newtab) 
      target <- "_blank"
  }
  tags$li(a(href = href, `data-toggle` = if (isTabItem) 
    "tab", `data-value` = if (!is.null(tabName)) 
      tabName, `data-start-selected` = if (isTRUE(selected)) 
        1
    else NULL, target = target, icon, span(text), badgeTag))
}


#rsconnect::deployApp('path/to/your/app')
#library(tableHTML)


customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"


################################################################
##############TRABAJANDO CON DATOS CONSOLIDADOS/WORKSPACE#################
################################################################

DATA_CONSOLIDADA_120 <- read.csv(file = 'modules/Data/DATA_CONSOLIDADA_120.csv', sep = ";")
###############FUCIONES#########################
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
redondeo <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=2)))

df<-DATA_CONSOLIDADA_120
#names(df)
names(df)<-c("Proveedor","RUC","Inscripción","Trabajadores (octubre)","Monto en millones",
             "Contratos","Sanciones","Penalidades")

df[,5]<-sapply(df[,5],redondeo)


ui <- dashboardPage(title="Contratos Directos COVID-19", skin="blue",  #Color del encabezado y nombre a la página (cuando abres con el explordor se nota)
                    #numericInput("ENTIDAD_DEPARTAMENTO",ENTIDAD_DEPARTAMENTO),
                    dashboardHeader(title="CONTENIDO",
                                    tags$li(class = "dropdown",
                                            tags$a(href = "http://www.facebook.com/sharer.php?u=https://abnercasallo.shinyapps.io/Data/", 
                                                   target = "_blank", 
                                                   tags$img( 
                                                     imageOutput("face",height = "20px"))
                                            )
                                    ),
                                    
                                    tags$li(class = "dropdown",
                                            tags$a(href = "http://twitter.com/share?url=https://abnercasallo.shinyapps.io/Data/", 
                                                   target = "_blank", 
                                                   tags$img( 
                                                            imageOutput("twr",height = "20px"))
                                            )
                                    ),
                                    
                                    
                                    tags$li(class = "dropdown",
                                            tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=https://abnercasallo.shinyapps.io/Data/", 
                                                   target = "_blank", 
                                                   tags$img( 
                                                            tags$img( 
                                                              imageOutput("link",height = "20px"))
                                            )
                                    )),
                                    
                                    dropdownMenu(type="message",
                                                 messageItem(from="Abner", message="Bienvenido"),
                                                 messageItem(from="Abner", message="Proyecto Open Source", icon=icon("bar-chart")),
                                                 messageItem(from="Abner", message="Trabajo parte del Proyecto Manos a la Data", icon=icon("vcard"), time = "10-10-2020")),
                                    dropdownMenu(type="notifications",        
                                                 notificationItem(
                                                   text="Actualizado hasta el 03/12",
                                                   icon=icon("dashboard"),
                                                   status="success"),
                                                 notificationItem(
                                                   text="Proyecto Beta",
                                                   icon=icon("warning"),
                                                   status="warning"))
                                    # ,dropdownMenu(type="task",
                                    #              taskItem(
                                    #                value=80,
                                    #                color="aqua",
                                    #                "Avance de ideas del proyecto"
                                    #              ),
                                    #              taskItem(
                                    #                value=60,
                                    #                color="green",
                                    #                "Avance de Gráficos y Tablas"
                                    #              ),
                                    #              taskItem(
                                    #                value=30,
                                    #                color="red",
                                    #                "Automatización"
                                    #              ))
                                    ),
                    
                    dashboardSidebar(
                      # tags$head(
                      #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      # ),
                      #sidebarSearchForm("searchText","buttonSearch","Search"),
                      sidebarMenu(                                 #Para crear un menú y se pueda abrir una nueva ventana por cada item.
                        id="sidebarID",
                        menuItem("Información General", tabName="map_mon", icon = icon("arrow-alt-circle-right")), #el tab Name=dep, permite relacionar el grÃ¡fico de dashboardBody
                        menuItem("Proveedores",id = "chartsID", #badgeLabel = "Importante",badgeColor ="red",
                                 icon = icon("arrow-alt-circle-right"),
                                 menuSubItem("Todos los Proveedores", tabName = "contr_prove"),
                                 menuSubItem("Personas Naturales",tabName = "per_nat"),
                                 menuSubItem2("Top 100 de Proveedores",tabName="raros",icon = shiny::icon("eye"),badgeLabel = "Importante",badgeColor ="red")
                        ),
                        menuItem("Rubros",id = "chartsID",icon = icon("arrow-alt-circle-right"),
                        menuSubItem("Por Montos",tabName = "rubros_funnel"),    #Más icons:https://fontawesome.com/icons?d=gallery
                        menuSubItem("Por Número de Contratos", tabName = "funnel_n")
                        ),
                        menuItem("Por entidad",id = "chartsID",icon = icon("arrow-alt-circle-right"), #el tab Name=contract, permite relacionar el histograma
                        menuSubItem("TOP 10 de Entidades",tabName = "entidad_mn"),
                        menuSubItem("Entidades en General", tabName = "entidt_num")),
                        #menuSubItem("Entidades por Monto",tabName="entidt_mon")),
                        menuItem("Buscador de RUC", tabName = "link_sunat",icon = icon("arrow-alt-circle-right")),
                        menuSubItem2("Buscador del OSCE",tabName = "detalle",icon = icon("arrow-alt-circle-right")),
                        menuItem ("Guía Normativa", tabName="derecho",icon = icon("arrow-alt-circle-right")),
                        menuItem("Repositorio de Git-Hub",tabName = "Git-Hub", icon=icon("github-square")),
                        menuItem("Agradecimientos", tabName="gracias",icon = icon("hands")),
                        menuItem("Donaciones", tabName = "dona", icon=icon("hand-holding-heart")),
                        textInput("text_input","Contáctenos", value="abner.casallo@unmsm.edu.pe"),
                        
                        textInput("text_input", "Aclaración", value="La información sobre sanciones y penalidades han sido obtenidas del buscador de proveedores y se actualiza cada cierto periodo (semanal o quincenal). En este sentido, es referencial. Para denuncias y otras cuestiones legales se debe verificar en la página del Buscador de Proveedores. Para cualquier consulta u observación no dude en contactarse con nosotros.
                                ")
                                                )),
                    
                    
                    dashboardBody(                        #Podría ir arriba, pero sale desordenado.
                      # tags$head(
                      #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      # ),
                      tags$style(
                        type = 'text/css', 
                        '.bg-aqua {background-color: #005CB9!important; }'
                      ),
                      tabItems(tabItem(tabName = "map_mon",
                                       div(style="font-size: 100%; width:100%;overflow-x: scroll",
                                     
                                       fluidRow(
                                         column(width=8,  
                                                valueBox("9 Meses","Periodo: Marzo-Diciembre",icon=icon("hourglass-3"),color="yellow"),
                                                valueBoxOutput("num"),
                                                #valueBox("xx", "Monto Total", color = "green"),
                                                valueBoxOutput("monto"),
                                                #infoBoxOutput("info"),
                                                
                                         )
                                         ,column(width = 3,
                                                 imageOutput("manos", width="50%",height="150px")
                                                )
                                         ),
                                         # fluidRow(
                                         #   column(width = 8,
                                         #          imageOutput("manos", width="20%",height="30px")
                                         #          )
                                         #   
                                         #   
                                         # )
                                         # ,
                                       
                                       fluidRow(column(width=8,
                                                infoBox("Transparencia","100%",icon=icon("thumbs-up")),
                                                infoBox("Dato abiertos", "100%"),
                                                         )
                                                # ,column(width = 3,
                                                #        imageOutput("manos", width="20%",height="150")
                                                # )
                                                ),
                                       
                                       fluidRow(
                                                       box(title="MONTOS POR DEPARTAMENTO",status="primary",
                                                       solidHeader = T,mapUI("map_mon"),
                                                       width=8, height=500)
                                                ,
                                                        box(monto_depUI("monto_dep"), width=4)
                                                
                                                       
                                                       
                                        ))
                                       
                                       #,fluidRow(box(DT::dataTableOutput("ley")))
                                       # ,fluidRow(imageOutput("manos"))
                                       
                      ),
                      tabItem(tabName = "entidad", h1("Entidades")),
                      tabItem(tabName = "rubros_funnel",fluidRow(
                        box(status="primary",solidHeader = T  ,width = 10,title="MONTO POR CADA RUBRO (en millones de soles)",plotly1UI(("rubros_funnel"))
                        ))),
                      tabItem(tabName="funnel_n",fluidRow(
                              box(status="primary",solidHeader = T  ,width = 10,
                                  title="NÚMERO DE CONTRATOS POR RUBRO", plotlyOutput("funnel_n")))),
                      tabItem(tabName = "map_num",
                              fluidRow(
                                box(leafletOutput("map_num"))
                              )),
                      # tabItem(tabName ="dep_mn",
                      #         fluidPage(
                      #                    column(width=6.5,plotOutput("num_dep")),
                      #                    column(width=6.5,ggmontUI("ggmont"))
                      #         )),
                      tabItem(tabName ="entidad_mn",
                              fluidPage(
                                column(width=12,entmontUI("entmont")),
                                column(width=12,entnumUI("entnum"))
                              )),
                      tabItem(tabName="entidt_num",
                              fluidPage(div(DT::dataTableOutput("entidt_num"),style = "font-size: 100%; width:100%;overflow-x: scroll"))),
                      # tabItem(tabName="entidt_mon",
                      #         fluidPage(DT::dataTableOutput("entidt_mon"))),
                      
                      tabItem(tabName="raros",
                              fluidRow(#tags$head(tags$style( type = 'text/css',  '.rdataTable{ overflow-x: scroll; }')),
                                       div(DT::dataTableOutput("table_raros"),style = "font-size: 100%; width: 100%;overflow-x: scroll")),
                                       p(style="font-family:Impact","ND-MES:No declaró en el mes de Octubre"),
                                       p(style="font-family:Impact","ND-ANUAL: No declaró durante el Año"),
                                       p(style="font-family:Impact","NA: No encontrado por mal registro del RUC en la base de datos")
                                
                              ),
                      tabItem(tabName = "detalle",
                              h1("BUSCADOR DE PROVEEDORES",style="font-family:Impact"),
                              p(style="font-family:Impact", 
                                "¿Algún proveedor le llamó la atención? Para una búsqueda detallada puede acceder
                                al buscador de proveedores del Osce:",
                                a("BUSCADOR DE PROVEEDORES",
                                  href="https://apps.osce.gob.pe/perfilprov-ui/"))
                              
                      ),
                      tabItem(tabName = "Git-Hub",
                              h1("PROYECTO OPEN SOURCE",style = "font-family:Impact, cursive; font-weight: 500; line-height: 1.1; 
                                 color:indigo;"),
                              p(style="font-family:Impact", 
                                "Usted puede contrubuir con nosotros a través de nuestro repositorio de",
                                a("Git-Hub",
                                href="https://github.com/manosaladata/contrataciones-estado-emergencia.git"))
                          
                              ),
                      tabItem(tabName="contr_prove",
                              fluidRow(div(DT::dataTableOutput("contr_prove"),style = "font-size: 100%; width: 100%;overflow-x: scroll"))
                      
                      ),
                      tabItem(
                        tabName = "link_sunat",
                        fluidRow(
                          div(tags$iframe(
                            seamless = "seamless", 
                            src = "https://e-consultaruc.sunat.gob.pe/cl-ti-itmrconsruc/FrameCriterioBusquedaMovil.jsp", 
                            height = 800, width = 1000
                          ),style = "font-size: 100%; width: 100%;overflow-x: scroll")
                        )),
                      tabItem(tabName = "per_nat",
                              fluidRow(div(per_natUI("per_nat"),style = "font-size: 100%; width: 100%;overflow-x: scroll"))),
                      tabItem(tabName="gracias",
                              h1("AGRADECIMIENTOS",style = "font-family:Impact"),
                              p(style="font-family:Georgia", 
                                "Este proyecto no hubiese sido posible sin el apoyo de muchas personas.
                                En particular un agradecimiento al equipo de Manos a la Data, es especial a Arturo Chian",
                                # a("Arturo Chian",href="https://arturochian.com/"), 
                                "por el apoyo constante durante todo este periodo.
                                Asimismo, un agracedimiento especial a Luigi Castro por sus valiosos insights en temas
                                de Contración Pública.")
                              ),
                      tabItem(tabName="dona",
                              h1("DONACIONES",style = "font-family:Impact"),
                              p(style="font-family:Georgia","Puedes apoyar a mantener este proyecto en la web
                                donando a través de",a("Buymeacoffe", href="https://www.buymeacoffee.com/AbnerCasallo"),
                                "o la siguiente cuenta...")),
                      tabItem(tabName="derecho",
                              fluidRow(tags$head(
                                  tags$link(rel = "stylesheet", type = "text/css", href = "derecho.css")
                                ),
                                includeHTML("www/derecho.html"))
                      )
                              
                      
                    )
))

server <- function(input
                   , output) {
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "CHARTS"){
      updateTabItems(session, "sidebarID", selected = "hiddenCharts")
    }
  })
  
  #output$num_dep<-renderPlot({num_dep})
  #output$mont_dep<-renderPlot({montos_dep})
  output$entidad_num<-renderPlot({entidad_num})
  #output$entidad_mont<-renderPlot({entidad_mont})
  entmontServer("entmont")
  entnumServer("entnum")
  
  
  
  
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
    `Contratos`= color_tile("white", "red"),
    `Sanciones`= formatter("span", style = ~ formattable::style(color = ifelse(`Sanciones` ==0 , "black","red"),font.weight = "bold")),
    `Penalidades`= formatter("span", style =  ~ formattable::style(color = ifelse(`Penalidades` ==0 , "black","red"),font.weight = "bold"))
    ,`Monto en millones` = color_tile("white","darkgreen")
    
   )) 
  
  }))
  output$entidt_num<-DT::renderDataTable(entidad_dt_num)
  #output$entidt_mon<-DT::renderDataTable(entidad_dt_mon)
  output$contr_prove<-DT::renderDataTable(contr_prove)
  #output$per_nat<-DT::renderDataTable(per_nat)
  per_natServer("per_nat")
  mapServer("map_mon")
  # output$map_mon <- renderLeaflet({map_mon})   #Leaflet object
  output$map_num <- renderLeaflet({map_num})
  # output$rubros_funnel <- renderImage({
  #   return(list(src = "rubros_funnel.svg",contentType = "image/svg+xml"))
  # })
  
  # getPage<-function() {
  #   return(includeHTML("index.html"))
  # }
  # output$rubros_funnel<-renderUI({getPage()})
  # output$rubros_funnel<- renderPlotly(({rubros_funnel}))
  plotly1Server("rubros_funnel")


  output$manos <- renderImage({
    return(list(src = "www/manos.png",contentType = "image/png",height = "200px"))
  }, deleteFile = FALSE)
  output$face <- renderImage({
    return(list(src = "www/face.png",contentType = "image/png",height = "30px"))
  }, deleteFile = FALSE)
  output$twr <- renderImage({
    return(list(src = "www/tw.png",contentType = "image/png",height = "30px"))
  }, deleteFile = FALSE)
  output$link <- renderImage({
    return(list(src = "www/linke.png",contentType = "image/png",height = "30px"))
  }, deleteFile = FALSE)
  monto_depServer("monto_dep")
  output$funnel_n<-renderPlotly(({rubros_funnel_n}))
  
  
}

shinyApp(ui = ui, server = server, options = list(height = 1080))


