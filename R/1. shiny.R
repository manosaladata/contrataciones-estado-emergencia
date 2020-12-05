library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(formattable)
library(data.table)
library(gridExtra)
library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
#library(tableHTML)


customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"


################################################################
##############TRABAJANDO CON DATOS CONSOLIDADOS/WORKSPACE#################
################################################################
setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")
load("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data/Datos_consolidados.RData")
remove(list=setdiff(ls(), "DATA_CONSOLIDADA_520"))  

###############FUCIONES#########################
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
redondeo <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=2)))

df<-DATA_CONSOLIDADA_520
#names(df)
names(df)<-c("Proveedor","RUC","Inscripción","Trabajadores","Monto",
             "Contratos","Sanciones","Penalidades")

df[,5]<-sapply(df[,5],redondeo)

####CARGAMOS DATA DE CONOSCE
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
 #names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
contr_direc[,28]<-sapply(contr_direc[,28],redondeo)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROV"
# #names(contr_direc)
options(scipen=999)                                 
contr_direc[28]


ui <- dashboardPage(title="Contratos Directos COVID-19", skin="blue",  #Color del encabezado y nombre a la página (cuando abres con el explordor se nota)
                    #numericInput("ENTIDAD_DEPARTAMENTO",ENTIDAD_DEPARTAMENTO),
                    dashboardHeader(title="CONTENIDO",
                                    dropdownMenu(type="message",   
                                                 messageItem(from="Abner", message="Bienvenido"),
                                                 messageItem(from="Abner", message="Proyecto Open Source", icon=icon("bar-chart")),
                                                 messageItem(from="Abner", message="Trabajo parte del Proyecto Manos a la Data", icon=icon("vcard"), time = "10-10-2020")),
                                    dropdownMenu(type="notifications",        
                                                 notificationItem(
                                                   text="Actualizado hasta octubre",
                                                   icon=icon("dashboard"),
                                                   status="success"),
                                                 notificationItem(
                                                   text="Proyecto Beta",
                                                   icon=icon("warning"),
                                                   status="warning")),
                                    dropdownMenu(type="task",
                                                 taskItem(
                                                   value=80,
                                                   color="aqua",
                                                   "Avance de ideas del proyecto"
                                                 ),
                                                 taskItem(
                                                   value=60,
                                                   color="green",
                                                   "Avance de Gráficos y Tablas"
                                                 ),
                                                 taskItem(
                                                   value=30,
                                                   color="red",
                                                   "Automatización"
                                                 ))),
                    
                    dashboardSidebar(
                      #sidebarSearchForm("searchText","buttonSearch","Search"),
                      sidebarMenu(                                 #Para crear un menú y se pueda abrir una nueva ventana por cada item.
                        id="sidebarID",
                        menuItem("Información General", tabName="map_mon", icon = icon("arrow-alt-circle-right")), #el tab Name=dep, permite relacionar el grÃ¡fico de dashboardBody
                        menuItem("Top 500 de proveedores",badgeLabel = "Importante",badgeColor ="red",icon = icon("arrow-alt-circle-right"),tabName="raros"),
                        menuItem("Rubros",id = "chartsID",icon = icon("arrow-alt-circle-right"),
                        menuSubItem("Por Montos",tabName = "rubros_funnel"),    #Más icons:https://fontawesome.com/icons?d=gallery
                        menuSubItem("Por Número de Contratos", tabName = "funnel_n")),
                        menuItem("Por entidad",id = "chartsID",icon = icon("arrow-alt-circle-right"), #el tab Name=contract, permite relacionar el histograma
                        menuSubItem("TOP 10 de Entidades",tabName = "entidad_mn"),
                        menuSubItem("Entidades por contratos", tabName = "entidt_num"),
                        menuSubItem("Entidades por Monto",tabName="entidt_mon")),
                        textInput("text_input","Contáctenos", value="abner.casallo@unmsm.edu.pe"),
                        textInput("text_input","Repositorio Git-Hub", value="https://github.com/manosaladata/contrataciones-estado-emergencia.git"),
                        textInput("text_input", "Aclaración", value="La información sobre sanciones y penalidades han sido obtenidas del buscador de proveedores y se actualiza cada cierto periodo (semanal o quincenal). En este sentido, es referencial. Para denuncias y otras cuestiones legales se debe verificar en la página del Buscador de Proveedores. Para cualquier consulta u observación no dude en contactarse con nosotros.
                                ")
                                                )),
                    
                    
                    dashboardBody(                        #Podría ir arriba, pero sale desordenado.
                      tags$style(
                        type = 'text/css', 
                        '.bg-aqua {background-color: #005CB9!important; }'
                      ),
                      tabItems(tabItem(tabName = "map_mon",
                                       fluidRow(
                                         column(width=8,  
                                                valueBox("5 Meses","Periodo: Marzo-Agosto",icon=icon("hourglass-3"),color="yellow"),
                                                valueBoxOutput("num"),
                                                #valueBox("xx", "Monto Total", color = "green"),
                                                valueBoxOutput("monto"),
                                                #infoBoxOutput("info")  
                                         ),
                                         column(width = 3,
                                                imageOutput("manos", width="50%",height="100px")
                                                )),
                                       
                                       fluidRow(column(width=8,
                                                infoBox("Transparencia","100%",icon=icon("thumbs-up")),
                                                infoBox("Dato abiertos", "100%"),
                                                         )),
                                       
                                       fluidRow(
                                                       box(title="MONTOS POR DEPARTAMENTO",status="primary",
                                                       solidHeader = T,leafletOutput("map_mon"),
                                                       width=8, height=500)
                                                ,
                                                        box(DT::dataTableOutput("monto_dep"), width=4)
                                                
                                                       
                                                       
                                        )
                                       
                                       #,fluidRow(box(DT::dataTableOutput("ley")))
                                       # ,fluidRow(imageOutput("manos"))
                                       
                      ),
                      tabItem(tabName = "entidad", h1("Entidades")),
                      tabItem(tabName = "rubros_funnel",fluidRow(
                        box(status="primary",solidHeader = T  ,width = 10,title="MONTO POR CADA RUBRO",plotlyOutput("rubros_funnel"))
                        )),
                      tabItem(tabName="funnel_n",fluidRow(
                              box(status="primary",solidHeader = T  ,width = 10,
                                  title="NÚMERO DE CONTRATOS POR RUBRO", plotlyOutput("funnel_n")))),
                      tabItem(tabName = "map_num",
                              fluidRow(
                                box(leafletOutput("map_num"))
                              )),
                      tabItem(tabName ="dep_mn",
                              fluidPage(
                                         column(width=6.5,plotOutput("num_dep")),
                                         column(width=6.5,plotOutput("mont_dep"))
                              )),
                      tabItem(tabName ="entidad_mn",
                              fluidPage(
                                column(width=12,plotOutput("entidad_mont")),
                                column(width=12,plotOutput("entidad_num"))
                              )),
                      tabItem(tabName="entidt_num",
                              fluidPage(DT::dataTableOutput("entidt_num"))),
                      tabItem(tabName="entidt_mon",
                              fluidPage(DT::dataTableOutput("entidt_mon"))),
                      
                      tabItem(tabName="raros",
                              fluidRow(DT::dataTableOutput("table_raros")
                                
                              ))
                      
                      ),
                      
                    )
)

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
  output$monto<-renderInfoBox({valueBox(sum((contr_direc)[28]),"Millones de Soles", 
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
  
  output$map_mon <- renderLeaflet({map_mon})   #Leaflet object
  output$map_num <- renderLeaflet({map_num})
  output$rubros_funnel <- renderPlotly({rubros_funnel})
  output$manos <- renderImage({
    return(list(src = "manos.PNG",contentType = "image/png"))
  }, deleteFile = FALSE)
  output$monto_dep<-DT::renderDataTable(monto_dep)
  output$funnel_n<-renderPlotly(({rubros_funnel_n}))
  
  
}

names(df)
shinyApp(ui = ui, server = server, options = list(height = 1080))



