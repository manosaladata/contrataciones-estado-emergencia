library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(formattable)
library(data.table)
library(gridExtra)
library(plotly)
#ó
library(sf)    #Permite relación geos
library(ggrepel)
library(tmap)

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
redondeo <- function(x, k) trimws(format(round(x, k), nsmall=2))

df<-DATA_CONSOLIDADA_520
#names(df)
names(df)<-c("Proveedor","RUC","Inscripción","Trabajadores","Monto",
             "Contratos","Sanciones","Penalidades")

df[,5]<-sapply(df[,5],redondeo)

####CARGAMOS
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
contr_direc[,28]<-sapply(contr_direc[,28],redondeo)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROV"
# #names(contr_direc)
options(scipen=999)                                 



ui <- dashboardPage(title="Proyecto", skin="red", #da color al encabezado y nombre a la página (cuando abres con el explordor se nota)
                    #numericInput("ENTIDAD_DEPARTAMENTO",ENTIDAD_DEPARTAMENTO),
                    dashboardHeader(title="EXPLORADOR",
                                    dropdownMenu(type="message",   #También se pueden trabajar con mensajes dinÃ¡micos con dropdownMenuOutput() usando un csv.
                                                 messageItem(from="Abner", message="Bienvenido"),
                                                 messageItem(from="Abner", message="Proyecto al 50%", icon=icon("bar-chart"), time = "21:00"),
                                                 messageItem(from="Abner", message="Trabajo parte del Proyecto Manos a la Data", icon=icon("vcard"), time = "10-10-2020")),
                                    dropdownMenu(type="notifications",        #para todos los inputs dentro de deopdownMenu, tambiÃ©n se puede automatizar con un csv.
                                                 notificationItem(
                                                   text="Esperamos que les sirva",
                                                   icon=icon("dashboard"),
                                                   status="success"),
                                                 notificationItem(
                                                   text="Base de datos aÃºn no actualizada",
                                                   icon=icon("warning"),
                                                   status="warning")),
                                    dropdownMenu(type="task",
                                                 taskItem(
                                                   value=50,
                                                   color="aqua",
                                                   "Avance de ideas del proyecto"
                                                 ),
                                                 taskItem(
                                                   value=60,
                                                   color="green",
                                                   "Avance de grÃ¡ficos y tablas"
                                                 ),
                                                 taskItem(
                                                   value=10,
                                                   color="red",
                                                   "automatizaciÃ³n"
                                                 ))),
                    
                    dashboardSidebar(
                      #sliderInput(inputId = "n",                   #En el dashboardsiderbar no van box, queda feo si lo pones.                        
                      # "Number of contracts",
                      # 1,100,50),
                      sidebarMenu(                                 #Esto permitirÃ¡ que todo sea visto como un menÃº y se pueda abrir una nueva ventana por cada item.
                        sidebarSearchForm("searchText","buttonSearch","Search"),
                        menuItem("Información General", tabName="rubros_funnel", icon = icon("arrow-alt-circle-right")), #el tab Name=dep, permite relacionar el grÃ¡fico de dashboardBody
                        menuItem("Visión departamental",tabName = "map",icon = icon("arrow-alt-circle-right")),                                   #MÃ¡s icons:https://fontawesome.com/icons?d=gallery 
                        menuSubItem("Por monto",tabName = "mont_dep"),
                        menuSubItem("Por número de contratos",tabName="num_dep"),
                        menuItem("Por entidad",tabName = "entidad",icon = icon("arrow-alt-circle-right")), #el tab Name=contract, permite relacionar el histograma
                        menuSubItem("Por número de contratos"),
                        menuSubItem("Orden por monto contratado"),
                        menuItem("Información de proveedores",tabName = "hist",icon = icon("arrow-alt-circle-right")),
                        menuSubItem("Proveedores con más contratos"),
                        menuSubItem("Información de SUNAT"),
                        menuItem("Casos de alerta",icon = icon("dashboard")),
                        menuSubItem("Sanciones previas"),
                        menuSubItem("Prácticas poco comunes", tabName = "raros"),
                        menuItem("Nuevo", badgeLabel = "New", badgeColor ="green" ),
                        textInput("text_input","ContÃ¡ctenos al:", value="xxxxxx@gmail.com")
                      )),
                    
                    dashboardBody(                        #Esta parte darÃ¡ el contenido (podrÃ­a ir arriba, pero sale desordenado)
                      tabItems(tabItem(tabName = "rubros_funnel",
                                       fluidRow(
                                         column(width=9,  #column da tamaÃ±o para todos sus , acÃ¡ daremos 9 a todos
                                                infoBox("Transparencia","100%",icon=icon("thumbs-up")),
                                                infoBox("Dato abiertos", "100%"),
                                                infoBoxOutput("info")  #otro forma de introducir trabajando con server
                                         )),
                                       
                                       fluidRow(valueBox("7 meses","Periodo de anÃ¡lisis",icon=icon("hourglass-3"),
                                                         color="yellow"),
                                                valueBoxOutput("num")),
                                       
                                       fluidRow(
                                         # splitLayout(cellWidths = c(700,700),
                                         #             box(title= "Gráfico 1", status="primary"
                                         #     ,solidHeader=T,  background="aqua", plotOutput("map_mon")),
                                         box(title="Rubros",status="primary",
                                             solidHeader = T,plotlyOutput("rubros_funnel"), width=15, height=700)
                                         # ,box(title= "Gráfico 2",status="primary", solidHeader=T,
                                         #      background="aqua",plotOutput("map_num")))
                                       )
                                       
                      ),
                      tabItem(tabName = "entidad", h1("Entidades")),
                      tabItem(tabName = "mont_dep",fluidPage(box(plotOutput("mont_dep")))),
                      
                      tabItem(tabName = "hist",
                              fluidRow(
                                box(plotOutput("num_contr")),
                                box(title="Controles",status="warning",solidHeader=T,background = "red",
                                    "USAR PARA ACOMODAR GRÁFICO", br(),"SI ES CONFUSO NO, USAR", #br() da espacio
                                    sliderInput(inputId = "n",
                                                "Número de contratos por proveedor",
                                                1,80,40),    #c.11: se puede usar tabpanel para meter esto a la imagen como una opciÃ³n.
                                    textInput("text_input","Anotaciones", value="ingrese sus anotaciones"))
                                
                              )),
                      tabItem(tabName = "num_dep",
                              fluidRow(
                                box(plotOutput("num_dep"))
                              )),
                      tabItem(tabName ="map",
                              fluidRow(splitLayout(cellWidths = c(700,700),   #Juntamos dos plots
                                                   box(title= "Gráfico 1", status="primary",
                                                       solidHeader=T,  background="aqua", plotOutput("map_mon")),
                                                   box(title= "Gráfico 2",status="primary", solidHeader=T,
                                                       background="aqua",plotOutput("map_num")))
                              )),
                      
                      tabItem(tabName="raros",
                              fluidRow(
                                column(width = 12,  #De 1 a 12
                                       box(DT::dataTableOutput("table_raros"))
                                )
                              ))
                      
                      ),
                      
                    )
)

server <- function(input
                   , output) {
  
  output$num_dep<-renderPlot({num_dep})
  
  output$mont_dep<-renderPlot({montos_dep})
  
  
  output$num_contr<-renderPlot({
    hist(proveedores2_num$Contratos, main= "Distribución de contratos",
         xlab="contratos por proveedor",ylab="nÃºmero de proveedores", col="purple", breaks=input$n)#Si no pones breaks no podrÃ¡ usarse el slideInput
    #hist(proveedores2_num$Contratos,xlab="contratos por proveedor",ylab="nÃºmero de proveedores",
    #breaks=input$n)              #Vistazo general
  })
  
  
  
  output$info<-renderInfoBox({infoBox("AÃ±adir algo","xxxxx",
                                      icon=icon("bar-chart-o"))})
  output$num<-renderInfoBox({valueBox(count((contr_direc)[28]),"Contratos Analizados", #count function
                                      icon=icon("eye"),color="red")})  #Ahora queremos un value-box
  output$table_raros <- DT::renderDataTable(as.datatable({formattable(df, align =c("c","c","c","c","c","c","c","c"), list( ###con align alineamos: ",align =c("l","c","c","c","c", "c", "c", "c", "r")"
    `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red"),font.weight = "bold"),
                           ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)),
    `Sanciones`= formatter("span", style = ~ style(color = ifelse(`Sanciones` ==0 , "green","red"),font.weight = "bold")),
    `Penalidades`= formatter("span", style = ~ style(color = ifelse(`Penalidades` ==0 , "green","red"),font.weight = "bold"))
    ,`Monto` = color_bar("green")
    #,area(col = 2) ~ color_tile("#DeF7E9", "#71CA97")  #SOLO VALE PARA NÚMEROS
    #,area(col = 4) ~ color_tile("#DeF7E9", "#71CA97")
    #,`TRABAJADORES-AGOSTO` = color_bar("green",fun=unit.scale)
    
  )) ###(condiciÃ³n, dato)
    
  }))
  
  output$map_mon <- renderPlot({map_mon})
  output$map_num <- renderPlot({map_num})
  output$rubros_funnel <- renderPlotly({rubros_funnel})
  
  rubros_funnel  
}

names(df)
shinyApp(ui = ui, server = server, options = list(height = 1080))






