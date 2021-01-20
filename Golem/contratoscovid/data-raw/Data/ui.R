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
#setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data") #Obs
#load("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data/Datos_consolidados.RData")
#remove(list=setdiff(ls(), "DATA_CONSOLIDADA_120"))  
DATA_CONSOLIDADA_120 <- read.csv(file = 'DATA_CONSOLIDADA_120.csv', sep = ";")
###############FUCIONES#########################
unit.scale = function(x) (x - min(x)) / (max(x) - min(x))
redondeo <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=2)))

df<-DATA_CONSOLIDADA_120
#names(df)
names(df)<-c("Proveedor","RUC","Inscripción","Trabajadores","Monto",
             "Contratos","Sanciones","Penalidades")

df[,5]<-sapply(df[,5],redondeo)

####CARGAMOS DATA DE CONOSCE
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)  #No aplicar redondeo
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROV"
# #names(contr_direc)
options(scipen=999)                                 
contr_direc[28]


ui <- dashboardPage(title="Contratos Directos COVID-19", skin="blue",  #Color del encabezado y nombre a la página (cuando abres con el explordor se nota)
                    #numericInput("ENTIDAD_DEPARTAMENTO",ENTIDAD_DEPARTAMENTO),
                    dashboardHeader(title="CONTENIDO",
                                    tags$li(class = "dropdown",
                                            tags$a(href = "http://www.facebook.com/sharer.php?u=https://behavioraleconomicsdatascienceteam.shinyapps.io/Terrorismo-Global/", 
                                                   target = "_blank", 
                                                   tags$img( 
                                                     imageOutput("facebook",height = "20px"))
                                            )
                                    ),
                                    
                                    tags$li(class = "dropdown",
                                            tags$a(href = "http://twitter.com/share?url=https://behavioraleconomicsdatascienceteam.shinyapps.io/Terrorismo-Global/", 
                                                   target = "_blank", 
                                                   tags$img( 
                                                     imageOutput("twitter",height = "20px"))
                                            )
                                    ),
                                    
                                    tags$li(class = "dropdown",
                                            tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=https://behavioraleconomicsdatascienceteam.shinyapps.io/Terrorismo-Global/", 
                                                   target = "_blank", 
                                                   tags$img( 
                                                     tags$img( 
                                                       imageOutput("linkedin",height = "20px"))
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
                                 menuSubItem("Entidades por contratos", tabName = "entidt_num"),
                                 menuSubItem("Entidades por Monto",tabName="entidt_mon")
                        ),
                        menuItem("Buscador de RUC", tabName = "link_sunat",icon = icon("arrow-alt-circle-right")),
                        menuSubItem2("Buscador del OSCE",tabName = "detalle",icon = icon("arrow-alt-circle-right")),
                        menuItem("Repositorio de Git-Hub",tabName = "Git-Hub", icon=icon("github-square")),
                        textInput("text_input","Contáctenos", value="abner.casallo@unmsm.edu.pe"),
                        
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
                                                valueBox("9 Meses","Periodo: Marzo-Diciembre",icon=icon("hourglass-3"),color="yellow"),
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
                                       
                              )),
                      tabItem(tabName = "detalle",
                              h1("BUSCADOR DE PROVEEDORES",style="font-family:Impact"),
                              p(style="font-family:Impact", 
                                "¿Algún proveedor le llamó la atención? Para una búsqueda detallada puede acceder
                                al buscador de proveedores del Osce:",
                                a("BUSCADOR DE PROVEEDORES",
                                  href="https://apps.osce.gob.pe/perfilprov-ui/"))
                              
                      ),
                      tabItem(tabName = "Git-Hub",
                              h1("PROYECTO OPEN SOURCE",style="font-family:Impact"),
                              p(style="font-family:Impact", 
                                "Usted puede contrubuir con nosotros a través de nuestro repositorio de",
                                a("Git-Hub",
                                  href="https://github.com/manosaladata/contrataciones-estado-emergencia.git"))
                              
                      ),
                      tabItem(tabName="contr_prove",
                              fluidRow(DT::dataTableOutput("contr_prove"))
                              
                      ),
                      tabItem(
                        tabName = "link_sunat",
                        fluidRow(
                          tags$iframe(
                            seamless = "seamless", 
                            src = "https://e-consultaruc.sunat.gob.pe/cl-ti-itmrconsruc/FrameCriterioBusquedaMovil.jsp", 
                            height = 800, width = 1000
                          )
                        )),
                      tabItem(tabName = "per_nat",
                              fluidRow(DT::dataTableOutput("per_nat")))
                      
                      )
                    ))