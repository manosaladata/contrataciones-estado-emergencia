library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)


setwd("D:/GITHUB-PROYECTOS BEST/contrataciones-estado-emergencia/Data")
contr_direc <- read_excel("CONOSCE_CONTRATACIONDIRECTA.xlsx")
#names(contr_direc)
contr_direc[,28]<-sapply(contr_direc[,28],function(x)x/1000000)
names(contr_direc)[28]="MONTO_SOLES_EN_MILLONES"
names(contr_direc)[31]="RUCPROV"
#names(contr_direc)
options(scipen=999)                                  #Evita que salga en notación científica (exponencial).
#sapply(contr_direc, class)      


proveedores<- select(contr_direc, "PROVEEDOR","ENTIDAD", "RUCPROV", "TIPOPROVEEDOR","MONTO_SOLES_EN_MILLONES")
proveedores2_num<-group_by(proveedores,PROVEEDOR,RUCPROV)
proveedores2_num<-summarize(proveedores2_num,MONTOADJSOLES= sum(MONTO_SOLES_EN_MILLONES), Contratos=n())
proveedores2_num<-arrange(proveedores2_num,desc(Contratos))




ui <- dashboardPage(
  #numericInput("ENTIDAD_DEPARTAMENTO",ENTIDAD_DEPARTAMENTO),
  dashboardHeader(title="EXPLORADOR",
                  dropdownMenu(type="message",   #También se pueden trabajar con mensajes dinámicos con dropdownMenuOutput() usando un csv.
                               messageItem(from="Abner", message="Bienvenido"),
                               messageItem(from="Abner", message="Proyecto al 50%", icon=icon("bar-chart"), time = "21:00"),
                               messageItem(from="Abner", message="Trabajo parte del Proyecto Manos a la Data", icon=icon("vcard"), time = "10-10-2020")),
                  dropdownMenu(type="notifications",        #para todos los inputs dentro de deopdownMenu, también se puede automatizar con un csv.
                               notificationItem(
                                 text="Esperamos que les sirva",
                                 icon=icon("dashboard"),
                                 status="success"),
                               notificationItem(
                                 text="Base de datos aún no actualizada",
                                 icon=icon("warning"),
                                 status="warning"))),
  dashboardSidebar(
                   #sliderInput(inputId = "n",                   #En el dashboardsiderbar no van box, queda feo si lo pones.                        
                                  # "Number of contracts",
                                   # 1,100,50),
                   sidebarMenu(                                 #Esto permitirá que todo sea visto como un menú y se pueda abrir una nueva ventana por cada item.
    
                   menuItem("Análisis departamental", tabName="dep", icon = icon("arrow-alt-circle-right")), #el tab Name=dep, permite relacionar el gráfico de dashboardBody
                             menuSubItem("Orden por número de contratos"),                                   #Más icons:https://fontawesome.com/icons?d=gallery 
                             menuSubItem("Orden por monto contratado"),
                   menuItem("Información de contratos por entidad",tabName = "entidad",icon = icon("arrow-alt-circle-right")), #el tab Name=contract, permite relacionar el histograma
                             menuSubItem("Orden por número de contratos"),
                             menuSubItem("Orden por monto contratado"),
                   menuItem("Información general de los proveedores",tabName = "hist",icon = icon("arrow-alt-circle-right")),
                             menuSubItem("Proveedores con mayores contratos"),
                             menuSubItem("Información de SUNAT"),
                   menuItem("Casos de alerta",icon = icon("dashboard")),
                             menuSubItem("Proveedores con sanciones previas"),
                             menuSubItem("Prácticas poco comunes")
                   )),
  
  dashboardBody(                        #Esta parte dará el contenido (podría ir arriba, pero sale desordenado)
    tabItems(tabItem(tabName = "dep",
                     fluidRow(
                       box(plotOutput("salida2"))
                       #box(sliderInput(inputId = "n",
                                       #"Number of contracts",
                                       #1,100,50))
                       
                     )),
            tabItem(tabName = "entidad", h1("Entidades")),#,
                     #fluidRow(
                       #box(plotOutput("salida2")),
                       #box(sliderInput(inputId = "n",
                       #"Number of contracts",
                       #1,100,50))
                       
                     #))),
              tabItem(tabName = "hist",
                     fluidRow(
                       box(plotOutput("num_contr")),
                       box(sliderInput(inputId = "n",
                                       "Number of contracts por proveedor",
                                       1,80,40))
                       
                     ))),
    
  )
)

server <- function(input
                  , output) {
  
  output$salida2<-renderPlot({
    ggplot(contr_direc, mapping=aes(x =ENTIDAD_DEPARTAMENTO)) + coord_flip()+ stat_count (width = 0.7) +
      theme(   axis.title = element_text( face = "bold" ),
               title=element_text(face = "bold" ),
               legend.text=element_text(size=80)) + labs(title = "Contratos por departamentos"                            ,
                                                         caption = "Fuente: OSCE",
                                                         x="Departamentos", y="Cantidad de contratos")
    #Vistazo general
  })
  
  output$num_contr<-renderPlot({
    hist(proveedores2_num$Contratos, main= "Distribución de contratos",
         xlab="contratos por proveedor",ylab="número de proveedores", col="purple", breaks=input$n)#Si no pones breaks no podrá usarse el slideInput
    #hist(proveedores2_num$Contratos,xlab="contratos por proveedor",ylab="número de proveedores",
         #breaks=input$n)              #Vistazo general
  })
 
}
  

shinyApp(ui, server)

