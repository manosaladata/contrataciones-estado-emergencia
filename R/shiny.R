library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(formattable)

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"


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




ui <- dashboardPage(title="Proyecto", skin="red", #da color al encabezado y nombre a la página (cuando abres con el explordor se nota)
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
                                                   "Avance de gráficos y tablas"
                                                 ),
                                                 taskItem(
                                                   value=10,
                                                   color="red",
                                                   "automatización"
                                                 ))),
                    
                    dashboardSidebar(
                      #sliderInput(inputId = "n",                   #En el dashboardsiderbar no van box, queda feo si lo pones.                        
                      # "Number of contracts",
                      # 1,100,50),
                      sidebarMenu(                                 #Esto permitirá que todo sea visto como un menú y se pueda abrir una nueva ventana por cada item.
                        sidebarSearchForm("searchText","buttonSearch","Search"),
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
                        menuSubItem("Prácticas poco comunes", tabName = "raros"),
                        menuItem("Nuevo", badgeLabel = "New", badgeColor ="green" ),
                        textInput("text_input","Contáctenos al:", value="xxxxxx@gmail.com")
                      )),
                    
                    dashboardBody(                        #Esta parte dará el contenido (podría ir arriba, pero sale desordenado)
                      tabItems(tabItem(tabName = "dep",
                                       fluidRow(
                                         column(width=9,  #column da tamaño para todos sus , acá daremos 9 a todos
                                                infoBox("Transparencia","100%",icon=icon("thumbs-up")),
                                                infoBox("Dato abiertos", "100%"),
                                                infoBoxOutput("info")  #otro forma de introducir trabajando con server
                                         )),
                                       
                                       fluidRow(valueBox("7 meses","Periodo de análisis",icon=icon("hourglass-3"),
                                                         color="yellow"),
                                                valueBoxOutput("num")),
                                       
                                       fluidRow(
                                         box(title= "Gráfico 1", status="primary"
                                             ,solidHeader=T,  background="aqua", plotOutput("salida2"))
                                       )
                                       
                      ),
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
                                box(title="Controles",status="warning",solidHeader=T,background = "red",
                                    "USAR PARA ACOMODAR GRÁFICO", br(),"SI ES CONFUSO NO, USAR", #br() da espacio
                                    sliderInput(inputId = "n",
                                                "Números de contratos por proveedor",
                                                1,80,40),    #c.11: se puede usar tabpanel para meter esto a la imagen como una opción.
                                    textInput("text_input","Anotaciones", value="ingrese sus anotaciones"))
                                
                              )),
                      tabItem(tabName="raros",
                              fluidRow(
                                column(width = 12,
                                       box(formattableOutput("table_raros"))
                                )
                              ))
                      
                      ),
                      
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
  output$info<-renderInfoBox({infoBox("Añadir algo","xxxxx",
                                      icon=icon("bar-chart-o"))})
  output$num<-renderInfoBox({valueBox(count((contr_direc)[28]),"Contratos Analizados", #count function
                                      icon=icon("eye"),color="red")})  #Ahora queremos un value-box
  output$table_raros <- renderFormattable({formattable(proveedores2_num, list( ###con align alineamos: ",align =c("l","c","c","c","c", "c", "c", "c", "r")"
    `MONTOADJSOLES`= color_tile(customGreen, customGreen0),
    `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red")),
                           ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)))) ###(condición, dato)
    
  })
  
  
}


shinyApp(ui, server)

