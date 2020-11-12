library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(formattable)

#ó
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"


setwd("D:/Git Hub-BEST/contrataciones-estado-emergencia/Data")
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

#CREACI?N DE FUNCIONES
#USANDO EL API DE SUNAT:
library(httr)
library(jsonlite)

#RUCS
#IDEA: QUE EL USUARIO PONGA "INGRESE RUC" Y SALGA EL RESULTADO
#Function
sunat<- function(x){
   RUC_str<-as.character(x)
   url1<-paste("https://api.sunat.cloud/ruc/",RUC_str,sep = "", collapse = NULL)
   url1
   res<- GET(url1)
   data<-fromJSON(content(res, type="text", encoding = "UTF-8"))
   razon_social<-data[["razon_social"]]
   empleados<-data[["empleados"]]
   fecha_inscripcion<-data[["fecha_inscripcion"]]
   representante_legal<-data[["representante_legal"]]
   representante_legal_name<-representante_legal[[1]][["nombre"]]
   trabajadores_agosto<-empleados[["2020-08"]][["trabajadores"]]
   y <- data.frame("Nombre de la empresa"=razon_social, "Nombre del representante legal(agosto)" = representante_legal_name,
                   "Trabajadores(agosto-2020)" = trabajadores_agosto,"fecha de inscripción"=fecha_inscripcion)
   #x #para ver de una vez
   #view(x)
 }
 #Trabajadores
trabajadores<-function(x){
   a<-sunat(x)
   b<-a[1,3]
   as.numeric(b)
 }

 #Testeo:
#trabajadores(20555589574)

#representante
representante<-function(x){
   a<-sunat(x)
   b<-a[1,2]
   b
 }
 #Testeo:
 #representante(20555589574)

# #fecha de inscripci?n
 fecha<-function(x){
   a<-sunat(x)
   b<-a[1,4]
   b
 }

 #fecha(20555589574)

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
                        menuItem("AnÃ¡lisis departamental", tabName="dep", icon = icon("arrow-alt-circle-right")), #el tab Name=dep, permite relacionar el grÃ¡fico de dashboardBody
                        menuSubItem("Orden por nÃºmero de contratos"),                                   #MÃ¡s icons:https://fontawesome.com/icons?d=gallery 
                        menuSubItem("Orden por monto contratado"),
                        menuItem("InformaciÃ³n de contratos por entidad",tabName = "entidad",icon = icon("arrow-alt-circle-right")), #el tab Name=contract, permite relacionar el histograma
                        menuSubItem("Orden por nÃºmero de contratos"),
                        menuSubItem("Orden por monto contratado"),
                        menuItem("InformaciÃ³n general de los proveedores",tabName = "hist",icon = icon("arrow-alt-circle-right")),
                        menuSubItem("Proveedores con mayores contratos"),
                        menuSubItem("InformaciÃ³n de SUNAT"),
                        menuItem("Casos de alerta",icon = icon("dashboard")),
                        menuSubItem("Proveedores con sanciones previas"),
                        menuSubItem("PrÃ¡cticas poco comunes", tabName = "raros"),
                        menuItem("Nuevo", badgeLabel = "New", badgeColor ="green" ),
                        textInput("text_input","ContÃ¡ctenos al:", value="xxxxxx@gmail.com")
                      )),
                    
                    dashboardBody(                        #Esta parte darÃ¡ el contenido (podrÃ­a ir arriba, pero sale desordenado)
                      tabItems(tabItem(tabName = "dep",
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
                                         box(title= "GrÃ¡fico 1", status="primary"
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
                                    "USAR PARA ACOMODAR GRÃFICO", br(),"SI ES CONFUSO NO, USAR", #br() da espacio
                                    sliderInput(inputId = "n",
                                                "NÃºmeros de contratos por proveedor",
                                                1,80,40),    #c.11: se puede usar tabpanel para meter esto a la imagen como una opciÃ³n.
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
    hist(proveedores2_num$Contratos, main= "DistribuciÃ³n de contratos",
         xlab="contratos por proveedor",ylab="nÃºmero de proveedores", col="purple", breaks=input$n)#Si no pones breaks no podrÃ¡ usarse el slideInput
    #hist(proveedores2_num$Contratos,xlab="contratos por proveedor",ylab="nÃºmero de proveedores",
    #breaks=input$n)              #Vistazo general
  })
  output$info<-renderInfoBox({infoBox("AÃ±adir algo","xxxxx",
                                      icon=icon("bar-chart-o"))})
  output$num<-renderInfoBox({valueBox(count((contr_direc)[28]),"Contratos Analizados", #count function
                                      icon=icon("eye"),color="red")})  #Ahora queremos un value-box
  output$table_raros <- renderFormattable({formattable(proveedores2_num, list( ###con align alineamos: ",align =c("l","c","c","c","c", "c", "c", "c", "r")"
    `MONTOADJSOLES`= color_tile(customGreen, customGreen0),
    `Contratos`= formatter("span", style = ~ style(color = ifelse(`Contratos` <= 3, "green","red")),
                           ~ icontext(ifelse(`Contratos` <= 3, "thumbs-up", "thumbs-down"),`Contratos`)))) ###(condiciÃ³n, dato)
    
  })
  
  
}


shinyApp(ui, server)



