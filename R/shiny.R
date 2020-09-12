library(shiny)
library(shinydashboard)
library(readxl)


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
  dashboardHeader(title="EXPLORADOR"),
  dashboardSidebar(
                   sliderInput(inputId = "n",                   #En el dashboardsiderbar no van box, queda feo si lo pones.                        
                                   "Number of contracts",
                                   1,100,50),
                   menuItem("Información de contratos por entidad", tabName = "contr"),
                             menuSubItem("Orden por número de contratos"),
                             menuSubItem("Orden por monto contratado"),
                   menuItem("Información general de los proveedores"),
                             menuSubItem("Proveedores con mayores contratos"),
                             menuSubItem("Información de SUNAT"),
                   menuItem("Casos de alerta"),
                             menuSubItem("Proveedores con sanciones previas"),
                             menuSubItem("Prácticas poco comunes")
                   ),
  
  dashboardBody(
    tabItems(tabItem(tabName = "contr",
                     fluidRow(
                       box(plotOutput("salida")),
                       box(sliderInput(inputId = "n",
                                       "Number of contracts",
                                       1,100,50))
                       
                     )))
  )
)
server <- function(input
                  , output) {
  
  output$salida<-renderPlot({
    hist(proveedores2_num$Contratos,xlab="contratos por proveedor",ylab="número de proveedores",
         breaks=input$n)              #Vistazo general
  })
}
  

shinyApp(ui, server)

