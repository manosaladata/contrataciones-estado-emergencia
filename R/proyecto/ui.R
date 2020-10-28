#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          textInput("ingresar_datos","ingrese datos requeridos"),
          textInput("ingresar_nombres","Ingrese nombres requeridos"),
          radioButtons("lug", "pregunta",choices = c("opción a","opción b"), selected="valor default",
                       inline = T), #inline alunea las opciones
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("datos"),  #vinculado a lo que entre en el sidebarPanel
          textOutput("nombres"),
            #plotOutput("distPlot")
          textOutput("loc")      #está vinculado al slidebrLyout vía "lug" en el server.
        )
    )
))
