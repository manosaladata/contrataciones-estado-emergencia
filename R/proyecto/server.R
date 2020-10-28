#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$datos<-{(renderText(input$ingresar_datos))}         #vinculamos el sidebarPanel con el ingresar_datos
    output$nombres<-{(renderText(input$ingresar_nombres))}     #vinculamos el sidebarPanel con el ingresar_nombres

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        

    })
    output$loc <-renderText(input$lug)
    

})
