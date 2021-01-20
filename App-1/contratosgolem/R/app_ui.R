#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import readxl
#' @import plotly
#' @import tidyverse
#' @import formattable
#' @import data.table
#' @import gridExtra
#' @import rsconnect
#' @import tmap
#' @import leaflet
#' @import plotly
#' @import DT
#' @import ggrepel
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = "Basic dashboard",
                      tags$li(class = "dropdown",
                              tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=https://abnercasallo.shinyapps.io/Data/", 
                                     target = "_blank", 
                                     tags$img( 
                                       tags$img( 
                                         imageOutput("link",height = "20px"))
                                     )
                              ))
                      
                      
                      
                      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Widgets", tabName = "widgets", icon = icon("th")),
          menuItem("Maps", tabName = "map_mont")
        )
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "dashboard",
                  fluidRow(
                    box(plotOutput("plot1", height = 250)),
                    
                    box(
                      title = "Controls",
                      sliderInput("slider", "Number of observations:", 1, 100, 50)
                    )
                  )
          ),
          
          # Second tab content
          tabItem(tabName = "widgets",
                  h2("Widgets tab content")
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
          tabItem(
            tabName = "map_mont",
            fluidRow(
              mod_maps_module_ui("maps_module_ui_1")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'contratosgolem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

