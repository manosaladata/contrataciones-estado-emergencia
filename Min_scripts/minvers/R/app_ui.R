#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    
    golem_add_external_resources(),
    
    dashboardPage(title="Contratos Directos COVID-19", skin="blue",
                  dashboardHeader(title="CONTENIDO",
                                  tags$li(class = "dropdown",
                                          tags$a(href = "http://www.facebook.com/sharer.php?u=https://abnercasallo.shinyapps.io/Data/", 
                                                 target = "_blank", 
                                                 tags$img( 
                                                   imageOutput("face",height = "20px"))
                                          )
                                  ),
                                  
                                  tags$li(class = "dropdown",
                                          tags$a(href = "http://twitter.com/share?url=https://abnercasallo.shinyapps.io/Data/", 
                                                 target = "_blank", 
                                                 tags$img( 
                                                   imageOutput("twr",height = "20px"))
                                          )
                                  ),
                                  
                                  
                                  tags$li(class = "dropdown",
                                          tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=https://abnercasallo.shinyapps.io/Data/", 
                                                 target = "_blank", 
                                                 tags$img( 
                                                   tags$img( 
                                                     imageOutput("link",height = "20px"))
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
                                  ),
      dashboardSidebar(
        sidebarMenu(                                 
          id="sidebarID",
          menuItem("Información General", tabName="map_mon", icon = icon("arrow-alt-circle-right")), #el tab Name=dep, permite relacionar el grÃ¡fico de dashboardBody
          menuItem("Proveedores",id = "chartsID", 
                   icon = icon("arrow-alt-circle-right"),
                   menuSubItem("Todos los Proveedores", tabName="contr_prove"),
                   menuSubItem("Personas Naturales",tabName = "per_nat"),
                   menuSubItem("Top 100 de Proveedores",icon = shiny::icon("eye"))
          ),
          menuItem("Rubros",id = "chartsID",icon = icon("arrow-alt-circle-right"),
                   menuSubItem("Por Montos"),    #Más icons:https://fontawesome.com/icons?d=gallery
                   menuSubItem("Por Número de Contratos")
          ),
          menuItem("Por entidad",id = "chartsID",icon = icon("arrow-alt-circle-right"), #el tab Name=contract, permite relacionar el histograma
                   menuSubItem("TOP 10 de Entidades"),
                   menuSubItem("Entidades en General")),
          #menuSubItem("Entidades por Monto",tabName="entidt_mon")),
          menuItem("Buscador de RUC",icon = icon("arrow-alt-circle-right")),
          menuSubItem("Buscador del OSCE",icon = icon("arrow-alt-circle-right")),
          menuItem ("Guía Normativa",icon = icon("arrow-alt-circle-right")),
          menuItem("Repositorio de Git-Hub", icon=icon("github-square")),
          menuItem("Agradecimientos",icon = icon("hands")),
          menuItem("Donaciones", icon=icon("hand-holding-heart")),
          textInput("text_input","Contáctenos", value="abner.casallo@unmsm.edu.pe"),
          
          textInput("text_input", "Aclaración", value="La información sobre sanciones y penalidades han sido obtenidas del buscador de proveedores y se actualiza cada cierto periodo (semanal o quincenal). En este sentido, es referencial. Para denuncias y otras cuestiones legales se debe verificar en la página del Buscador de Proveedores. Para cualquier consulta u observación no dude en contactarse con nosotros.
                                ")
        )
    
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "map_mon",
                  fluidPage(
                    box(title="MONTOS POR DEPARTAMENTO",status="primary",
                        solidHeader = T, mod_mod1_ui("mod1_ui_1"),
                        width=8, height=500),
                    box(mod_monto_dep_ui("monto_dep_ui_1"),width=4))),
          tabItem(tabName="per_nat",
                  fluidPage(
                    h1("Nombre"),
                    mod_DT_ui("DT_ui_1"))),
          tabItem(tabName="contr_prove",
                  fluidRow(div(mod_contr_prove_ui("contr_prove_ui_1"),
                               style = "font-size: 100%; width: 100%;overflow-x: scroll"))
                  
          )
  
  
  
  ))
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
      app_title = 'minvers'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

