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
                   menuSubItem("Top 100 de Proveedores",tabName= "raros", icon = shiny::icon("eye"))
          ),
          menuItem("Rubros",id = "chartsID",icon = icon("arrow-alt-circle-right"),
                   menuSubItem("Por Montos", tabName = "rubros_funnel"),    #Más icons:https://fontawesome.com/icons?d=gallery
                   menuSubItem("Por Número de Contratos", tabName = "rubros_funnel_n")
          ),
          menuItem("Por entidad",id = "chartsID",icon = icon("arrow-alt-circle-right"), #el tab Name=contract, permite relacionar el histograma
                   menuSubItem("TOP 10 de Entidades", tabName = "entnum"),
                   menuSubItem("Entidades en General", tabName = "entotal_num")),
          #menuSubItem("Entidades por Monto",tabName="entidt_mon")),
          menuItem("Buscador de RUC",tabName = "link_sunat" ,icon = icon("arrow-alt-circle-right")),
          menuSubItem("Buscador del OSCE", tabName="detalle",icon = icon("arrow-alt-circle-right")),
          menuItem ("Guía Normativa", tabName="derecho",icon = icon("arrow-alt-circle-right")),
          menuItem("Repositorio de Git-Hub", tabName = "Git-Hub", icon=icon("github-square")),
          menuItem("Agradecimientos",tabName="gracias", icon = icon("hands")),
          menuItem("Donaciones", tabName="dona", icon=icon("hand-holding-heart")),
          textInput("text_input","Contáctenos", value="abner.casallo@unmsm.edu.pe"),
          
          textInput("text_input", "Aclaración", value="La información sobre sanciones y penalidades han sido obtenidas del buscador de proveedores y se actualiza cada cierto periodo (semanal o quincenal). En este sentido, es referencial. Para denuncias y otras cuestiones legales se debe verificar en la página del Buscador de Proveedores. Para cualquier consulta u observación no dude en contactarse con nosotros.
                                ")
        )
    
      ),
      dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "map_mon",
                  div(style="font-size: 100%; width:100%;overflow-x: scroll",
                      
                      fluidRow(
                        column(width=8,  
                               valueBox("9 Meses","Periodo: Marzo-Diciembre",icon=icon("hourglass-3"),color="yellow"),
                               valueBoxOutput("num"),
                               #valueBox("xx", "Monto Total", color = "green"),
                               valueBoxOutput("monto")
                               #infoBoxOutput("info"),
                               
                        )
                        ,column(width = 4,
                                imageOutput("manos", width="50%",height="150px")
                        )),
                  fluidRow(
                    
                    box(title="MONTOS POR DEPARTAMENTO",status="primary",
                        solidHeader = T, mod_mod1_ui("mod1_ui_1"),
                        width=8, height=500),
                    box(mod_monto_dep_ui("monto_dep_ui_1"),width=4)))),
          tabItem(tabName="per_nat",
                  fluidPage(
                    h1("Nombre"),
                    mod_DT_ui("DT_ui_1"))),
          tabItem(tabName="contr_prove",
                  fluidRow(div(mod_contr_prove_ui("contr_prove_ui_1"),
                               style = "font-size: 100%; width: 100%;overflow-x: scroll"))
                  
          ),
          tabItem(tabName = "raros",
                  mod_raros_ui("raros_ui_1")),
          tabItem(tabName = "rubros_funnel",
                  mod_plotly_ui("plotly_ui_1")),
          tabItem(tabName = "rubros_funnel_n",
                  mod_funnel_n_ui("funnel_n_ui_1")),
          tabItem(tabName = "entnum",
                  mod_ggplot_ui("ggplot_ui_1"),
                  mod_ggplot2_ui("ggplot2_ui_1"))
          # ,tabItem(tabName = "entmon",
          #         mod_ggplot2_ui("ggplot2_ui_1"))
          ,tabItem(tabName = "entotal_num",
            mod_ent_dt_mon_ui("ent_dt_mon_ui_1")
          ),
          tabItem(
            tabName = "link_sunat",
            fluidRow(
              div(tags$iframe(
                seamless = "seamless", 
                src = "https://e-consultaruc.sunat.gob.pe/cl-ti-itmrconsruc/frameCriterioBusqueda.jsp", 
                height = 800, width = 1000
              ),style = "font-size: 100%; width: 100%;overflow-x: scroll")
            )),
          tabItem(tabName = "detalle",
                  h1("BUSCADOR DE PROVEEDORES",style="font-family:Impact"),
                  p(style="font-family:Impact", 
                    "¿Algún proveedor le llamó la atención? Para una búsqueda detallada puede acceder
                                al buscador de proveedores del Osce:",
                    a("BUSCADOR DE PROVEEDORES",
                      href="https://apps.osce.gob.pe/perfilprov-ui/"))
                  
          ),
          tabItem(tabName="dona",
                  h1("DONACIONES",style = "font-family:Impact"),
                  p(style="font-family:Georgia","Puedes apoyar a mantener este proyecto en la web
                                donando a través de",a("Buymeacoffe", href="https://www.buymeacoffee.com/AbnerCasallo"),
                    "o la siguiente cuenta...")),
          tabItem(tabName="derecho",
                  fluidRow(tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "derecho.css")
                  ),
                  includeHTML("www/derecho.html"))
          ),
          tabItem(tabName = "Git-Hub",
                  h1("PROYECTO OPEN SOURCE",style = "font-family:Impact, cursive; font-weight: 500; line-height: 1.1; 
                                 color:indigo;"),
                  p(style="font-family:Impact", 
                    "Usted puede contrubuir con nosotros a través de nuestro repositorio de",
                    a("Git-Hub",
                      href="https://github.com/manosaladata/contrataciones-estado-emergencia.git"))
                  
          ),
          tabItem(tabName="gracias",
                  h1("AGRADECIMIENTOS",style = "font-family:Impact"),
                  p(style="font-family:Georgia", 
                    "Este proyecto no hubiese sido posible sin el apoyo de muchas personas.
                                En particular un agradecimiento al equipo de Manos a la Data, es especial a Arturo Chian",
                    # a("Arturo Chian",href="https://arturochian.com/"), 
                    "por el apoyo constante durante todo este periodo.
                                Asimismo, un agracedimiento especial a Luigi Castro por sus valiosos insights en temas
                                de Contración Pública.")
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

