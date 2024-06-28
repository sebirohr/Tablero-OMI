
source("procesa_basesZP.R")



dashboardPage(
  dashboardHeader(title = HTML("Situación de la oferta de alquiler de viviendas en CABA"), 
                  disable = FALSE, 
                  titleWidth  = "50%",
                  dropdownMenu(type = 'message', 
                               messageItem(
                                 from = "sebastianrohr3@gmail.com",
                                 message = "",
                                 icon = icon("envelope"),
                                 href = "mailto:sebastianrohr3@gmail.com"
                               ),
                               icon = icon('comment'))#,
                  #dropdownMenu(
                   #   type = 'message',
                    #  icon = icon("share-alt"),
                     # messageItem(
                      #  from = 'LinkedIn',
                       # message = "",
                        #icon = icon("linkedin"),
                        #href = "http://www.linkedin.com/shareArticle?mini=true&url=https://observatoriovivienda.shinyapps.io/Tablero-OMI/"
  #                  )
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$div(
      h4("Junio 2024"),
    tabPanel("Mapa de publicaciones",
             includeCSS("styles.css"),
             
             
    box(title = "Total de unidades" , width = 2,
        textOutput("general1")),
    box(title = "Variación unidades", width = 2, 
        textOutput("general2")),
    box(title = "Precio promedio", width = 2, 
        textOutput("general3")),
    box(title = "Precio mediana", width = 2,  
        textOutput("general4")),
    box(title = "Dolarizados", width = 2, 
        textOutput("general5")),
    box(title = "Avisos nuevos", width = 2, 
        textOutput("general6")),
    
    tabsetPanel(
      tabPanel("Mapa de anuncios inmobiliarios",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("sliderPrecio", 
                               label = 'Rango de precios',
                               min = 0, 
                               max = 10000000, 
                               value = c(0, 100000000),  # Cambiado para coincidir con el rango completo
                               step = 1,
                               sep = ".",
                               pre = "$"),
                   radioButtons("intensityOption",
                                label = "Opción de Intensidad:",
                                choices = c("Cantidad", "Precio"),
                                selected = "Cantidad")
                 ),
                 mainPanel(
                   h2("Mapa de anuncios de alquiler"),
                   leafletOutput("mapa")
                 )
               )
      ),
      tabPanel("Resumen por características",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("data_tabla", 
                               label = "Seleccione una variable:",
                               choices = c("Zona", "Barrio", "Ambientes", "Rango precio"),
                               selected = "Barrio")
                 ),
                 mainPanel(
                   DT::dataTableOutput("tabla")
                 )
               ))
      ,tabPanel("Resumen por barrio (Mapa)",
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("VariableMapeada",
                                  label = "Opción de Intensidad:",
                                  choices = c("Cantidad", "Precio"),
                                  selected = "Cantidad")
                   ), mainPanel(
                     h2("Mapa de avisos por barrio"),
                     leafletOutput("mapabarrio")
      
    )
)
)
)
)
)
)
)