
source("Ayudas Tablero OMI.R")



dashboardPage(
  skin = "yellow",
  dashboardHeader(title = HTML("Observatorio del Mercado inmobiliario"), 
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
    tags$div(
      h4("Monitor de alquileres. Mayo 2024"),
    tabPanel("Mapa de publicaciones",
             includeCSS("styles.css"),
             
             
    box(title = "Total de unidades" , width = 3,
        textOutput("general1")),
    box(title = "Precio promedio", width = 3, 
        textOutput("general2")),
    box(title = "Precio mediana", width = 3,  
        textOutput("general3")),
    box(title = "Porcentaje dolarizados", width = 3, 
        textOutput("general4")),
    
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
                   dataTableOutput("tabla")
                 )
               )
      ),tabPanel("Resumen por barrio (Mapa)",
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