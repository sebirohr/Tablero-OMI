source("procesa_bases.R")

source("procesa_bases.R")



dashboardPage(
  dashboardHeader(title = HTML("Situación de la oferta de alquiler de viviendas en CABA"), 
                  disable = FALSE, 
                  dropdownMenu(type = 'message', 
                               messageItem(
                                 from = "sebastianrohr3@gmail.com",
                                 message = "",
                                 icon = icon("envelope"),
                                 href = "mailto:sebastianrohr3@gmail.com"
                               ),
                               icon = icon('comment'))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa de anuncios", tabName = "tab1", icon = icon("map")),
      menuItem("Cuadros por características", tabName = "tab2", icon = icon("chart-bar")),
      menuItem("Mapa por barrios", tabName = "tab3", icon = icon("map-marker-alt")),
      menuItem("Aclaraciones metodológicas", tabName = "tab4", icon = icon("book")),
      menuItem("Resumen ejecutivo", tabName = "tab5", icon = icon("clipboard-list"))
          )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$div(
      h4("Julio 2024"),
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
                               min = minprecio, 
                               max = maxprecio, 
                               value = c(0, 100000000), 
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
                     leafletOutput("mapabarrio"))))
      ,tabPanel("Metodología",
                h2("Aclaraciones metodologicas"),
                tags$div(
                  class = "panel-body",
                  tags$p("La información surge de la extracción de datos mediante WebScrapping del portal ZonaProp"),
                  tags$p("La ubicación de las viviendas se obtiene con el uso de la API Geopy a partir de la dirección informada por el portal. Ya sea por formato o por falta de información no todos los avisos pueden ser geolocalizados (Aprox. 75%)"),
                  tags$p("Los avisos nuevos se obtienen a partir del cruce con el ID de la publicación con la base del mes anterior."),
                  tags$p("Los precios se pasan de dólares a pesos según el valor del MEP en el día de referencia."),
                  tags$p("Para el cálculo de promedios se quitan los precios con valores extremos ya que estos configuran un error en la publicación. (Ejemplo de valor alto: viviendas en venta que se publican por error en alquiler; Ejemplo de valor bajo: viviendas que podemos suponer están valuadas en dólares pero no se aclara en la publicación y no lo lee el programa.)")
                )
    )
  )
)
)
)
)
)

