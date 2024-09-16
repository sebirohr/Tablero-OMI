source("procesa_bases.R")

title <- tags$a(
    tags$img(src = "logoOferta.png", class = "header-logo")
  )

dashboardPage(title = "Oferta de alquiler de vivienda (CABA)",
  dashboardHeader(title = title, 
#                  titleWidth = "30%",
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
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tags$div(
      h4("Septiembre 2024"),
      fluidRow(
        column(width = 2, box(title = "Total de unidades", width = NULL, textOutput("general1"), class = "top-box")),
        column(width = 2, box(title = "Var. unidades", width = NULL, textOutput("general2"), class = "top-box")),
        column(width = 2, box(title = "Precio promedio", width = NULL, textOutput("general3"), class = "top-box")),
        column(width = 2, box(title = "Precio mediana", width = NULL, textOutput("general4"), class = "top-box")),
        column(width = 2, box(title = "Dolarizados", width = NULL, textOutput("general5"), class = "top-box")),
        column(width = 2, box(title = "Avisos nuevos", width = NULL, textOutput("general6"), class = "top-box"))
      )
    ),

    # Todas las pestañas dentro de un solo tabItems
    tabItems(
      tabItem(tabName = "tab1",
              fluidRow(
                box(
                  width = 3,
                  shinyWidgets::autonumericInput(
                            inputId  = "inputUserMinPrecio", 
                            label = "Precio minimo de publicacion", 
                            value = minprecio, 
                            align = "left",
                            currencySymbolPlacement = "p",
                            currencySymbol = "$",
                            decimalPlaces = 0,
                            digitGroupSeparator = ".",
                            decimalCharacter = ","),
                  shinyWidgets::autonumericInput(
                            inputId  = "inputUserMaxPrecio", 
                            label = "Precio máximo de publicacion", 
                            value = maxprecio, 
                            align = "left",
                            currencySymbolPlacement = "p",
                            currencySymbol = "$",
                            decimalPlaces = 0,
                            digitGroupSeparator = ".",
                            decimalCharacter = ","),
                  radioButtons("intensityOption",
                               label = "Opción de Intensidad",
                               choices = c("Cantidad", "Precio"),
                               selected = "Cantidad"),
                  radioButtons("dataOption", "Propiedades a mostrar",
                               choices = c("Todas las propiedades" = "todas",
                                           "Solo propiedades en pesos" = "pesos",
                                           "Solo nuevas propiedades" = "nuevas"))
                ),
                box(
                  width = 9,
                  h2("Mapa de anuncios de alquiler"),
                  leafletOutput("mapa")
                )
              )
      ),
      
      tabItem(tabName = "tab2",
              fluidRow(
                box(
                  width = 3,
                  selectInput("data_tabla", 
                              label = "Seleccione una variable:",
                              choices = c("Zona", "Barrio", "Ambientes", "Rango precio", "Nuevas por barrio"),
                              selected = "Barrio")
                ),
                box(
                  width = 9,
                  DT::dataTableOutput("tabla")
                )
              )
      ),
      
      tabItem(tabName = "tab3",
              fluidRow(
                box(
                  width = 3,
                  class = "side-box",
                  radioButtons("VariableMapeada",
                               label = "Opción de Intensidad:",
                               choices = c("Cantidad", "Precio"),
                               selected = "Cantidad")
                ),
                box(
                  width = 9,
                  h2("Mapa de avisos por barrio"),
                  leafletOutput("mapabarrio")
                )
              )
      ),
      
      tabItem(tabName = "tab4",
              h2("Aclaraciones metodologicas"),
              tags$div(
                class = "panel-body",
                tags$p("Este es un análisis de los anuncios de alquiler del portal ZonaProp. Esto implica que tanto una disminución como un aumento de las unidades en alquiler no implican en sí mismo un mejor o peor funcionamiento del mercado. Las publicaciones pueden bajar porque mucha gente logró alquilar y se sacan los anuncios o porque muchos propietarios sacan sus unidades de alquiler.
El análisis se refiere entonces a la situación de la oferta de alquiler de viviendas mediante mecanismos de mercado no incluyendo otras modalidades de contrato y búsqueda."),
                tags$p("La ubicación de las viviendas se obtiene con el uso de la API Geopy a partir de la dirección informada por el portal. Ya sea por formato o por falta de información no todos los avisos pueden ser geolocalizados (Aprox. 75%)."),
                tags$p("Los avisos nuevos se obtienen a partir del cruce con el ID de la publicación y características generales con la base del mes anterior."),
                tags$p("Los precios se pasan de dólares a pesos según el valor del MEP en el día de referencia."),
                tags$p("Para el cálculo de promedios se quitan los precios con valores extremos ya que estos configuran un error en la publicación. (Ejemplo de valor alto: viviendas en venta que se publican por error en alquiler; Ejemplo de valor bajo: viviendas que podemos suponer están valuadas en dólares pero no se aclara en la publicación y no lo lee el programa.)")
              ))
              ,
      
      tabItem(tabName = "tab5",
              h2("Resumen ejecutivo"),
              tags$p(
                br(),
                tags$h4(tags$b("Cantidades")),
                tags$b("Histórico"),
                br(),
                "Segundo mes consecutivo de disminución de la oferta. Tercero si se incluye el estancamiento que comenzó en julio (-1%). De noviembre 2023 a junio 2024 la cantidad de unidades publicadas habían subido alrededor de un 200%.",
                br(),
                "El total de unidades publicadas es de 13.018 viviendas lo que representa una disminución en relación al mes anterior del 3%.",
                br(),
                tags$b("Por barrios"),
                br(),
                "Si bien la oferta disminuye, el mercado sigue teniendo dinamismo en algunos segmentos. El 53% de los avisos son nuevos, es decir, no estaban el mes pasado.",
                br(),
                "Aumenta levemente el porcentaje de avisos dolarizados que ya alcanza el 44% de las publicaciones. El barrio con mayor porcentaje de avisos en dólares es Puerto Madero (96%), le siguen Retiro (67%) y Palermo (61%).",
                br(),
                "De los barrios con oferta considerable aumentaron la cantidad de publicaciones en San Nicolás mientras que disminuyeron en Caballito, Almagro, Villa Urquiza y Núñez. En estos casos la disminución de la oferta estuvo acompañada con nuevas unidades en alquiler lo que muestra barrios que, si bien son de un segundo órden de consolidación (por detrás de Palermo, Recoleta, Belgrano), presentan un interesante funcionamiento de mercado por cuarto mes consecutivo.",
                br(),
                "De los barrios con mayor consolidación de oferta la dinámica de cantidades se mantuvo estable a excepción de Puerto Madero que tiene pocas nuevas publicaciones.",
                br(),
                "Si bien el corredor Norte es el que presenta la mayor cantidad de ofertas de alquiler con 8.039 es interesante ver que es la zona con menor porcentaje de nuevas publicaciones, mayor porcentaje de anuncios dolarizados y mayor aumento de precios, lo que supone una oferta consolidada hace un tiempo pero que no presenta tanto dinamismo como otros sectores.",
                br(),
                tags$h4(tags$b("Precios")),
                "Los precios de oferta aumentaron en el último mes. Si bien el promedio muestra un aumento de 4,6%, la mediana alcanzó los $645.750 producto de un aumento del 7,6% respecto del mes anterior, lo que representa valores por encima de la pauta inflacionaria. De todos modos es necesario aclarar que son precios de publicación.",
                br(),
                "Las nuevas unidades tienen una mediana de $580.000, inferior a la mediana general pero superior a la mediana de nuevas unidades del mes anterior que fue $550.000.",
                br(),
                "Palermo y Belgrano presentan aumentos por encima de la media al igual que Villa Urquiza y Villa Crespo. Caballito sostuvo el valor de la mediana del precio de una publicación ($450.000) mientras que Puerto Madero la disminuyó 6,7% debido a la baja del dólar paralelo del último mes.",
                br(),
                "Los departamentos de 2 ambientes presentaron un aumento de precios del 10%, un valor superior al resto de las unidades."
              
              
              )
      )
    )
  )
)
