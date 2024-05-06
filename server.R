
source("Ayudas Tablero OMI.R")

function(input, output) {
  

  output$mapa <- renderLeaflet({
    data <- Zonaprop_Mapa %>% filter(
      PrecioPesos > input$sliderPrecio[1],
      PrecioPesos < input$sliderPrecio[2]
    )
    
    leaflet() %>%
      addTiles() %>%
      addHeatmap(data = data,
                 lng = ~as.numeric(Longitud), lat = ~as.numeric(Latitud),
                 intensity = if(input$intensityOption == "Cantidad") { ~1 } else { ~PrecioPesos },  
                 blur = if(input$intensityOption == "Cantidad") { ~5 } else { ~5 },  
                 max = if(input$intensityOption == "Cantidad") { ~5 } else { ~10000000 },  
                 radius = 5)
  })
  
  
  output$general1 <- renderText({
    format(Generales$Total, big.mark = ".", scientific = FALSE)
  })
  
  output$general2 <- renderText({
    paste0("$", format(Generales$`Promedio precio`, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."))
  })
  
  output$general3 <- renderText({
    paste0("$", format(Generales$`Mediana precio`, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."))
  })
  
  output$general4 <- renderText({
    paste0(format(100*Generales$`Porcentaje dolarizado`, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%")
  })


  output$tabla <- renderDataTable({
    # Definir cómo se muestra la tabla en función de la base de datos seleccionada
    if (input$data_tabla == "Zona") {
      return(dt_zona)
    } else if (input$data_tabla == "Barrio") {
      return(dt_barrio)
    } else if (input$data_tabla == "Ambientes") {
      return(dt_ambientes)
    } else if (input$data_tabla == "Rango precio") {
      return(dt_rangoprecio)
    }
  })
  
  
  output$mapabarrio <- renderLeaflet({
    
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = Resumen_CABA, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                  color = if(input$VariableMapeada == "Cantidad") {~pal(log10(Resumen_CABA$Total)) } else { ~pal(log10(Resumen_CABA$Precio)) },
                  opacity = 1,
                  popup = paste("<a><strong>", Resumen_CABA$BARRIO,"</strong></a><br>",
                                "Propiedades: ", Resumen_CABA$Total, "<br>",
                                "Precio promedio: ", Resumen_CABA$`Promedio precio`, "<br>",
                                "% Dolarizado: ", Resumen_CABA$`Porcentaje dolarizado`, "<br>")) %>%
      addLegend("topright", pal = pal,
                values = if(input$VariableMapeada == "Cantidad") { Resumen_CABA$Total } else { Resumen_CABA$Precio },
                title = if(input$VariableMapeada == "Cantidad") {"Cantidad de viviendas" } else { "Precio promedio" }  )
  })
    
    
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Mapa de publicaciones", tabName = "mapapublicaciones", icon = icon('dashboard')),
      menuItem("Tabla por características", tabName = "tabla", icon = icon('question-circle')),
      menuItem("Contacto", tabName = "contacto", icon = icon('bell'))
    )
  })
  
output$selected_tab_content <- renderUI({
  switch(input$menu,
         "mapapublicaciones" = leafletOutput("mapa"),
         "tabla" = dataTableOutput("tabla")
  )
})
}

