
source("procesa_bases.R")

function(input, output) {
  

  output$mapa <- renderLeaflet({
    data <- df_map %>% filter(
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
    format(sumGenerales$Total0, big.mark = ".", scientific = FALSE)
  })
  
  output$general2 <- renderText({
    paste0(format(100*sumGenerales$dif_total, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%")
  })
  
  output$general3 <- renderText({
    paste0("$", format(sumGenerales$promedioP0, nsmall = 2, decimal.mark = ",", big.mark = "."))
  })
  
  output$general4 <- renderText({
    paste0("$", format(sumGenerales$medianaP0, nsmall = 2, decimal.mark = ",", big.mark = ".", scientific = FALSE))
  })
  
  output$general5 <- renderText({
    paste0(format(100*sumGenerales$dolarizado0, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%")
  })
  
  output$general6 <- renderText({
    paste0(format(100*sumGenerales$nueva0, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%")
  })

  output$tabla <- renderDT({
    data <- switch(input$data_tabla,
                   "Zona" = sumZona,
                   "Barrio" = sumBarrio,
                   "Ambientes" = sumAmbientes,
                   "Rango precio" = sumRangos)
    
    datatable(data) %>%
      formatPercentage('Var mensual Avisos', digits = 2) %>%
      formatCurrency('Mediana', mark = ".", dec.mark = ",", currency = '$', digits = 1) %>%
      formatCurrency('Promedio', mark = ".", dec.mark = ",", currency = '$', digits = 1) %>%
      formatCurrency('Promedio M2', mark = ".", dec.mark = ",", currency = '$', digits = 1) %>%
      formatPercentage('% Dolarizado', digits = 2) %>%
      formatPercentage('% Nueva', digits = 2)
 })

  
  output$mapabarrio <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = sumBarrioMapa, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
                  color = if(input$VariableMapeada == "Cantidad") {~pal(log10(sumBarrioMapa$`Q Avisos`)) } else { ~pal(log10(sumBarrioMapa$Mediana)) },
                  opacity = 1,
                  popup = paste("<a><strong>", sumBarrioMapa$BARRIO,"</strong></a><br>",
                                "Propiedades: ", sumBarrioMapa$`Q Avisos`, "<br>",
                                "Var. mensual avisos: ", paste0(format(100*sumBarrioMapa$`Var mensual Avisos`, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%"), "<br>",
                                "Precio promedio: ", paste0("$", format(sumBarrioMapa$`Promedio`, nsmall = 2, decimal.mark = ",", big.mark = ".")), "<br>",
                                "% Dolarizado: ", paste0(format(100*sumBarrioMapa$`% Dolarizado`, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%"), "<br>")) %>%
      addLegend("topright", pal = pal,
                values = if(input$VariableMapeada == "Cantidad") { sumBarrioMapa$`Q Avisos` } else { sumBarrioMapa$Mediana },
                title = if(input$VariableMapeada == "Cantidad") {"Cantidad de viviendas" } else { "Precio promedio" }  )
  })
    
    
  output$general1 <- renderText({
    format(sumGenerales$Total0, big.mark = ".", scientific = FALSE)
  })
  
  output$general2 <- renderText({
    paste0(format(100*sumGenerales$dif_total, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%")
  })
  
  output$general3 <- renderText({
    paste0("$", format(sumGenerales$promedioP0, nsmall = 2, decimal.mark = ",", big.mark = "."))
  })
  
  output$general4 <- renderText({
    paste0("$", format(sumGenerales$medianaP0, nsmall = 2, decimal.mark = ",", big.mark = ".", scientific = FALSE))
  })
  
  output$general5 <- renderText({
    paste0(format(100*sumGenerales$dolarizado0, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%")
  })
  
  output$general6 <- renderText({
    paste0(format(100*sumGenerales$nueva0, nsmall = 2, digits = 2, decimal.mark = ",", big.mark = "."),"%")
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
