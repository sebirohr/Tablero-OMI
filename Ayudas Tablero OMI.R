### CARGO PAQUETES
library(leaflet)
library(readxl)
library(maps)
library(sf)
library(geojsonio)
library(googlesheets4)
library(dplyr)
library(gt)
library(DT)
library(ggplot2)
library(readr)
library(leaflet.extras)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(rsconnect)

options(scipen = 999, digits = 10)

Zonaprop <- read_csv("zonaprop 2024-04-24alquiler-capital-federal.csv")
## Saco duplicados
Zonaprop <- unique(Zonaprop) %>%
  filter(Barrios != "Otro, Capital Federal")

BarriosRecod <- read_excel("Barrios_Recodificado.xlsx")
Barrios_CABA <- st_read("barrios/barrios_wgs84.shp")
Barrios_CABA <- Barrios_CABA %>%
  mutate(BARRIO = case_when(
    BARRIO == "NUÑEZ" ~ "NUNEZ",
    TRUE ~ as.character(BARRIO)
  ))
Zonaprop <- Zonaprop %>% left_join(BarriosRecod,by = "Barrios")


Zonaprop <- Zonaprop %>%
  mutate(RangosPrecio = case_when(
    PrecioPesos < 200000 ~ "Menor a 200",
    PrecioPesos >= 200000 & PrecioPesos < 400000  ~ "200 a 400",
    PrecioPesos >= 400000 & PrecioPesos < 600000  ~ "400 a 600",
    PrecioPesos >= 600000 & PrecioPesos < 800000  ~ "600 a 800",
    PrecioPesos >= 800000 & PrecioPesos < 1000000  ~ "800 a 1.000",
    PrecioPesos >= 1000000 & PrecioPesos < 1500000  ~ "1.000 a 1.500",
    PrecioPesos >= 1500000 & PrecioPesos < 2000000  ~ "1.500 a 2.000",
    PrecioPesos >= 2000000 ~ "Mayor a 2.000")) %>%
  mutate(RangosAmbientes = case_when(
    Ambientes == 1 ~ "1 ambiente",
    Ambientes == 2 ~ "2 ambientes",
    Ambientes == 3 ~ "3 ambientes",
    Ambientes == 4 ~ "4 ambientes",
    Ambientes > 4  ~ "5 ambientes",
    TRUE ~ "Sin información"))

ResumenBarriosCABA <- Zonaprop %>%
  group_by(Barrio_Recod) %>%
  summarise('Total' = n(),
            'Promedio precio' = paste0("$", format(sum(PrecioPesos) / n(), nsmall = 2, decimal.mark = ",", big.mark = ".")),
            'Promedio precio m2' = paste0("$", format(sum(PrecioM2Pesos) / n(), nsmall = 2, decimal.mark = ",", big.mark = ".")),
            'Porcentaje dolarizado' = paste0(format(round(100 * (sum(ifelse(Dolarizado == "si", 1, 0)) / n()), digits = 2), nsmall = 2, decimal.mark = ",", big.mark = "."), "%")) 

# Procesamientos
#### Limpieza bases precio

Zonaprop_Precio <- Zonaprop %>%
  filter(!is.na(PrecioPesos) & PrecioPesos < 100000000 & PrecioPesos > 1000) 

#### Limpieza base ambientes

Zonaprop_Ambientes <- Zonaprop_Precio %>%
  filter(!is.na(Ambientes)) 

#### RESULTADOS

## Generales

Generales <- Zonaprop_Precio %>%
  summarise('Total' = n(),
            'Promedio precio' = sum(PrecioPesos) / n(),
            'Mediana precio' = median(PrecioPesos),
            'Promedio precio m2' = sum(PrecioM2Pesos) / n(),
            'Porcentaje dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(desc(Total))

## Generales por ambientes
dt_ambientes <- Zonaprop_Precio %>%
  group_by(RangosAmbientes) %>%
  summarise('Total' = n(),
            'Promedio precio' = paste0("$", format(round(sum(PrecioPesos) / n(),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Mediana precio' = paste0("$", format(round(median(PrecioPesos),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Promedio precio m2' = paste0("$", format(round(sum(PrecioM2Pesos) / n(),2), nsmall = 2, decimal.mark = ",", big.mark = ".")),
            'Porcentaje dolarizado' = paste0(format(round(100 * (sum(ifelse(Dolarizado == "si", 1, 0)) / n()), digits = 2), nsmall = 2, decimal.mark = ",", big.mark = "."), "%")) %>%
  arrange(RangosAmbientes)  %>%
  rename("Ambientes" = RangosAmbientes)


## Por barrio
dt_barrio <- Zonaprop_Precio %>%
  group_by(Barrio_Recod) %>%
  summarise('Total' = n(),
            'Promedio precio' = paste0("$", format(round(sum(PrecioPesos) / n(),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Mediana precio' = paste0("$", format(round(median(PrecioPesos),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Promedio precio m2' = paste0("$", format(round(sum(PrecioM2Pesos) / n(),2), nsmall = 2, decimal.mark = ",", big.mark = ".")),
            'Porcentaje dolarizado' = paste0(format(round(100 * (sum(ifelse(Dolarizado == "si", 1, 0)) / n()), digits = 2), nsmall = 2, decimal.mark = ",", big.mark = "."), "%")) %>%
  arrange(desc(Total)) %>%
  rename("Barrio" = Barrio_Recod)

## Por zona. Precio medio y si alquiler está dolarizado o no

dt_zona <- Zonaprop_Precio %>%
  group_by(Corredor) %>%
  summarise('Total' = n(),
            'Promedio precio' = paste0("$", format(round(sum(PrecioPesos) / n(),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Mediana precio' = paste0("$", format(round(median(PrecioPesos),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Promedio precio m2' = paste0("$", format(round(sum(PrecioM2Pesos) / n(),2), nsmall = 2, decimal.mark = ",", big.mark = ".")),
            'Porcentaje dolarizado' = paste0(format(round(100 * (sum(ifelse(Dolarizado == "si", 1, 0)) / n()), digits = 2), nsmall = 2, decimal.mark = ",", big.mark = "."), "%")) %>%
  arrange(desc(Total)) %>%
  rename("Zona" = Corredor)

## Por Rango precios. Precio medio y si alquiler está dolarizado o no

dt_rangoprecio <- Zonaprop_Precio %>%
  group_by(RangosPrecio) %>%
  summarise('Total' = n(),
            'Orden' = sum(PrecioPesos) / n(),
            'Promedio precio' = paste0("$", format(round(sum(PrecioPesos) / n(),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Mediana precio' = paste0("$", format(round(median(PrecioPesos),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Promedio precio m2' = paste0("$", format(round(sum(PrecioM2Pesos) / n(),2), nsmall = 2, decimal.mark = ",", big.mark = ".")),
            'Porcentaje dolarizado' = paste0(format(round(100 * (sum(ifelse(Dolarizado == "si", 1, 0)) / n()), digits = 2), nsmall = 2, decimal.mark = ",", big.mark = "."), "%")) %>%
  arrange(Orden) %>%
  select(-Orden) %>%
  rename("Rangos precio (en miles de pesos)" = RangosPrecio)

## Resumen por Barrio para mapa

resumen_barrio <- Zonaprop_Precio %>%
  group_by(Barrio_Recod) %>%
  summarise('Total' = n(),
            'Promedio precio' = paste0("$", format(round(sum(PrecioPesos) / n(),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Mediana precio' = paste0("$", format(round(median(PrecioPesos),2) , nsmall = 2, decimals = 2, decimal.mark = ",", big.mark = ".")),
            'Promedio precio m2' = paste0("$", format(round(sum(PrecioM2Pesos) / n(),2), nsmall = 2, decimal.mark = ",", big.mark = ".")),
            'Porcentaje dolarizado' = paste0(format(round(100 * (sum(ifelse(Dolarizado == "si", 1, 0)) / n()), digits = 2), nsmall = 2, decimal.mark = ",", big.mark = "."), "%"))

Resumen_CABA <- Barrios_CABA %>% 
  left_join(resumen_barrio, by = c("BARRIO" = "Barrio_Recod")) #%>%
 # mutate(Precio = case_when(is.na(Precio) ~ 0.001,
  #       TRUE ~ as.integer(Precio)),
   #      Total = case_when(is.na(Total) ~ 0.001,
    #      TRUE ~ as.integer(Total)))


## Mapa poligonos 

pal <- colorNumeric("Blues", NULL)

#### Limpieza base ambientes

Zonaprop_Ambientes <- Zonaprop %>%
  filter(!is.na(Ambientes)) 

## Filtro para mapa
Zonaprop_Mapa <- Zonaprop_Precio %>%
  filter(!is.na(Longitud) & !is.na(Direccion)) 

a <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="Menor a 200") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

b <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="200 a 400") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

c <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="400 a 600") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

d <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="600 a 800") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

e <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="800 a 1.000") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

f <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="1.000 a 1.500") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

g <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="1.500 a 2.000") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

h <- Zonaprop_Mapa %>% 
  filter(RangosPrecio=="Mayor a 2.000") %>%
  filter(!is.na(Longitud) & !is.na(Direccion))

## Valor máximo precios
max_value <- max(Zonaprop_Precio$PrecioPesos)


