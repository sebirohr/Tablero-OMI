library(readr)
library(dplyr)
library(readr)
library(readxl)
library(sf)
library(shiny)
library(shinydashboard)
library(leaflet)
#library(maps)
library(geojsonio)
library(gt)
library(DT)
#library(ggplot2)
library(leaflet.extras)
#library(RColorBrewer)


setwd("C:/Users/Sebastián/Documents/Trabajos Propios/Urbano/OMI/Tablero-OMI")


df_0 <- read_csv("df/AvisosInmobiliarios2024-06.csv")
df_1 <- read_csv("df/AvisosInmobiliarios2024-05.csv")

## Agrego si es publicación nueva o no
# Cuento la cantidad de veces que el ID figura en la base del mes pasado
counts <- df_1 %>%
  count(ID)


df_0 <- df_0 %>%
  left_join(counts, by = "ID") %>%
  mutate(nueva = case_when(is.na(n) ~ "Si", TRUE ~ "No")) %>%
  select(-n)

## Quito publicaciones duplicadas
df_0 <- unique(df_0) %>%
  filter(Barrios != "Otro, Capital Federal")

df_0 <- df_0 %>%
  mutate(PrecioPesos = case_when(PrecioPesos > 70000000 | PrecioPesos < 20000 ~ NA, 
                                 TRUE ~ PrecioPesos),
         PrecioM2Pesos = case_when(PrecioPesos > 70000000 | PrecioPesos < 20000 ~ NA, 
                                   MetrosCuadrados == 1  ~ NA,
                                 TRUE ~ PrecioM2Pesos)) 

## Quito publicaciones duplicadas
df_1 <- unique(df_1) %>%
  filter(Barrios != "Otro, Capital Federal")

df_1 <- df_1 %>%
  mutate(PrecioPesos = case_when(PrecioPesos > 70000000 | PrecioPesos < 20000 ~ NA, 
                                 TRUE ~ PrecioPesos),
         PrecioM2Pesos = case_when(PrecioPesos > 70000000 | PrecioPesos < 20000 ~ NA, 
                                   MetrosCuadrados == 1  ~ NA,
                                   TRUE ~ PrecioM2Pesos)) 

## Armo bases
quitarubicacion <- read_excel("revisión direcciones.xlsx")
BarriosRecod <- read_excel("Barrios_Recodificado.xlsx")
Barrios_CABA <- st_read("barrios/barrios_wgs84.shp")
Barrios_CABA <- Barrios_CABA %>%
  mutate(BARRIO = case_when(
    BARRIO == "NUÑEZ" ~ "NUNEZ",
    TRUE ~ as.character(BARRIO)
  ))

### Agrego variables

df_0 <- df_0 %>%
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

df_1 <- df_1 %>%
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


#### RESULTADOS base 0

## Generales

Generales_0 <- df_0 %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'nueva' = sum(ifelse(nueva == "Si", 1, 0)) / n(),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(desc(Total))

## Generales por ambientes
dt_ambientes_0 <- df_0 %>%
  group_by(RangosAmbientes) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'nueva' = sum(ifelse(nueva == "Si", 1, 0)) / n(),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(RangosAmbientes)  %>%
  rename("Ambientes" = RangosAmbientes)


## Por barrio
dt_barrio_0 <- df_0 %>%
  group_by(Barrio_Recod) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'nueva' = sum(ifelse(nueva == "Si", 1, 0)) / n(),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(desc(Total))

## Por zona. Precio medio y si alquiler está dolarizado o no

dt_zona_0 <- df_0 %>%
  group_by(Corredor) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'nueva' = sum(ifelse(nueva == "Si", 1, 0)) / n(),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(desc(Total)) %>%
  rename("Zona" = Corredor)

## Por Rango precios. Precio medio y si alquiler está dolarizado o no

dt_rangoprecio_0 <- df_0 %>%
  group_by(RangosPrecio) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'nueva' = sum(ifelse(nueva == "Si", 1, 0)) / n(),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(`medianaP`)

##################################
#### RESULTADOS base 1

## Generales

Generales_1 <- df_1 %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(desc(Total))

## Generales por ambientes
dt_ambientes_1 <- df_1 %>%
  group_by(RangosAmbientes) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(RangosAmbientes)  %>%
  rename("Ambientes" = RangosAmbientes)


## Por barrio
dt_barrio_1 <- df_1 %>%
  group_by(Barrio_Recod) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(desc(Total))

## Por zona. Precio medio y si alquiler está dolarizado o no

dt_zona_1 <- df_1 %>%
  group_by(Corredor) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(desc(Total)) %>%
  rename("Zona" = Corredor)

## Por Rango precios. Precio medio y si alquiler está dolarizado o no

dt_rangoprecio_1 <- df_1 %>%
  group_by(RangosPrecio) %>%
  summarise('Total' = n(),
            'promedioP' = mean(PrecioPesos, na.rm = TRUE),
            'medianaP' = median(PrecioPesos, na.rm = TRUE),
            'promedioPm2' = mean(PrecioM2Pesos, na.rm = TRUE),
            'dolarizado' = sum(ifelse(Dolarizado == "si", 1, 0)) / n()) %>%
  arrange(`medianaP`)


### Calculo diferencias y armo tablas finales

## Generales
Generales_0 <- setNames(Generales_0, paste0(names(Generales_0), "0"))
Generales_1 <- setNames(Generales_1, paste0(names(Generales_1), "1"))

sumGenerales <- Generales_0 %>%
  cbind(Generales_1) %>%
  mutate(
    dif_total = (Total0 - Total1) / Total1,
    dif_promedioP = (promedioP0 - promedioP1) / promedioP1,
    dif_medianaP = (medianaP0 - medianaP1) / medianaP1,
    dif_promedioPm2 = (promedioPm20 - promedioPm21) / promedioPm21,
    dif_dolarizado = dolarizado0 - dolarizado1
  ) %>%
  select(-c(Total1,promedioP1, medianaP1, promedioPm21, dolarizado1))

## Por caracteristicas
### Ambientes

sumAmbientes <- dt_ambientes_0 %>%
  inner_join(dt_ambientes_1, by = "Ambientes", suffix = c("0", "1")) %>%
  mutate(
    dif_total = (Total0 - Total1) / Total1,
    dif_promedioP = (promedioP0 - promedioP1) / promedioP1,
    dif_medianaP = (medianaP0 - medianaP1) / medianaP1,
    dif_promedioPm2 = (promedioPm20 - promedioPm21) / promedioPm21,
  ) %>%
  select(-c(Total1,promedioP1, medianaP1, promedioPm21, dolarizado1, dif_promedioP, dif_medianaP, dif_promedioPm2)) %>%
  rename(
    'Var mensual Avisos' = dif_total,
    Mediana = medianaP0,
    Promedio = promedioP0,
    '% Dolarizado' = dolarizado0,
    '% Nueva' = nueva,
    'Promedio M2' = promedioPm20,
    'Q Avisos' = Total0
  )


sumZona <- dt_zona_0 %>%
  inner_join(dt_zona_1, by = "Zona", suffix = c("0", "1")) %>%
  mutate(
    dif_total = (Total0 - Total1) / Total1,
    dif_promedioP = (promedioP0 - promedioP1) / promedioP1,
    dif_medianaP = (medianaP0 - medianaP1) / medianaP1,
    dif_promedioPm2 = (promedioPm20 - promedioPm21) / promedioPm21,
  ) %>%
  select(-c(Total1,promedioP1, medianaP1, promedioPm21, dolarizado1, dif_promedioP, dif_medianaP, dif_promedioPm2)) %>%
  rename(
    'Var mensual Avisos' = dif_total,
    Mediana = medianaP0,
    Promedio = promedioP0,
    '% Dolarizado' = dolarizado0,
    '% Nueva' = nueva,
    'Promedio M2' = promedioPm20,
    'Q Avisos' = Total0
  )

sumRangos <- dt_rangoprecio_0 %>%
  inner_join(dt_rangoprecio_1, by = "RangosPrecio", suffix = c("0", "1")) %>%
  mutate(
    dif_total = (Total0 - Total1) / Total1,
    dif_promedioP = (promedioP0 - promedioP1) / promedioP1,
    dif_medianaP = (medianaP0 - medianaP1) / medianaP1,
    dif_promedioPm2 = (promedioPm20 - promedioPm21) / promedioPm21,
  ) %>%
  select(-c(Total1,promedioP1, medianaP1, promedioPm21, dolarizado1, dif_promedioP, dif_medianaP, dif_promedioPm2)) %>%
  rename(
    'Var mensual Avisos' = dif_total,
    Mediana = medianaP0,
    Promedio = promedioP0,
    '% Dolarizado' = dolarizado0,
    '% Nueva' = nueva,
    'Promedio M2' = promedioPm20,
    'Q Avisos' = Total0
  )

sumBarrio <- dt_barrio_0 %>%
  inner_join(dt_barrio_1, by = "Barrio_Recod", suffix = c("0", "1")) %>%
  mutate(
    dif_total = (Total0 - Total1) / Total1,
    dif_promedioP = (promedioP0 - promedioP1) / promedioP1,
    dif_medianaP = (medianaP0 - medianaP1) / medianaP1,
    dif_promedioPm2 = (promedioPm20 - promedioPm21) / promedioPm21,
  ) %>%
  select(-c(Total1,promedioP1, medianaP1, promedioPm21, dolarizado1, dif_promedioP, dif_medianaP, dif_promedioPm2)) %>%
  rename(
    Barrio = Barrio_Recod,
    'Var mensual Avisos' = dif_total,
    Mediana = medianaP0,
    Promedio = promedioP0,
    '% Dolarizado' = dolarizado0,
    '% Nueva' = nueva,
    'Promedio M2' = promedioPm20,
    'Q Avisos' = Total0
  )

## Base para mapear
df_map <- df_0 %>%
  left_join(quitarubicacion, by = "ID") %>%
  filter(!is.na(Longitud) & !is.na(Direccion) & is.na(Van)) 

## paleta para mapa
pal <- colorNumeric("Blues", NULL)

## Mapa poligonos 
sumBarrioMapa <- Barrios_CABA %>%
  left_join(sumBarrio, by = c("BARRIO" = "Barrio"))

# maximon y minimos para mapas
minprecio <- min(df_0$PrecioPesos, na.rm = TRUE)
maxprecio <- max(df_0$PrecioPesos, na.rm = TRUE)
