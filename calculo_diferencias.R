library(readr)
library(dplyr)

setwd("C:/Users/Sebastián/Documents/Trabajos Propios/Urbano/OMI/Tablero-OMI")
df_8 <- read_csv("df/AvisosInmobiliarios2024-08.csv")
df_7 <- read_csv("df/AvisosInmobiliarios2024-07.csv")
df_6 <- read_csv("df/AvisosInmobiliarios2024-06.csv")
df_5 <- read_csv("df/AvisosInmobiliarios2024-05.csv")
df_4 <- read_csv("df/AvisosInmobiliarios2024-04.csv")

options(scipen = 999)  # Establece un valor alto para evitar la not





## Funcion para obtener base

quitarduplicados <- function(df_actual, df_anterior) {
  # Contar publicaciones por ID en la base anterior
  counts_anterior <- df_anterior %>%
    count(ID)
  
  # Concatenar características en la base anterior
  viejas_df <- df_anterior %>%
    mutate(Concat = paste(as.character(Direccion), as.character(Ambientes), as.character(MetrosCuadrados), sep = " - ")) %>%
    count(Concat)
  
  # Actualizar la base actual
  df_actual <- df_actual %>%
    # Unir con conteo por ID de la base anterior
    left_join(counts_anterior, by = "ID") %>%
    # Crear la columna 'nueva'
    mutate(nueva = case_when(is.na(n) ~ "Si", TRUE ~ "No")) %>%
    select(-n)
  
  # Unir con conteo por características (Concat) de la base anterior
  df_actual <- df_actual %>%
    mutate(Concat = paste(as.character(Direccion), as.character(Ambientes), as.character(MetrosCuadrados), sep = " - ")) %>%
    left_join(viejas_df, by = "Concat") %>%
    mutate(nueva = case_when(
      nueva == "No" ~ "No",
      !is.na(n) ~ "No",
      TRUE ~ nueva        
    )) %>%
    select(-n)  # Eliminar la columna 'n' de la unión
  
  # Eliminar duplicados y filtrar
  df_actual <- df_actual %>%
    distinct() %>%
    filter(Barrios != "Otro, Capital Federal")
  
  df_actual <- df_actual %>%
    mutate(PrecioPesos = case_when(PrecioPesos > 70000000 | PrecioPesos < 20000 ~ NA, 
                                   TRUE ~ PrecioPesos),
           PrecioM2Pesos = case_when(PrecioPesos > 70000000 | PrecioPesos < 20000 ~ NA, 
                                     MetrosCuadrados == 1  ~ NA,
                                     TRUE ~ PrecioM2Pesos)) 
  
  return(df_actual)
}


df_8 <- quitarduplicados(df_8, df_7)
df_7 <- quitarduplicados(df_7, df_6)
df_6 <- quitarduplicados(df_6, df_5)
df_5 <- quitarduplicados(df_5, df_4)


# Calcular el promedio
promedio_0 <- mean(df_7$PrecioPesos, na.rm = TRUE)

# Calcular la mediana
mediana_0 <- median(df_8$PrecioPesos, na.rm = TRUE)

# Contar 
total_0 <- nrow(df_8)

# Calcular nuevas
nuevas_0 <- sum(df_8$nueva == "Si")

# Calcular el promedio
promedio_3 <- mean(df_5$PrecioPesos, na.rm = TRUE)

# Calcular la mediana
mediana_3 <- median(df_5$PrecioPesos, na.rm = TRUE)

# Contar 
total_3 <- nrow(df_5)

# Calcular nuevas
nuevas_3 <- sum(df_5$nueva == "Si")

#Precio Nuevas
precio2amb <- df_8 %>%
  filter(Ambientes == 2 & Dolarizado == "no") %>%
  summarise(media_valor = mean(PrecioPesos, na.rm = TRUE)) %>%
  pull(media_valor)

calculo_diferencias <- function(T0, T1) {
  porcentaje_cambio <- (T0 - T1) / T1 * 100
  return(porcentaje_cambio)
}

difmediana <- calculo_diferencias(mediana_0,mediana_3)
difpromedio <- calculo_diferencias(promedio_0,promedio_3)
diftotal <- calculo_diferencias(total_0,total_3)
difnuevas <- calculo_diferencias(nuevas_0,nuevas_3)