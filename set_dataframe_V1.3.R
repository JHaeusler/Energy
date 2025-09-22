repositorio <- "http://cran.us.r-project.org"
install.packages("tidyr", repos = repositorio)
install.packages("readxl", repos = repositorio)
install.packages("dplyr", repos = repositorio)
install.packages("purrr", repos = repositorio)
install.packages("janitor", repos = repositorio)
install.packages("lubridate", repos = repositorio)

# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(purrr)
library(janitor) 
library(lubridate) 
library(tidyr)


# Define la ruta a tu carpeta principal
ruta_carpeta <- "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy/Datos/New_Format"

# Obtener la lista de rutas de todos los archivos .xls y .xlsx
archivos_excel <- list.files(
  path = ruta_carpeta,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

# Crear una lista vacía para almacenar los data frames
listas <- list()

#data_aux <- read_excel(archivos_excel[42])

# Bucle para leer cada archivo y almacenarlo de manera segura
for (i in 1:length(archivos_excel)) {
  tryCatch({
    data_aux <- read_excel(archivos_excel[i])

    if ("temaline_interface" %in% colnames(data_aux)) {
      row_aux <- which(data_aux$temaline_interface == "Views")
      # Si se encontró el marcador, se filtra la tabla
      if (length(row_aux) > 0) {
        data_aux <- data_aux[1:(row_aux[1] - 1), ]
      }
    }
        
    # Renombrar la tercera columna a "operator" ANTES de cualquier operación
    if (ncol(data_aux) >= 3) {
      colnames(data_aux)[3] <- "operator"
    }
    
    # Limpiar nombres de columnas para estandarizar
    data_aux <- data_aux %>% clean_names()
    
    # Convertir a formato de fecha y hora aquí
    if ("operator" %in% colnames(data_aux)) {
      data_aux$operator <- as_datetime(data_aux$operator)
    }
    
    # Corregir el tipo de dato de la columna 'transits'
    if ("transits" %in% colnames(data_aux)) {
      data_aux$transits <- as.character(data_aux$transits)
    }
    
    # Corregir el tipo de dato de la columna 'card_number'
    if ("card_number" %in% colnames(data_aux)) {
      data_aux$card_number <- as.character(data_aux$card_number)
    }
    
    # Corregir el tipo de dato de la columna 'transaction_date'
    if ("transaction_date" %in% colnames(data_aux)) {
      data_aux$transaction_date <- as_datetime(data_aux$transaction_date)
    }
    
    # Corregir el tipo de dato de la columna 'transit_status'
    if ("transit_status" %in% colnames(data_aux)) {
      data_aux$transit_status <- as_datetime(data_aux$transit_status)
    }
    
    # Encontrar la fila del marcador "views"

    
    # Almacenar el data frame en la lista
    listas[[i]] <- data_aux
    
  }, error = function(e) {
    warning("Error procesando el archivo: ", archivos_excel[i], " - Mensaje: ", e$message)
  })
}

# Combina todos los data frames de la lista en uno solo
df_combinado <- bind_rows(listas)

# Lista de las categorías a filtrar en la columna 'Name'
categorias_filtrar <- c(
  "TK54RT54", "TK55RT55", "TK56RT56", "TK57RT57", "TK58RT58", 
  "TK60RT60", "TK72RT72", "TK76RT76", "TK77RT77", "TK78RT78"
)

df_resumen <- df_combinado %>%
  filter(name %in% categorias_filtrar) %>%
  mutate(
    fecha = as_date(operator),
    hora = hour(operator)
  ) %>%
  
  # Contar la cantidad de transacciones por hora y por tipo de tránsito
  count(fecha, sap_transit_flag, hora, name = "transacciones_por_hora") %>%
  
  # Ordenar los datos por fecha y hora
  arrange(fecha, hora) %>%
  
  # Agrupar por fecha y tipo de tránsito para la suma acumulada
  group_by(fecha, sap_transit_flag) %>%
  
  # Aplicar la suma acumulada sobre el conteo por hora
  mutate(
    conteo_acumulado = cumsum(transacciones_por_hora),
    .groups = 'drop'
  )

# 1. Pivoting de los datos para ver el conteo por fecha y hora
df_pivoteado <- df_resumen %>%
  pivot_wider(
    id_cols = c(fecha, sap_transit_flag),
    names_from = hora,
    values_from = conteo_acumulado
  )

columnas_horas_ordenadas <- as.character(0:23)

# Seleccionar las columnas en el orden deseado
df_pivoteado_ordenado <- df_pivoteado %>%
  select(fecha, sap_transit_flag, all_of(columnas_horas_ordenadas))

data_exit <- df_pivoteado %>% filter(sap_transit_flag == "Exit")
data_exit <- data_exit %>%
  select(fecha, sap_transit_flag, all_of(columnas_horas_ordenadas))

data_entry <- df_pivoteado %>% filter(sap_transit_flag == "Entry")
data_entry <- data_entry %>%
  select(fecha, sap_transit_flag, all_of(columnas_horas_ordenadas))
