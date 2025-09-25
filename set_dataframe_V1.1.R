# repositorio <- "http://cran.us.r-project.org"
# install.packages("tidyr", repos = repositorio)
# install.packages("readxl", repos = repositorio)
# install.packages("dplyr", repos = repositorio)
# install.packages("purrr", repos = repositorio)
# install.packages("janitor", repos = repositorio)
# install.packages("lubridate", repos = repositorio)
# 
# # Cargar las librerías necesarias
# library(readxl)
# library(dplyr)
# library(purrr)
# library(janitor) 
# library(lubridate) 
# library(tidyr)
# Define la ruta a tu carpeta principal
ruta_carpeta_OF <- "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy/Datos/Old_Format/2025"

# Obtener la lista de rutas de todos los archivos .xls y .xlsx
archivos_excel <- list.files(
  path = ruta_carpeta_OF,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

# Crear una lista vacía para almacenar los data frames
listas <- list()

# Crear una lista vacía para almacenar los data frames
listas <- list()

# data_aux <- read_excel(archivos_excel[4], skip = 1)

# Bucle para leer cada archivo y almacenarlo de manera segura
for (i in 1:length(archivos_excel)) {
  tryCatch({
    data_aux <- read_excel(archivos_excel[i], skip = 1)
    
    # Limpiar nombres de columnas para estandarizar
    data_aux <- data_aux %>% clean_names()
    
    # Corregir el tipo de dato de la columna 'transits'
    if ("transit_status_description" %in% colnames(data_aux)) {
      data_aux$transit_status_description <- as.character(data_aux$transit_status_description)
    }
    
    # Corregir el tipo de dato de la columna 'card_number'
    if ("card_number" %in% colnames(data_aux)) {
      data_aux$card_number <- as.character(data_aux$card_number)
    }
    
    # Corregir el tipo de dato de la columna 'card_number'
    if ("identifier" %in% colnames(data_aux)) {
      data_aux$identifier <- as.character(data_aux$identifier)
    }
    
    # Corregir el tipo de dato de la columna 'card_number'
    if ("parameter_1" %in% colnames(data_aux)) {
      data_aux$parameter_1 <- as.character(data_aux$parameter_1)
    }
    
    # Corregir el tipo de dato de la columna 'transaction_date'
    if ("transaction_date" %in% colnames(data_aux)) {
      data_aux$transaction_date <- as_datetime(data_aux$transaction_date)
    }
    
    
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

# Realizar todas las operaciones de limpieza y resumen en un solo flujo
df_resumen <- df_combinado %>%
  filter(tema_key %in% categorias_filtrar) %>%
  mutate(
    # Convertir la columna 'operator' a formato de fecha y hora aquí
    fecha = as_date(transaction_date),
    hora = hour(transaction_date)
  ) %>%
  # Agrupar los datos para el conteo
  group_by(fecha, hora, transit_direction_description) %>%
  summarise(
    conteo = n(),
    .groups = 'drop'
  )

# 1. Pivoting de los datos para ver el conteo por fecha y hora
df_pivoteado <- df_resumen %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = hora,
    values_from = conteo
  )

columnas_horas_ordenadas <- as.character(0:23)

# Seleccionar las columnas en el orden deseado
df_pivoteado_ordenado <- df_pivoteado %>%
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas))

data_exit <- df_pivoteado %>% filter(transit_direction_description == "Exit")
data_exit <- data_exit %>%
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas))

data_entry <- df_pivoteado %>% filter(transit_direction_description == "Entry")
data_entry <- data_entry %>%
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas))

mat_dif <- as.matrix(data_entry[, 3:26]) - as.matrix(data_exit[, 3:26])



