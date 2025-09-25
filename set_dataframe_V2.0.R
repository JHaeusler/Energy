# repositorio <- "http://cran.us.r-project.org"
# install.packages("tidyr", repos = repositorio)
# install.packages("readxl", repos = repositorio)
# install.packages("dplyr", repos = repositorio)
# install.packages("purrr", repos = repositorio)
# install.packages("janitor", repos = repositorio)
# install.packages("lubridate", repos = repositorio)
# install.packages("lubridate", repos = repositorio)
# 
# # Cargar las librerías necesarias
# library(readxl)
# library(dplyr)
# library(purrr)
# library(janitor)
# library(lubridate)
# library(tidyr)
# library(ggplot2)
# Define la ruta a tu carpeta principal
# ruta_carpeta_NF <- "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy/Datos/Old_Format/2025"
ruta_carpeta_OF <- "D:/Github/Energy/Datos/Old_Format/2025"

# Obtener la lista de rutas de todos los archivos .xls y .xlsx
archivos_excel <- list.files(
  path = ruta_carpeta_OF,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

# Crear una lista vacía para almacenar los data frames
listas <- list()

# data_aux <- read_excel(archivos_excel[31], skip = 1)

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
# Paso 1: Contar las transacciones por hora (como lo habías hecho)
df_resumen <- df_combinado %>%
  filter(tema_key %in% categorias_filtrar) %>%
  mutate(
    fecha = as_date(transaction_date),
    hora = hour(transaction_date)
  ) %>%
  count(fecha, transit_direction_description, hora, name = "transacciones_por_hora")

# Paso 2: Generar una tabla de referencia con todas las combinaciones de fecha, tipo de tránsito y hora
fechas_unicas <- unique(df_resumen$fecha)
tipos_transito <- unique(df_resumen$transit_direction_description)
horas_completas <- 0:23

df_horas_completas <- expand_grid(
  fecha = fechas_unicas,
  transit_direction_description = tipos_transito,
  hora = horas_completas
)

# Paso 3: Unir y rellenar los datos antes de la suma acumulada
df_resumen_completo <- left_join(
  df_horas_completas,
  df_resumen,
  by = c("fecha", "transit_direction_description", "hora")
) %>%
  # Rellena los NA con 0
  replace_na(list(transacciones_por_hora = 0))  %>%
  # Ordena los datos por fecha y hora para que cumsum() funcione bien
  arrange(fecha, hora) %>%

  # Agrupa por fecha y tipo de tránsito para la suma acumulada
  group_by(fecha, transit_direction_description) %>%

  # Aplica la suma acumulada a los conteos por hora (ahora sin NAs)
  mutate(
    conteo_acumulado = cumsum(transacciones_por_hora),
    .groups = 'drop'
  )

# 4. Pivoting de los datos para ver el conteo por fecha y hora
df_pivoteado <- df_resumen_completo %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = hora,
    values_from = conteo_acumulado
  )
columnas_horas_ordenadas <- as.character(0:23)

# Seleccionar las columnas en el orden deseado
df_pivoteado_ordenado <- df_pivoteado %>%
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
  drop_na(fecha)

data_exit <- df_pivoteado %>% filter(transit_direction_description == "Exit")
data_exit <- data_exit %>%
  select(fecha, transit_direction_description,
         all_of(columnas_horas_ordenadas)) %>% drop_na(fecha)

data_entry <- df_pivoteado %>% filter(transit_direction_description == "Entry")
data_entry <- data_entry %>%
  select(fecha, transit_direction_description,
         all_of(columnas_horas_ordenadas)) %>% drop_na(fecha)

Matriz_difer <- data_entry[, 3:26] - data_exit[, 3:26]

# colMeans(Matriz_difer)
# rowMeans(Matriz_difer)
# 
# Convertir la matriz de diferencias en un data frame
df_diferencias <- as.data.frame(Matriz_difer)

# Agregar la fecha como una columna para el pivot
df_diferencias$fecha <- data_entry$fecha

# Transformar a formato largo
df_diferencias_long_hora <- df_diferencias %>%
  pivot_longer(
    cols = as.character(0:23),
    names_to = "hora",
    values_to = "diferencia"
  )

# Convertir la columna 'hora' a un factor para el orden correcto en el eje X
df_diferencias_long_hora$hora <- factor(df_diferencias_long_hora$hora, levels = 0:23)

# ggplot(df_diferencias_long_hora, aes(x = hora, y = diferencia)) +
#   geom_boxplot(fill = "steelblue", color = "black") +
#   labs(
#     title = "Distribución de Diferencias (Entradas - Salidas) por Hora",
#     x = "Hora del Día",
#     y = "Diferencia de Transacciones"
#   ) +
#   theme_minimal()

# Paso 1: Crear las categorías de días de la semana
aux_day <- df_diferencias %>% 
  mutate(dia_Sem = wday(fecha, week_start = 1)) %>% # week_start = 1: Lunes = 1, Domingo = 7
  mutate(state_ = case_when(
    dia_Sem >= 1 & dia_Sem <= 5 ~ "L-V",
    dia_Sem == 6 ~ "Sab",
    dia_Sem == 7 ~ "Dom"
  ))

aux_day[c(1,6), 26] <- "Dom"


aux_hour <- df_resumen_completo
state_day <- aux_hour %>% mutate(state = case_when(
  hora >= 0 & hora <= 5 ~ "Madrugada",
  hora >= 6 & hora <= 12 ~ "Mañana",
  hora >= 13 & hora <= 18 ~ "Tarde",
  hora >= 19 & hora <= 23 ~ "Noche"
  ))

state_day

state_pivoteado <- df_resumen_completo %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = state_day,
    values_from = conteo_acumulado
  )


# Paso 2: Convertir a formato largo para poder calcular el promedio
# Esto apila las columnas de horas (0-23) en una sola columna 'diferencia'
aux_Q_long <- aux_Q %>%
  pivot_longer(
    cols = as.character(0:23),
    names_to = "hora",
    values_to = "diferencia"
  )

# Paso 3: Agrupar por la nueva categoría de día de la semana y calcular el promedio
resumen_promedio_dias <- aux_Q_long %>%
  group_by(state_) %>%
  summarise(
    promedio_diferencia = mean(diferencia, na.rm = TRUE),
    .groups = 'drop'
  )

print(resumen_promedio_dias)


