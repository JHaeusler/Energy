# 1. Carga e instalación de paquetes ----
# Asegura que todos los paquetes necesarios estén instalados y cargados.
message("1. Verificando e instalando paquetes necesarios...")
lista_de_paquetes <- c("readxl", "ggplot2", "readr", "tidyverse", "dplyr",
                       "stats", "boot", "coda", "matrixStats", "gridExtra",
                       "pracma", "AcceptanceSampling", "GA", "purrr", "lubridate", "janitor") 

for (paquete in lista_de_paquetes) {
  if(!require(paquete, character.only = TRUE)){
    install.packages(paquete)
    require(paquete, character.only = TRUE)
  }
}

# Define la ruta a tu carpeta principal
ruta_carpeta <- "D:/Github/Energy/09.SEPTIEMBRE" # ¡Asegúrate de que esta ruta sea correcta!

# Obtener la lista de rutas de todos los archivos .xls y .xlsx
archivos_excel <- list.files(
  path = ruta_carpeta,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

# Leer los archivos de forma segura para manejar errores
resultados <- archivos_excel %>%
  map(safely(read_excel))

# Identificar los archivos que fallaron
errores <- resultados %>%
  map_lgl(~!is.null(.x$error))

# Filtrar los archivos que se leyeron correctamente
archivos_sin_error <- archivos_excel[-which(errores == 1)]

# Leer todos los archivos sin error, limpiar los nombres y combinarlos
df_combinado <- archivos_sin_error %>%
  map_dfr(~read_excel(.x) %>%
            clean_names()) # <-- Limpiamos los nombres de las columnas

# Preparar la columna de tiempo y extraer la hora
df_agrupado_por_hora <- df_combinado %>%
  mutate(
    # Reemplaza 'fecha_y_hora' con el nombre de tu columna
    timestamp = as_datetime(df_combinado$operator_nn_date_5_09_2025_hour_11_08_56_p_m), 
    fecha = as_date(timestamp),
    hora = hour(timestamp)
  ) %>%
  # Agrupar por fecha y hora para sumar entradas y salidas
  group_by(fecha, hora) %>%
  summarise(
    # Reemplaza 'entradas' y 'salidas' con el nombre de tus columnas
    entradas = sum(entradas, na.rm = TRUE),
    salidas = sum(salidas, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calcular el total neto de personas por hora
  mutate(
    personas_por_hora = entradas - salidas
  )


# Calcular la media y desviación estándar para cada hora
medias_por_hora <- df_agrupado_por_hora %>%
  group_by(hora) %>%
  summarise(
    media = mean(personas_por_hora, na.rm = TRUE),
    desviacion = sd(personas_por_hora, na.rm = TRUE),
    .groups = 'drop'
  )

# Unir las estadísticas al data frame principal para hacer el ajuste
df_ajustado <- df_agrupado_por_hora %>%
  left_join(medias_por_hora, by = "hora") %>%
  mutate(
    # Identificar valores que están fuera de 2 desviaciones estándar
    se_sale_de_media = abs(personas_por_hora - media) > 2 * desviacion,
    # Si el valor se sale, lo ajusta a la media; de lo contrario, deja el valor original
    personas_por_hora_ajustado = if_else(se_sale_de_media, media, personas_por_hora)
  )

# Resultado final con la columna ajustada
df_final <- df_ajustado %>%
  select(fecha, hora, entradas, salidas, personas_por_hora, personas_por_hora_ajustado)

# Puedes ver el resultado final
head(df_final)



