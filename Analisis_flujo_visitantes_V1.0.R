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

# Define la ruta a tu carpeta principal
# Define la ruta a tu carpeta principal
ruta_carpeta_OF <- "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy/Datos/Old_Format/2025"
# ruta_carpeta_OF <- "D:/Github/Energy/Datos/Old_Format/2025"

# Obtener la lista de rutas de todos los archivos .xls y .xlsx de forma segura
archivos_excel <- list.files(
  path = ruta_carpeta_OF,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

# Utilizar map_dfr para un manejo más eficiente y seguro de la lectura de archivos
df_combinado <- purrr::map_dfr(archivos_excel, ~{
  tryCatch({
    # Leer el archivo con manejo de errores
    data_aux <- readxl::read_excel(., skip = 1) %>% 
      janitor::clean_names()
    
    # Convertir tipos de datos de forma más concisa
    data_aux <- data_aux %>%
      mutate(
        across(
          c(transit_status_description, card_number, identifier, parameter_1),
          as.character
        ),
        transaction_date = as_datetime(transaction_date)
      )
    
    return(data_aux)
    
  }, error = function(e) {
    warning("Error procesando el archivo: ", ., " - Mensaje: ", e$message)
    return(NULL)
  })
})

# Lista de las categorías a filtrar
categorias_filtrar <- c(
  "TK54RT54", "TK55RT55", "TK56RT56", "TK57RT57", "TK58RT58", 
  "TK60RT60", "TK72RT72", "TK76RT76", "TK77RT77", "TK78RT78"
)

# Resumen de conteos por fecha, dirección y hora
df_resumen <- df_combinado %>%
  filter(tema_key %in% categorias_filtrar) %>%
  mutate(
    fecha = as_date(transaction_date),
    hora = hour(transaction_date)
  ) %>%
  count(fecha, transit_direction_description, hora, name = "conteo_por_hora")

# Generar tabla de referencia con todas las combinaciones de fecha, tipo de tránsito y hora
fechas_unicas <- unique(df_resumen$fecha)
tipos_transito <- unique(df_resumen$transit_direction_description)
horas_completas <- 0:23

df_horas_completas <- expand_grid(
  fecha = fechas_unicas,
  transit_direction_description = tipos_transito,
  hora = horas_completas
)

# Unir, rellenar y calcular la suma acumulada
df_resumen_completo <- left_join(
  df_horas_completas,
  df_resumen,
  by = c("fecha", "transit_direction_description", "hora")
) %>%
  replace_na(list(conteo_por_hora = 0)) %>%
  arrange(fecha, hora) %>%
  group_by(fecha, transit_direction_description) %>%
  mutate(
    conteo_acumulado = cumsum(conteo_por_hora),
    .groups = 'drop'
  )

# Pivoteo de los datos por hora (para el análisis de diferencias)
df_pivoteado <- df_resumen_completo %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = hora,
    values_from = conteo_acumulado
  )

columnas_horas_ordenadas <- as.character(0:23)

data_exit <- df_pivoteado %>% filter(transit_direction_description == "Exit")
data_exit <- data_exit %>%
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
  drop_na(fecha)

data_entry <- df_pivoteado %>% filter(transit_direction_description == "Entry")
data_entry <- data_entry %>%
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
  drop_na(fecha)

Matriz_difer <- data_entry[, 3:26] - data_exit[, 3:26]
df_diferencias <- as.data.frame(Matriz_difer)
df_diferencias$fecha <- data_entry$fecha

# Transformar a formato largo para visualización
df_diferencias_long_hora <- df_diferencias %>%
  pivot_longer(
    cols = as.character(0:23),
    names_to = "hora",
    values_to = "diferencia"
  )
df_diferencias_long_hora$hora <- factor(df_diferencias_long_hora$hora, levels = 0:23)

### Análisis Adicional Corregido
  
  # 1. Añadir la columna de franja horaria al data frame de resumen completo
  df_resumen_con_franjas <- df_resumen_completo %>%
  mutate(
    franja_horaria = case_when(
      hora >= 0 & hora <= 5 ~ "Madrugada",
      hora >= 6 & hora <= 12 ~ "Mañana",
      hora >= 13 & hora <= 18 ~ "Tarde",
      hora >= 19 & hora <= 23 ~ "Noche",
      TRUE ~ NA_character_
    )
  )

# 2. Añadir las categorías de días de la semana
df_final <- df_resumen_con_franjas %>%
  mutate(
    dia_Sem = wday(fecha, week_start = 1),
    tipo_dia = case_when(
      dia_Sem >= 1 & dia_Sem <= 5 ~ "L-V",
      dia_Sem == 6 ~ "Sab",
      dia_Sem == 7 ~ "Dom",
      TRUE ~ NA_character_
    )
  )

# 3. Pivoteo por franja horaria para ver el conteo total en cada una
state_pivoteado <- df_final %>%
  group_by(tipo_dia, transit_direction_description, franja_horaria) %>%
  summarise(conteo_total_franja = sum(conteo_por_hora), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(tipo_dia, transit_direction_description),
    names_from = franja_horaria,
    values_from = conteo_total_franja
  )

# Opcional: Para ver el conteo promedio por tipo de día (L-V, Sab, Dom)
df_por_tipo_dia <- df_final %>%
  group_by(tipo_dia, transit_direction_description, hora) %>%
  summarise(
    conteo_promedio_por_hora = mean(conteo_por_hora, na.rm = TRUE),
    .groups = "drop"
  )


# Agrupar por tipo de día, dirección y franja horaria para calcular el PROMEDIO
df_promedio_por_tipo_dia <- df_final %>%
  group_by(tipo_dia, transit_direction_description, franja_horaria) %>%
  summarise(conteo_promedio_franja = mean(conteo_por_hora, na.rm = TRUE), .groups = "drop")

# Ahora, pivotea los datos para una vista clara del promedio
df_promedio_pivoteado <- df_promedio_por_tipo_dia %>%
  pivot_wider(
    id_cols = c(tipo_dia, transit_direction_description),
    names_from = franja_horaria,
    values_from = conteo_promedio_franja
  )
df_analisis_final <- df_final %>%
  group_by(tipo_dia, transit_direction_description, franja_horaria) %>%
  summarise(
    conteo_promedio = mean(conteo_por_hora, na.rm = TRUE),
    desviacion_estandar = sd(conteo_por_hora, na.rm = TRUE),
    .groups = "drop"
  )
