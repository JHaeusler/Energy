# repositorio <- "http://cran.us.r-project.org"
# install.packages("tidyr", repos = repositorio)
# install.packages("readxl", repos = repositorio)
# install.packages("dplyr", repos = repositorio)
# install.packages("purrr", repos = repositorio)
# install.packages("janitor", repos = repositorio)
# install.packages("lubridate", repos = repositorio)
# install.packages("lubridate", repos = repositorio)
# 
# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(lubridate)
library(tidyr)
library(ggplot2)

# Define la ruta a tu carpeta principal
ruta_carpeta_OF <- "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy/Datos/Old_Format/2025"
# ruta_carpeta_OF <- "D:/Github/Energy/Datos/Old_Format/2025"

# --- BLOQUE 1: CARGA, LIMPIEZA Y COMBINACIÓN DE DATOS ---

# Obtener la lista de rutas de todos los archivos .xls y .xlsx
archivos_excel <- list.files(
  path = ruta_carpeta_OF,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

listas <- list()

# Bucle para leer cada archivo y almacenarlo de manera segura
for (i in 1:length(archivos_excel)) {
  tryCatch({
    data_aux <- read_excel(archivos_excel[i], skip = 1) %>% clean_names()
    
    # Estandarización de tipos de datos cruciales
    if ("transit_status_description" %in% colnames(data_aux)) {
      data_aux$transit_status_description <- as.character(data_aux$transit_status_description)
    }
    if ("card_number" %in% colnames(data_aux)) {
      data_aux$card_number <- as.character(data_aux$card_number)
    }
    if ("identifier" %in% colnames(data_aux)) {
      data_aux$identifier <- as.character(data_aux$identifier)
    }
    if ("parameter_1" %in% colnames(data_aux)) {
      data_aux$parameter_1 <- as.character(data_aux$parameter_1)
    }
    if ("transaction_date" %in% colnames(data_aux)) {
      data_aux$transaction_date <- as_datetime(data_aux$transaction_date)
    }
    
    listas[[i]] <- data_aux
    
  }, error = function(e) {
    warning("Error procesando el archivo: ", archivos_excel[i], " - Mensaje: ", e$message)
  })
}

df_combinado <- bind_rows(listas)

# Lista de las categorías a filtrar
categorias_filtrar <- c("TK54RT54", "TK55RT55", "TK56RT56", "TK57RT57", "TK58RT58", 
                        "TK60RT60", "TK72RT72", "TK76RT76", "TK77RT77", "TK78RT78")

# --- BLOQUE 2: CONTEO, COMPLEMENTO DE TIEMPO Y CÁLCULO ACUMULADO ---

df_general <- df_combinado %>%
  filter(tema_key %in% categorias_filtrar) %>%
  mutate(
    fecha = as_date(transaction_date),
    hora = hour(transaction_date)
  )

Count_hora <- df_general %>% 
  count(fecha, transit_direction_description, hora, name = "transacciones_por_hora")

# Generar tabla de referencia completa (todas las horas)
fechas_unicas <- unique(df_general$fecha)
tipos_transito <- unique(df_general$transit_direction_description)
horas_completas <- 0:23

df_horas_completas <- expand_grid(
  fecha = fechas_unicas,
  transit_direction_description = tipos_transito,
  hora = horas_completas
)

# Unir, rellenar NAs con 0 y calcular el conteo acumulado (BRUTO)
df_final <- left_join(
  df_horas_completas,
  Count_hora,
  by = c("fecha", "transit_direction_description", "hora")
) %>%
  replace_na(list(transacciones_por_hora = 0)) %>%
  arrange(fecha, hora) %>%
  group_by(fecha, transit_direction_description) %>%
  mutate(
    conteo_acumulado = cumsum(transacciones_por_hora),
    .groups = 'drop'
  ) %>%
  # Añadir categorías de tiempo
  mutate(
    franja_horaria = case_when(
      hora >= 0 & hora <= 5 ~ "Madrugada",
      hora >= 6 & hora <= 12 ~ "Maniana",
      hora >= 13 & hora <= 18 ~ "Tarde",
      hora >= 19 & hora <= 23 ~ "Noche",
      TRUE ~ NA_character_
    ),
    dia_Sem = wday(fecha, week_start = 1),
    tipo_dia = case_when(
      dia_Sem >= 1 & dia_Sem <= 5 ~ "L-V",
      dia_Sem == 6 ~ "Sab",
      dia_Sem == 7 ~ "Dom",
      TRUE ~ NA_character_
    )
  )

# --- BLOQUE 3: IMPUTACIÓN DE OUTLIERS POR LA MEDIA (MU) Y RECALCULACIÓN DE FLUJO NETO ---

# 1. Calcular estadísticas descriptivas (para límites de imputación)
df_imputacion_stats <- df_final %>%
  group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
  summarise(
    mu = mean(transacciones_por_hora, na.rm = TRUE),
    sigma = sd(transacciones_por_hora, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    limite_superior = mu + (2 * sigma),
    limite_inferior = mu - (2 * sigma)
  )

# 2. Aplicar Imputación por la Media y actualizar df_final
df_imputado <- df_final %>%
  left_join(
    df_imputacion_stats, 
    by = c("tipo_dia", "franja_horaria", "transit_direction_description")
  ) %>%
  mutate(
    transacciones_imputadas = ifelse(
      transacciones_por_hora < limite_inferior | transacciones_por_hora > limite_superior, 
      mu, 
      transacciones_por_hora
    ),
    transacciones_imputadas = pmax(0, transacciones_imputadas)
  )

# Actualizar df_final con los valores imputados
df_final$transacciones_por_hora <- df_imputado$transacciones_imputadas

# Generar la secuencia de nombres de columna para las horas
columnas_horas_ordenadas <- as.character(0:23)

# 3. Recalcular el ACUMULADO con los conteos IMPUTADOS
Acum_hora_IMPUTADO <- df_final %>%
  arrange(fecha, hora) %>%
  group_by(fecha, transit_direction_description) %>%
  mutate(
    conteo_acumulado_IMPUTADO = cumsum(transacciones_por_hora), 
    .groups = 'drop'
  )

# 4. Pivoteo del Acumulado Imputado
df_pivoteado_acumulado_IMPUTADO <- Acum_hora_IMPUTADO %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = hora,
    values_from = conteo_acumulado_IMPUTADO
  ) %>%
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
  drop_na(fecha)

# 5. Cálculo del Flujo Neto Acumulado AJUSTADO (Entry - Exit)
data_exit_IMPUTADO <- df_pivoteado_acumulado_IMPUTADO %>% filter(transit_direction_description == "Exit")
data_entry_IMPUTADO <- df_pivoteado_acumulado_IMPUTADO %>% filter(transit_direction_description == "Entry")

# Asegurar que las columnas sean numéricas antes de la resta (solución al error 'non-numeric')
entry_num <- data_entry_IMPUTADO %>% select(all_of(columnas_horas_ordenadas)) %>% mutate(across(everything(), as.numeric))
exit_num <- data_exit_IMPUTADO %>% select(all_of(columnas_horas_ordenadas)) %>% mutate(across(everything(), as.numeric))

Matriz_difer_IMPUTADO <- entry_num[ ,3:26] - exit_num[ ,3:26]

# Resultado final del flujo neto AJUSTADO (formato ancho)
df_flujo_neto_IMPUTADO <- data_entry_IMPUTADO %>% 
  select(fecha) %>% 
  cbind(Matriz_difer_IMPUTADO)

message("✅ Flujo Neto Acumulado Ajustado (df_flujo_neto_IMPUTADO) calculado.")

# --- BLOQUE 5: ANÁLISIS DESCRIPTIVO Y VISUALIZACIONES ---

## 1. Análisis de Patrones Típicos (Flujo Neto Ajustado)

# Transformar a formato largo el Flujo Neto Ajustado para análisis
df_flujo_neto_IMPUTADO_long <- df_flujo_neto_IMPUTADO %>%
  pivot_longer(
    cols = all_of(columnas_horas_ordenadas),
    names_to = "hora",
    values_to = "flujo_neto_ajustado"
  ) %>%
  mutate(
    hora = factor(hora, levels = 0:23),
    # Unir con tipo_dia para análisis
    fecha_join = fecha # Usamos una columna auxiliar para el join
  ) %>%
  left_join(
    df_final %>% select(fecha, tipo_dia) %>% distinct(),
    by = c("fecha_join" = "fecha")
  ) %>%
  select(-fecha_join) # Eliminar la columna auxiliar


# Visualización: Distribución del Flujo Neto Ajustado por Hora (Boxplot)
ggplot(df_flujo_neto_IMPUTADO_long, aes(x = hora, y = flujo_neto_ajustado, fill = tipo_dia)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Flujo Neto Ajustado por Hora del Día y Tipo de Día",
    x = "Hora del Día",
    y = "Flujo Neto Ajustado (Entradas - Salidas)",
    fill = "Tipo de Día"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~tipo_dia, scales = "free_y")
message("Grafico 1: Distribución del Flujo Neto Ajustado por Hora (Boxplot)")

# Gráfico 2: Línea de Flujo Neto Promedio por Hora y Tipo de Día
df_promedio_flujo_neto_por_tipo_dia <- df_flujo_neto_IMPUTADO_long %>%
  group_by(tipo_dia, hora) %>%
  summarise(
    flujo_neto_promedio = mean(flujo_neto_ajustado, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(df_promedio_flujo_neto_por_tipo_dia, aes(x = hora, y = flujo_neto_promedio, color = tipo_dia, group = tipo_dia)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Flujo Neto Promedio Ajustado por Hora y Tipo de Día",
    x = "Hora del Día",
    y = "Flujo Neto Promedio (Entradas - Salidas)",
    color = "Tipo de Día"
  ) +
  theme_minimal() +
  scale_x_discrete(breaks = as.character(seq(0, 23, by = 2))) # Mostrar solo cada 2 horas
message("Grafico 2: Flujo Neto Promedio por Hora y Tipo de Día (Líneas)")

## 2. Visualización: Heatmap de Conteo Imputado por Día del Mes y Hora

# Preparar los datos para el heatmap
df_heatmap_data <- df_final %>%
  mutate(
    dia_del_mes = mday(fecha),
    hora_factor = factor(hora, levels = 0:23) # Convertir hora a factor para orden correcto
  ) %>%
  select(dia_del_mes, transit_direction_description, hora_factor, transacciones_por_hora, tipo_dia)

# Gráfico 3: Heatmap
ggplot(df_heatmap_data, aes(x = hora_factor, y = interaction(dia_del_mes, transit_direction_description), fill = transacciones_por_hora)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Conteo Imputado") +
  labs(
    title = "Heatmap del Conteo de Transacciones Imputadas por Hora, Día y Dirección",
    x = "Hora del Día",
    y = "Día del Mes - Dirección"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6), # Ajustar tamaño de texto Y
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~tipo_dia, scales = "free_y") # Separar por tipo de día

message("Grafico 3: Heatmap del Conteo de Transacciones Imputadas")

## 3. Visualización: Desviación Estándar (Variabilidad)

# Recálculo de Estadísticas Descriptivas (Promedio y SD) con Valores Imputados
df_imputado_stats_final <- df_final %>%
  group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
  summarise(
    conteo_promedio_IMPUTADO = mean(transacciones_por_hora, na.rm = TRUE),
    conteo_desv_estandar_IMPUTADO = sd(transacciones_por_hora, na.rm = TRUE),
    .groups = 'drop'
  )

# Gráfico 4: Desviación Estándar del Conteo Imputado por Franja Horaria y Tipo de Día
ggplot(df_imputado_stats_final, aes(x = franja_horaria, y = conteo_desv_estandar_IMPUTADO, fill = transit_direction_description)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "Desviación Estándar del Conteo Imputado por Franja Horaria y Tipo de Día",
    x = "Franja Horaria",
    y = "Desviación Estándar del Conteo",
    fill = "Dirección"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~tipo_dia, scales = "free_y")
message("Grafico 4: Desviación Estándar del Conteo Imputado")

message("\n✅ Proceso de ETL y Análisis Completado con Visualizaciones.")
message("Revise las ventanas de gráficos para ver los resultados visuales.")