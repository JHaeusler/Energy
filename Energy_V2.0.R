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
    limite_inferior = mu - (2 * sigma),
    limite_superior = mu + (2 * sigma)
    
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
df_final$transacciones_por_hora_imp <- df_imputado$transacciones_imputadas

# Generar la secuencia de nombres de columna para las horas
columnas_horas_ordenadas <- as.character(0:23)

# 3. Recalcular el ACUMULADO con los conteos IMPUTADOS
Acum_hora_IMPUTADO <- df_final %>%
  arrange(fecha, hora) %>%
  group_by(fecha, transit_direction_description) %>%
  mutate(
    conteo_acumulado_IMPUTADO = cumsum(transacciones_por_hora_imp), 
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


# --- BLOQUE 5: Preparación de Datos de Diferencia Neta por Hora (CORREGIDO) ---

# 1. Preparar datos para pivoteo (solo variables necesarias)
df_conteo_limpio <- df_final %>%
  select(fecha, hora, transit_direction_description, transacciones_por_hora) %>%
  # Aseguramos que la hora sea numérica para el join, aunque en el pivoteo será factor/carácter
  mutate(hora = as.integer(hora))

# 2. Pivotear para tener 'Entry' y 'Exit' como columnas separadas (por FECHA y HORA)
df_neto_transacciones_por_hora_ancho <- df_conteo_limpio %>%
  pivot_wider(
    id_cols = c(fecha, hora), # Clave única: Cada fila es una combinación de fecha y hora
    names_from = transit_direction_description, # Crea columnas 'Entry' y 'Exit'
    values_from = transacciones_por_hora,
    values_fill = 0 # Si falta una dirección de tránsito para esa hora/fecha, se asume 0
  )

# 3. Calcular la Diferencia Neta (Entry - Exit) y transformar a formato largo
df_neto_transacciones_por_hora <- df_neto_transacciones_por_hora_ancho %>%
  # Calcular la diferencia neta de transacciones en esa hora específica
  mutate(
    diferencia_neta = Entry - Exit
  ) %>%
  # Transformar a formato largo, manteniendo solo la columna de diferencia neta
  select(fecha, hora, diferencia_neta) %>%
  # Añadir categorías de tiempo
  mutate(
    dia_del_mes = mday(fecha),
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


# Transformar a formato largo el Flujo Neto Ajustado para análisis
df_flujo_neto_IMPUTADO_long <- df_imputado %>%
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

# Preparar los datos para el heatmap
df_heatmap_data <- df_final %>%
  mutate(
    dia_del_mes = mday(fecha),
    hora_factor = factor(hora, levels = 0:23) # Convertir hora a factor para orden correcto
  ) %>%
  select(dia_del_mes, transit_direction_description, hora_factor, transacciones_por_hora, tipo_dia)




# Gráfico 2: Línea de Flujo Neto Promedio por Hora y Tipo de Día
df_promedio_flujo_neto_por_tipo_dia <- df_flujo_neto_IMPUTADO_long %>%
  group_by(tipo_dia, hora) %>%
  summarise(
    flujo_neto_promedio = mean(flujo_neto_ajustado, na.rm = TRUE),
    .groups = 'drop'
  )


# Gráfico 2: Línea de Flujo Neto Promedio por Hora y Tipo de Día
df_promedio_flujo_neto_por_tipo_dia <- df_flujo_neto_IMPUTADO_long %>%
  group_by(tipo_dia, hora) %>%
  summarise(
    flujo_neto_promedio = mean(flujo_neto_ajustado, na.rm = TRUE),
    .groups = 'drop'
  )


## 3. Visualización: Desviación Estándar (Variabilidad)

# Recálculo de Estadísticas Descriptivas (Promedio y SD) con Valores Imputados
df_imputado_stats_final <- df_final %>%
  group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
  summarise(
    conteo_promedio_IMPUTADO = mean(transacciones_por_hora, na.rm = TRUE),
    conteo_desv_estandar_IMPUTADO = sd(transacciones_por_hora, na.rm = TRUE),
    .groups = 'drop'
  )

# Definir el orden y las etiquetas en español para los días de la semana
# (Asumiendo que 1=Lunes, 7=Domingo, según la definición de wday(week_start=1) en tu código)
dias_espanol <- c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")

# 1. Preparar los datos: Crear una variable de factor ordenada para el eje X
df_boxplot_dias <- df_final %>%
  mutate(
    dia_ordenado = factor(
      dia_Sem,
      levels = 1:7,
      labels = dias_espanol,
      ordered = TRUE
    )
  )

# Definir el orden y las etiquetas en español para los días de la semana
dias_espanol <- c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")

# 1. Preparar los datos y agregar el factor ordenado del día
df_boxplot_flujo_neto <- df_flujo_neto_IMPUTADO_long %>%
  mutate(
    dia_Sem = wday(fecha, week_start = 1),
    dia_ordenado = factor(
      dia_Sem,
      levels = 1:7,
      labels = dias_espanol,
      ordered = TRUE
    )
  )

# 2. CALCULAR LAS ESTADÍSTICAS LOCALES (por día de la semana)
df_stats_locales <- df_boxplot_flujo_neto %>%
  group_by(dia_ordenado, tipo_dia) %>% # Agrupamos por día ordenado y tipo de día
  summarise(
    media_local = mean(flujo_neto_ajustado, na.rm = TRUE),
    p10_local = quantile(flujo_neto_ajustado, 0.10, na.rm = TRUE),
    p90_local = quantile(flujo_neto_ajustado, 0.90, na.rm = TRUE),
    .groups = 'drop'
  )

# Definir el orden de las franjas horarias para el eje X
orden_franjas <- c("Madrugada", "Maniana", "Tarde", "Noche")
# orden_franjas <- c("Madrugada", "Maniana", "Tarde", "Noche")
dias_espanol_lv <- c("Lun", "Mar", "Mié", "Jue", "Vie")

# 1. Preparar y filtrar los datos: Solo Lunes a Viernes
df_boxplot_franjas_lv <- df_imputado %>%
  filter(tipo_dia == "L-V") %>%
  mutate(
    # Asegurar el orden correcto de las franjas horarias
    franja_ordenada = factor(franja_horaria, levels = orden_franjas, ordered = TRUE)
  )



# 1. Preparar y filtrar los datos: Solo Lunes a Viernes
df_boxplot_franjas_diario <- df_imputado %>%
  filter(tipo_dia == "L-V") %>%
  mutate(
    # Crear factor ordenado para Franja Horaria (Eje X)
    franja_ordenada = factor(franja_horaria, levels = orden_franjas, ordered = TRUE),
    # Crear factor ordenado para Día de la Semana (Facetas horizontales)
    dia_Sem = wday(fecha, week_start = 1),
    dia_ordenado = factor(dia_Sem, levels = 1:5, labels = dias_espanol_lv, ordered = TRUE)
  )

# 2. CALCULAR LAS ESTADÍSTICAS LOCALES (por Día, Franja y Dirección)
df_stats_locales_diario <- df_boxplot_franjas_diario %>%
  group_by(dia_ordenado, franja_ordenada, transit_direction_description) %>%
  summarise(
    media_local = mean(transacciones_por_hora, na.rm = TRUE),
    p10_local = quantile(transacciones_por_hora, 0.10, na.rm = TRUE),
    p90_local = quantile(transacciones_por_hora, 0.90, na.rm = TRUE),
    .groups = 'drop'
  )


# 1. Preparar y filtrar los datos: Flujo Neto solo Lunes a Viernes
# df_flujo_neto_IMPUTADO_long ya debe contener franja_horaria y dia_Sem
df_boxplot_flujo_neto_diario <- df_imputado %>%
  # Asegurar que solo incluimos Lunes a Viernes 
  filter(tipo_dia == "L-V") %>%
  mutate(
    # Crear factor ordenado para Franja Horaria (Eje X)
    franja_ordenada = factor(franja_horaria, levels = orden_franjas, ordered = TRUE),
    # Crear factor ordenado para Día de la Semana (Facetas)
    dia_ordenado = factor(dia_Sem, levels = 1:5, labels = dias_espanol_lv, ordered = TRUE)
  )

# 2. CALCULAR LAS ESTADÍSTICAS LOCALES (por Día y Franja)
df_stats_neto_diario <- df_boxplot_flujo_neto_diario %>%
  group_by(dia_ordenado, franja_ordenada) %>%
  summarise(
    media_local = mean(transacciones_imputadas, na.rm = TRUE),
    p10_local = quantile(transacciones_imputadas, 0.10, na.rm = TRUE),
    p90_local = quantile(transacciones_imputadas, 0.90, na.rm = TRUE),
    .groups = 'drop'
  )

# [Bloques 1, 2 y 3 (Imputación) OMITIDOS aquí por ser idénticos al tuyo. 
# Se asume que df_final$transacciones_por_hora_imp y df_final existen y son correctos.]

# --- BLOQUE 3 (Continuación): CÁLCULO DEL FLUJO NETO ACUMULADO AJUSTADO ---

# 5. Cálculo del Flujo Neto Acumulado AJUSTADO (Entry - Exit)
data_exit_IMPUTADO <- df_pivoteado_acumulado_IMPUTADO %>% filter(transit_direction_description == "Exit")
data_entry_IMPUTADO <- df_pivoteado_acumulado_IMPUTADO %>% filter(transit_direction_description == "Entry")

# Asegurar que las columnas sean numéricas antes de la resta (solución al error 'non-numeric')
entry_num <- data_entry_IMPUTADO %>% select(all_of(columnas_horas_ordenadas)) %>% mutate(across(everything(), as.numeric))
exit_num <- data_exit_IMPUTADO %>% select(all_of(columnas_horas_ordenadas)) %>% mutate(across(everything(), as.numeric))

Matriz_difer_IMPUTADO <- entry_num - exit_num

# 🛑 CORRECCIÓN: Convertir la matriz de diferencia acumulada a un dataframe con la columna 'fecha'
# Se asume que las primeras dos columnas son 'fecha' y 'transit_direction_description' o similares en los df originales.
# Usamos data_entry_IMPUTADO como base para extraer la columna 'fecha'.
df_flujo_neto_IMPUTADO <- data_entry_IMPUTADO %>% 
  select(fecha) %>%
  cbind(Matriz_difer_IMPUTADO)

# Transformar a formato largo el Flujo Neto Acumulado Ajustado para análisis (df_flujo_neto_IMPUTADO_long)
df_flujo_neto_IMPUTADO_long <- df_flujo_neto_IMPUTADO %>%
  pivot_longer(
    cols = all_of(columnas_horas_ordenadas),
    names_to = "hora",
    values_to = "flujo_neto_ajustado"
  ) %>%
  mutate(
    hora = factor(hora, levels = 0:23)
  ) %>%
  # Unir con tipo_dia usando la columna 'fecha'
  left_join(
    df_final %>% select(fecha, tipo_dia) %>% distinct(),
    by = "fecha"
  )

# message("✅ Flujo Neto Acumulado Ajustado (df_flujo_neto_IMPUTADO_long) calculado.")


# --- BLOQUE 5: Preparación de Datos de Diferencia Neta por Hora (CORREGIDO) ---

# 1. Preparar datos para pivoteo (usando transacciones_por_hora_imp)
df_conteo_limpio_imp <- df_final %>%
  select(fecha, hora, transit_direction_description, transacciones_por_hora_imp) %>%
  mutate(hora = as.integer(hora)) # Aseguramos que la hora sea numérica para el pivoteo

# 2. Pivotear para tener 'Entry' y 'Exit' (IMPUTADOS) como columnas separadas
df_neto_transacciones_por_hora_ancho <- df_conteo_limpio_imp %>%
  pivot_wider(
    id_cols = c(fecha, hora), # Clave única: Cada fila es una combinación de fecha y hora
    names_from = transit_direction_description, # Crea columnas 'Entry' y 'Exit'
    values_from = transacciones_por_hora_imp, # 🛑 USAR EL VALOR IMPUTADO
    values_fill = 0 # Rellenar ausencias con 0
  )

# 3. Calcular la Diferencia Neta (Entry - Exit) y transformar a formato largo
df_neto_transacciones_por_hora <- df_neto_transacciones_por_hora_ancho %>%
  mutate(
    diferencia_neta = Entry - Exit
  ) %>%
  select(fecha, hora, diferencia_neta) %>%
  # Unir con las categorías de tiempo
  left_join(
    df_final %>% select(fecha, hora, tipo_dia, franja_horaria) %>% distinct(),
    by = c("fecha", "hora")
  ) %>%
  mutate(
    dia_del_mes = mday(fecha),
    hora_factor = factor(hora, levels = 0:23)
  )


# message("✅ Diferencia Neta de Transacciones por Hora (df_neto_transacciones_por_hora) calculada.")

# --- BLOQUE 6: VISUALIZACIONES (Heatmap corregido) ---

# Gráfico de Línea de Flujo Neto Promedio por Hora y Tipo de Día (Tu código)
df_promedio_flujo_neto_por_tipo_dia <- df_flujo_neto_IMPUTADO_long %>%
  group_by(tipo_dia, hora) %>%
  summarise(
    flujo_neto_promedio = mean(flujo_neto_ajustado, na.rm = TRUE),
    .groups = 'drop'
  )

# Gráfico 3: Heatmap de la Diferencia Neta de Transacciones por Hora
# 🛑 CORRECCIÓN: Usar escala de colores divergente y título/subtítulo correctos
ggplot(df_neto_transacciones_por_hora, aes(x = hora_factor, y = factor(dia_del_mes), fill = diferencia_neta)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~ tipo_dia, scales = "free_y", ncol = 1) + # Separar por tipo de día
  scale_fill_gradient2(
    low = "red", 
    mid = "white", 
    high = "blue", 
    midpoint = 0, # El 0 es el punto medio (Entry = Exit)
    name = "Diferencia Neta (Entry - Exit)"
  ) +
  labs(
    title = "Heatmap del Flujo Neto de Transacciones por Hora y Día del Mes",
    subtitle = "Azul: Excedente de Entry (Flujo Positivo). Rojo: Excedente de Exit (Flujo Negativo).",
    x = "Hora del Día",
    y = "Día del Mes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# [El resto de los gráficos están bien definidos, pero podrían usar el 'hora_factor'
# que se define en df_neto_transacciones_por_hora para consistencia en el eje X]

# 2. Generar el Boxplot
ggplot(df_boxplot_franjas_lv, aes(x = franja_ordenada, y = transacciones_imputadas)) +
  
  # Pintamos el boxplot. Usamos fill según la dirección.
  geom_boxplot(aes(fill = transit_direction_description), alpha = 0.8) +
  
  # Separamos el gráfico en dos paneles (uno para Entry y uno para Exit)
  facet_wrap(~transit_direction_description, scales = "free_y", 
             labeller = as_labeller(c(Entry = "Entradas (L-V)", Exit = "Salidas (L-V)"))) +
  
  labs(
    title = "Distribución del Conteo Absoluto por Franja Horaria (Lunes a Viernes)",
    subtitle = "El conteo imputado muestra la variabilidad típica por hora en cada franja.",
    x = "Franja Horaria",
    y = "Conteo por Hora (Transacciones Imputadas)"
  ) +
  scale_fill_manual(values = c("Entry" = "#0072B2", "Exit" = "#D55E00")) + # Colores temáticos
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none" # La leyenda ya no es necesaria al usar facet_wrap
  )

# message("Gráfico 7: Boxplot de Conteo Absoluto por Franja Horaria (Lunes a Viernes) generado.")


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
# message("Grafico 1: Distribución del Flujo Neto Ajustado por Hora (Boxplot)")


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
# message("Grafico 2: Flujo Neto Promedio por Hora y Tipo de Día (Líneas)")

## 2. Visualización: Heatmap de Conteo Imputado por Día del Mes y Hora


# Gráfico 3: Heatmap
ggplot(df_neto_transacciones_por_hora, aes(x = hora, y = dia_del_mes, fill = diferencia_neta)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Conteo Imputado") +
  labs(
    title = "Heatmap del Conteo de Transacciones Imputadas por Hora, Día y Dirección",
    x = "Hora del Día",
    y = "Día del Mes - Dirección"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6), # Ajustar tamaño de texto Y
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) #+
  #facet_wrap(~tipo_dia, scales = "free_y") # Separar por tipo de día

# message("Grafico 3: Heatmap del Conteo de Transacciones Imputadas")


ggplot(df_heatmap_data, aes(x = hora_factor, y = interaction(dia_del_mes, transit_direction_description), fill = conteo_acumulado)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Conteo Imputado") +
  labs(
    title = "Heatmap del Conteo de Transacciones Imputadas por Hora, Día y Dirección",
    x = "Hora del Día",
    y = "Día del Mes - Dirección"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6), # Ajustar tamaño de texto Y
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))# +
  facet_wrap(~tipo_dia, scales = "free_y") # Separar por tipo de día

# message("Grafico 3: Heatmap del Conteo de Transacciones Imputadas")


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
# message("Grafico 4: Desviación Estándar del Conteo Imputado")

# message("\n✅ Proceso de ETL y Análisis Completado con Visualizaciones.")
# message("Revise las ventanas de gráficos para ver los resultados visuales.")

# 2. Generar el Boxplot
# Usamos facet_wrap para separar las Entradas y Salidas en dos paneles distintos.
ggplot(df_boxplot_dias, aes(x = dia_ordenado, y = transacciones_por_hora)) +
  # Pintamos el boxplot. Usamos fill según la dirección para mejor contraste visual.
  geom_boxplot(aes(fill = transit_direction_description), alpha = 0.8) +
  
  # Separamos el gráfico en dos paneles (uno para Entry y uno para Exit)
  facet_wrap(~transit_direction_description, scales = "free_y", 
             labeller = as_labeller(c(Entry = "Entradas", Exit = "Salidas"))) +
  
  labs(
    title = "Distribución de Transacciones Imputadas por Día de la Semana",
    subtitle = "El boxplot representa el conteo horario (Entradas y Salidas separadas)",
    x = "Día de la Semana",
    y = "Transacciones por Hora (Conteo Imputado)"
  ) +
  scale_fill_manual(values = c("Entry" = "#0072B2", "Exit" = "#D55E00")) + # Colores temáticos
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none" # La leyenda ya no es necesaria al usar facet_wrap
  )

# message("Gráfico 5: Boxplot de Conteo por Día de la Semana generado.")



# --- 3. GENERAR EL BOXPLOT CON PUNTOS DE REFERENCIA LOCALES ---

ggplot(df_boxplot_flujo_neto, aes(x = dia_ordenado, y = flujo_neto_ajustado)) +
  
  # 3a. Pintar el boxplot
  geom_boxplot(aes(fill = tipo_dia), alpha = 0.8) +
  
  # 3b. Añadir la línea de equilibrio
  geom_hline(yintercept = 0, linetype = "solid", color = "gray20", linewidth = 0.8) +
  
  # 3c. Añadir los PUNTOS DE ESTADÍSTICAS LOCALES usando df_stats_locales
  
  # Media Local (Punto grande negro/azul)
  geom_point(data = df_stats_locales, 
             aes(y = media_local, color = "Media"), 
             size = 3.5, shape = 18) + # Forma de diamante
  
  # Percentil 90 Local (Punto pequeño rojo)
  geom_point(data = df_stats_locales, 
             aes(y = p90_local, color = "P90"), 
             size = 2, shape = 17) + # Forma de triángulo
  
  # Percentil 10 Local (Punto pequeño rojo)
  geom_point(data = df_stats_locales, 
             aes(y = p10_local, color = "P10"), 
             size = 2, shape = 17) + # Forma de triángulo
  
  labs(
    title = "Flujo Neto Ajustado: Distribución Semanal con Estadísticas Locales",
    subtitle = "Los puntos indican la Media (diamante) y los Percentiles P10/P90 (triángulos) de cada día.",
    x = "Día de la Semana",
    y = "Flujo Neto Ajustado (Entradas - Salidas)",
    fill = "Tipo de Día",
    color = "Estadística"
  ) +
  scale_color_manual(
    values = c("Media" = "black", "P10" = "red", "P90" = "red"),
    labels = c("Media", "P10", "P90")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# message("Gráfico 6 (Definitivo) generado con Media, P10 y P90 como puntos locales por boxplot.")


# --- 3. GENERAR EL BOXPLOT CON PUNTOS DE REFERENCIA LOCALES ---
# Definir el orden de las franjas horarias y días para el eje X y etiquetas


ggplot(df_boxplot_franjas_diario, aes(x = franja_ordenada, y = transacciones_por_hora)) +
  
  # 3a. Pintar el boxplot
  geom_boxplot(aes(fill = transit_direction_description), alpha = 0.8) +
  
  # 3b. Añadir los PUNTOS DE ESTADÍSTICAS LOCALES usando df_stats_locales_diario
  
  # Media Local (Punto grande negro/azul)
  geom_point(data = df_stats_locales_diario, 
             aes(y = media_local, color = "Media"), 
             size = 2.5, shape = 18) + # Forma de diamante
  
  # Percentil 90 Local (Punto pequeño rojo)
  geom_point(data = df_stats_locales_diario, 
             aes(y = p90_local, color = "P90"), 
             size = 1.5, shape = 17) + # Forma de triángulo
  
  # Percentil 10 Local (Punto pequeño rojo)
  geom_point(data = df_stats_locales_diario, 
             aes(y = p10_local, color = "P10"), 
             size = 1.5, shape = 17) + # Forma de triángulo
  
  # 3c. Separación total: Dirección (Filas) vs. Día (Columnas)
  facet_grid(transit_direction_description ~ dia_ordenado, scales = "free_y", 
             labeller = labeller(transit_direction_description = c(Entry = "Entradas", Exit = "Salidas"))) +
  
  labs(
    title = "Conteo Horario Imputado por Franja y Día (Lunes a Viernes)",
    subtitle = "Los puntos indican la Media (diamante) y los Percentiles P10/P90 (triángulos) para cada combinación.",
    x = "Franja Horaria",
    y = "Conteo por Hora (Transacciones Imputadas)",
    fill = "Dirección",
    color = "Estadística"
  ) +
  scale_fill_manual(values = c("Entry" = "#0072B2", "Exit" = "#D55E00"), guide = "none") +
  scale_color_manual(
    values = c("Media" = "black", "P10" = "red", "P90" = "red")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

# message("Gráfico 8: Boxplot de Conteo Absoluto Desglosado por Día y Franja Horaria generado.")

# Definir el orden de las franjas horarias y días para el eje X y etiquetas
orden_franjas <- c("Madrugada", "Maniana", "Tarde", "Noche")
dias_espanol_lv <- c("Lun", "Mar", "Mié", "Jue", "Vie")




# --- 3. GENERAR EL BOXPLOT CON PUNTOS DE REFERENCIA LOCALES ---

ggplot(df_boxplot_flujo_neto_diario, aes(x = franja_ordenada, y = transacciones_imputadas)) +
  
  # 3a. Pintar el boxplot.
  geom_boxplot(aes(fill = franja_ordenada), alpha = 0.8, show.legend = FALSE) +
  
  # 3b. Añadir la línea de equilibrio (Flujo Neto = 0)
  geom_hline(yintercept = 0, linetype = "solid", color = "gray20", linewidth = 0.8) +
  
  # 3c. Añadir los PUNTOS DE ESTADÍSTICAS LOCALES
  
  # Media Local (Punto grande negro/azul)
  geom_point(data = df_stats_neto_diario, 
             aes(y = media_local, color = "Media"), 
             size = 2.5, shape = 18) + # Forma de diamante
  
  # Percentil 90 Local (Punto pequeño rojo)
  geom_point(data = df_stats_neto_diario, 
             aes(y = p90_local, color = "P90"), 
             size = 1.5, shape = 17) + # Forma de triángulo
  
  # Percentil 10 Local (Punto pequeño rojo)
  geom_point(data = df_stats_neto_diario, 
             aes(y = p10_local, color = "P10"), 
             size = 1.5, shape = 17) + # Forma de triángulo
  
  # 3d. Separación total: Día (Facetas horizontales)
  facet_wrap(~dia_ordenado, scales = "free_y", ncol = 5) +
  
  labs(
    title = "Flujo Neto Ajustado por Franja Horaria (Lunes a Viernes)",
    subtitle = "La línea en 0 marca el equilibrio. El diamante es la Media, los triángulos P10/P90.",
    x = "Franja Horaria",
    y = "Flujo Neto Ajustado (Entradas - Salidas)",
    color = "Estadística"
  ) +
  scale_color_manual(
    values = c("Media" = "black", "P10" = "red", "P90" = "red")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

# message("✅ Gráfico 9: Boxplot de Flujo Neto Ajustado Desglosado por Día y Franja Horaria listo para visualizar.")