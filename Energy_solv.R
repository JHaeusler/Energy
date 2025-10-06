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
df_general <- df_combinado %>%
  # Nota: Se usa 'tema_key' asumiendo que se genera con clean_names()
  filter(tema_key %in% categorias_filtrar) %>%
  mutate(
    fecha = as_date(transaction_date),
    hora = hour(transaction_date)
  )

Count_hora <- df_general %>% 
  count(fecha, transit_direction_description, hora,
        name = "transacciones_por_hora")

# Paso 2: Generar una tabla de referencia con todas las combinaciones de fecha, tipo de tránsito y hora
fechas_unicas <- unique(df_general$fecha)
tipos_transito <- unique(df_general$transit_direction_description)
horas_completas <- 0:23

df_horas_completas <- expand_grid(
  fecha = fechas_unicas,
  transit_direction_description = tipos_transito,
  hora = horas_completas
)

# Paso 3: Unir y rellenar los datos antes de la suma acumulada
Acum_hora <- left_join(
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
  )


df_final <- Acum_hora %>%
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

## **Generación de Tablas Pivoteadas (Conteo y Acumulado)**

# Generar la secuencia de nombres de columna para las horas
columnas_horas_ordenadas <- as.character(0:23)

# 4. Pivoting de los datos de CONTEO
df_pivoteado_conteo <- df_final %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = hora,
    values_from = transacciones_por_hora,
    values_fill = 0 # Asegura que las horas sin transacciones sean 0
  ) %>%
  # Ordenar las columnas del conteo
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
  drop_na(fecha)


# 5. Pivoting de los datos ACUMULADOS
df_pivoteado_acumulado <- Acum_hora %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = hora,
    values_from = conteo_acumulado
  ) %>%
  # Ordenar las columnas del acumulado
  select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
  drop_na(fecha)

# ---

## **Cálculo de la Diferencia (Matriz de Flujo Neto)**

# 6. Filtrar las tablas y extraer las matrices
data_exit <- df_pivoteado_acumulado %>% 
  filter(transit_direction_description == "Exit")

data_entry <- df_pivoteado_acumulado %>% 
  filter(transit_direction_description == "Entry")

# 7. Realizar la resta: Entry - Exit
# Las columnas 3 a 26 corresponden a las horas (0 a 23)
# Se asume que ambas matrices tienen el mismo orden de filas (fechas)
Matriz_difer <- data_entry[, 3:26] - data_exit[, 3:26]

# Opcional: Volver a unir las columnas de identificación al resultado
df_flujo_neto <- cbind(data_entry[, 1:2], Matriz_difer)

head(df_flujo_neto)

# colMeans(Matriz_difer)
# rowMeans(Matriz_difer)
# 
# Transformar a formato largo
df_diferencias_long_hora <- df_flujo_neto %>%
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

# Paso 1: Resumir los datos por franja_horaria (se mantiene igual)

## **Resúmenes por Franja Horaria y Tipo de Día (Lógica Corregida)**

# 1. Resumen por Franja Horaria
df_acum_franjahoraria_final <- df_final %>%
  # Agrupamos por los identificadores: fecha, dirección, franja
  group_by(fecha, transit_direction_description, franja_horaria) %>%
  # ¡CORRECCIÓN CLAVE! Tomamos el máximo valor acumulado, que es el valor final de la franja.
  summarise(conteo_final_franja = max(conteo_acumulado, na.rm = TRUE), .groups = "drop") %>%
  
  # Pivoteo para el análisis
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = franja_horaria,
    values_from = conteo_final_franja
  )

# 2. Resumen por Tipo de Día y Franja Horaria
df_acum_tipodia_final <- df_final %>%
  # Agrupamos por TODAS las categorías de interés
  group_by(fecha, tipo_dia, transit_direction_description, franja_horaria) %>%
  # Tomamos el máximo valor acumulado (el valor final de la franja/día)
  summarise(conteo_final_franja = max(conteo_acumulado, na.rm = TRUE), .groups = "drop") %>%
  
  # Pivoteo para el análisis
  pivot_wider(
    id_cols = c(fecha, tipo_dia, transit_direction_description),
    names_from = franja_horaria,
    values_from = conteo_final_franja
  )

# Mostramos los resultados (Opcional)
head(df_acum_franjahoraria_final)
head(df_acum_tipodia_final)



## **Cálculo de la Diferencia (Flujo Neto) por Franja Horaria**

# 1. Filtrar las tablas de Acumulado por Franja Horaria
data_exit_franja <- df_acum_franjahoraria_final %>% 
  filter(transit_direction_description == "Exit")

data_entry_franja <- df_acum_franjahoraria_final %>% 
  filter(transit_direction_description == "Entry")

# 2. Realizar la resta (Entry - Exit)
# Las columnas a restar ahora son las franjas horarias ('Madrugada', 'Maniana', 'Tarde', 'Noche').
# Necesitamos asegurar que el orden de las columnas de franja horaria sea el mismo.
columnas_franjas <- c("Madrugada", "Maniana", "Tarde", "Noche")

# Seleccionar solo las columnas de franja horaria y ordenar
Matriz_difer_franja <- data_entry_franja %>% select(all_of(columnas_franjas)) - 
  data_exit_franja %>% select(all_of(columnas_franjas))

# 3. Volver a unir las columnas de identificación al resultado
df_flujo_neto_franja <- data_entry_franja %>% 
  select(fecha, transit_direction_description) %>% 
  cbind(Matriz_difer_franja)

# Ajustar el nombre de la columna de dirección y eliminar la columna 'transit_direction_description'
df_flujo_neto_franja <- df_flujo_neto_franja %>% 
  select(-transit_direction_description) 

head(df_flujo_neto_franja)

# Asegurarse de que df_final solo tenga las columnas clave para la unión
df_categorias <- df_final %>%
  select(fecha, hora, tipo_dia, franja_horaria) %>%
  distinct() # Eliminar duplicados, dejando solo las combinaciones únicas

# 1. Unir el Flujo Neto Largo con las Categorías de Día/Franja
df_analisis_completo <- df_diferencias_long_hora %>%
  # Convertir 'hora' a numérico para la unión (ya que en df_final es numérico)
  mutate(hora = as.numeric(as.character(hora))) %>% 
  left_join(df_categorias, by = c("fecha", "hora")) %>%
  drop_na(tipo_dia) # Eliminar filas donde no se pudo determinar el tipo de día/franja

# 2. Resumir y Pivotar para obtener el VALOR PROMEDIO
df_promedio_pivoteado <- df_analisis_completo %>%
  group_by(tipo_dia, franja_horaria) %>%
  # Calcular el valor promedio (media) de la columna 'diferencia'
  summarise(
    flujo_neto_promedio = mean(diferencia, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pivotar: tipo_dia en filas y franja_horaria en columnas
  pivot_wider(
    names_from = franja_horaria,
    values_from = flujo_neto_promedio
  ) %>%
  # Reordenar las columnas de franja horaria para una mejor visualización
  select(tipo_dia, all_of(c("Madrugada", "Maniana", "Tarde", "Noche")))

# 3. Resumir y Pivotar para obtener la DESVIACIÓN ESTÁNDAR
df_desv_estandar_pivoteado <- df_analisis_completo %>%
  group_by(tipo_dia, franja_horaria) %>%
  # Calcular la desviación estándar (SD) de la columna 'diferencia'
  summarise(
    flujo_neto_desv_estandar = sd(diferencia, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pivotar: tipo_dia en filas y franja_horaria en columnas
  pivot_wider(
    names_from = franja_horaria,
    values_from = flujo_neto_desv_estandar
  ) %>%
  # Reordenar las columnas de franja horaria
  select(tipo_dia, all_of(c("Madrugada", "Maniana", "Tarde", "Noche")))

# Mostrar las tablas resultantes
message("Tabla de Flujo Neto PROMEDIO por Día y Franja Horaria:")
print(df_promedio_pivoteado)

message("\nTabla de Flujo Neto DESVIACIÓN ESTÁNDAR por Día y Franja Horaria:")
print(df_desv_estandar_pivoteado)

#########################
#   conteos acumulados  #
#########################

# 1. Preparar la tabla de Conteo con todas las categorías de tiempo
# Usamos df_final porque ya contiene el conteo, la hora y las categorías de día/franja.

df_conteo_analisis_completo <- df_final %>%
  # Seleccionamos las columnas necesarias para el análisis (Conteo por hora)
  select(fecha, hora, transit_direction_description, transacciones_por_hora, 
         tipo_dia, franja_horaria) %>%
  # Asegurar que 'hora' es numérico
  mutate(hora = as.numeric(hora)) %>%
  # Eliminar NAs si hay algún error en la categorización
  drop_na(tipo_dia)

# 2. Resumir y Pivotar para obtener el CONTEO PROMEDIO
df_conteo_promedio_pivoteado <- df_conteo_analisis_completo %>%
  group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
  # Calculamos el promedio de las transacciones por hora
  summarise(
    conteo_promedio = mean(transacciones_por_hora, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pivotar para tener la franja horaria en columnas
  pivot_wider(
    names_from = franja_horaria,
    values_from = conteo_promedio
  ) %>%
  # Reordenar las columnas para una mejor visualización
  select(tipo_dia, transit_direction_description, all_of(c("Madrugada", "Maniana", "Tarde", "Noche")))

# 3. Resumir y Pivotar para obtener la DESVIACIÓN ESTÁNDAR
df_conteo_desv_estandar_pivoteado <- df_conteo_analisis_completo %>%
  group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
  # Calculamos la desviación estándar de las transacciones por hora
  summarise(
    conteo_desv_estandar = sd(transacciones_por_hora, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pivotar para tener la franja horaria en columnas
  pivot_wider(
    names_from = franja_horaria,
    values_from = conteo_desv_estandar
  ) %>%
  # Ordenar las columnas
  select(tipo_dia, transit_direction_description, all_of(c("Madrugada", "Maniana", "Tarde", "Noche")))

# Mostrar las tablas resultantes
message("Tabla de Conteo PROMEDIO (Bruto) por Día, Dirección y Franja Horaria:")
print(df_conteo_promedio_pivoteado)

message("\nTabla de Conteo DESVIACIÓN ESTÁNDAR (Bruto) por Día, Dirección y Franja Horaria:")
print(df_conteo_desv_estandar_pivoteado)


# 1. Calcular Media (mu) y Desviación Estándar (sigma) por Grupo
df_sigma <- df_final %>%
  # Agrupamos por las dos categorías de interés
  group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
  summarise(
    # Calculamos la media (mu) y la desviación estándar (sigma) del conteo por hora
    mu = mean(transacciones_por_hora, na.rm = TRUE),
    sigma = sd(transacciones_por_hora, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calcular los límites del intervalo de +/- 2 sigma
  mutate(
    limite_superior = mu + (2 * sigma),
    limite_inferior = mu - (2 * sigma)
  )

# 1. Calcular Media (mu) y Desviación Estándar (sigma) por Grupo
# Usamos el mismo cálculo de sigma que antes, pero renombramos la tabla para claridad.
df_imputacion_stats <- df_final %>%
  # Agrupamos por las tres categorías clave:
  group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
  summarise(
    # Calculamos la media (mu) y la desviación estándar (sigma) del conteo por hora
    mu = mean(transacciones_por_hora, na.rm = TRUE),
    sigma = sd(transacciones_por_hora, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calcular los límites del intervalo de +/- 2 sigma
  mutate(
    limite_superior = mu + (2 * sigma),
    limite_inferior = mu - (2 * sigma)
  )

# 2. Unir las estadísticas a la tabla de datos original (df_final)
df_imputado <- df_final %>%
  # Unimos por las tres claves de agrupación
  left_join(
    df_imputacion_stats, 
    by = c("tipo_dia", "franja_horaria", "transit_direction_description")
  ) %>%
  
  # 3. Aplicar la Imputación por la Media
  mutate(
    # La lógica es:
    # SI (valor < límite inferior) O (valor > límite superior) ENTONCES mu (promedio del grupo)
    # SINO, mantener el valor original (transacciones_por_hora)
    transacciones_imputadas = ifelse(
      transacciones_por_hora < limite_inferior | transacciones_por_hora > limite_superior, 
      mu, # Reemplazar con la media del grupo (mu)
      transacciones_por_hora # Mantener el valor original
    ),
    
    # Asegurar que no haya valores imputados negativos (aunque mu debería ser >= 0)
    transacciones_imputadas = pmax(0, transacciones_imputadas)
  )

# 4. Reemplazar la columna original en df_final (como solicitaste)
# Esto actualiza tu data frame principal para análisis futuros.
df_final$transacciones_por_hora <- df_imputado$transacciones_imputadas

# Opcional: Mostrar una comparación para verificar la imputación
head(
  df_imputado %>% 
    select(fecha, hora, tipo_dia, franja_horaria, transit_direction_description,
           transacciones_por_hora, limite_inferior, limite_superior, transacciones_imputadas)
)


# Aseguramos que la columna 'fecha' se convierta al día del mes para las filas
df_pivot_base <- df_final %>%
  mutate(
    # Convertimos la fecha a solo el número del día (e.g., 1, 2, 3...)
    dia_del_mes = mday(fecha)
  )

# Generar la secuencia de nombres de columna para las horas (si no está definida)
columnas_horas_ordenadas <- as.character(0:23)

# 1. Realizar el Pivoting
df_pivoteado_imputado <- df_pivot_base %>%
  pivot_wider(
    # Las filas serán el día del mes y la dirección del tránsito
    id_cols = c(dia_del_mes, transit_direction_description),
    # Las columnas serán las horas
    names_from = hora,
    # Los valores son los conteos imputados/ajustados
    values_from = transacciones_por_hora,
    values_fill = 0
  ) %>%
  # 2. Ordenar las columnas
  select(dia_del_mes, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
  # Ordenar las filas por día del mes
  arrange(dia_del_mes) 

# Mostrar las primeras filas de la tabla
message("Tabla Pivoteada con Conteo Imputado: Día del Mes y Dirección vs. Hora:")
print(head(df_pivoteado_imputado))




