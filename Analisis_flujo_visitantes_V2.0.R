# repositorio <- "http://cran.us.r-project.org"
# install.packages("tidyr", repos = repositorio)
# install.packages("readxl", repos = repositorio)
# install.packages("dplyr", repos = repositorio)
# install.packages("purrr", repos = repositorio)
# install.packages("janitor", repos = repositorio)
# install.packages("lubridate", repos = repositorio)
# 
#
# Carga de paquetes
library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(lubridate)
library(tidyr)
library(ggplot2)

# Define la ruta a tu carpeta principal
# Es importante que reemplaces esta ruta con la de tu propio sistema
# ruta_carpeta_OF <- "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy/Datos/Old_Format/2025"
ruta_carpeta_OF <- "D:/Github/Energy/Datos/Old_Format/2025"

# --- 1. Lectura y Consolidación de Archivos ---

# Obtener la lista de rutas de todos los archivos .xls y .xlsx de forma segura
archivos_excel <- list.files(
  path = ruta_carpeta_OF,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

# Leer y combinar todos los data frames en uno solo usando purrr::map_dfr
df_combinado <- purrr::map_dfr(archivos_excel, ~{
  tryCatch({
    # Leer el archivo y limpiar nombres de columnas
    data_aux <- readxl::read_excel(., skip = 1) %>% 
      janitor::clean_names()
    
    # Corregir tipos de datos usando dplyr::across para un código más conciso
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

# --- 2. Preparación y Estandarización de Datos ---

# Lista de categorías de conteo a filtrar
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

# Unir y rellenar los datos para asegurar que no falten horas
df_resumen_completo <- left_join(
  df_horas_completas,
  df_resumen,
  by = c("fecha", "transit_direction_description", "hora")
) %>%
  replace_na(list(conteo_por_hora = 0))

# --- 3. Análisis Descriptivo y de Variabilidad ---

# Añadir las columnas de franja horaria y tipo de día
df_final <- df_resumen_completo %>%
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

# Agrupar y resumir los datos para obtener el promedio y la desviación estándar
df_analisis_final <- df_final %>%
  group_by(tipo_dia, transit_direction_description, franja_horaria) %>%
  summarise(
    conteo_promedio = mean(conteo_por_hora, na.rm = TRUE),
    desviacion_estandar = sd(conteo_por_hora, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4. Detección y Corrección de Datos Anómalos ---

# Unir la tabla de datos original con la tabla de promedios y desviaciones estándar
df_correccion <- left_join(
  df_final,
  df_analisis_final,
  by = c("tipo_dia", "transit_direction_description", "franja_horaria")
)

# Implementar la lógica de corrección: reemplazar valores fuera de 2 desviaciones estándar
df_corregido <- df_correccion %>%
  mutate(
    conteo_corregido = if_else(
      conteo_por_hora > (conteo_promedio + 2 * desviacion_estandar) |
        conteo_por_hora < (conteo_promedio - 2 * desviacion_estandar),
      conteo_promedio,
      conteo_por_hora
    )
  )

# --- 5. Resumen Final y Preparación de Tablas ---

# Opcional: Pivotear la tabla de datos corregidos
df_pivoteado_corregido <- df_corregido %>%
  group_by(fecha, transit_direction_description, franja_horaria) %>%
  summarise(conteo_corregido = sum(conteo_corregido), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = franja_horaria,
    values_from = conteo_corregido
  )

# Opcional: Pivotear los promedios y desviaciones estándar para un resumen
df_promedios_pivot <- df_analisis_final %>%
  pivot_wider(
    id_cols = c(tipo_dia, transit_direction_description),
    names_from = franja_horaria,
    values_from = c(conteo_promedio, desviacion_estandar)
  )


df_difer_acum <- df_corregido %>%
  group_by(fecha, transit_direction_description, hora) %>%
  summarise(conteo_corregido = sum(conteo_corregido), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = hora,
    values_from = conteo_corregido
  )

aux_data <- df_pivoteado_corregido %>% 
         filter(transit_direction_description == "Entry")


ggplot(aux_data,
       aes(x = fecha, y = Madrugada)) +
  geom_line(color = "blue") +
  geom_point(color = "black") + # Opcional: añade puntos para cada dato
  labs(
    title = "Conteo de Entradas Corregido por Fecha (Franja: Madrugada)",
    x = "Fecha",
    y = "Conteo Corregido"
  ) +
  theme_minimal()
  

# Asegúrate de tener la librería ggplot2 cargada
library(ggplot2)

# Filtra el dataframe corregido para incluir solo las "Entradas"
# para que el gráfico muestre todas las franjas horarias de entradas


ggplot() +
  
       geom_line(aes(x = fecha, y = Madrugada), aux_data) +
  geom_line(aes(x = fecha, y = Madrugada), aux_data) +
  geom_line(aes(x = fecha, y = Maniana), aux_data) +
  geom_line(aes(x = fecha, y = Tarde), aux_data) +
  geom_line(aes(x = fecha, y = Noche), aux_data)+
  labs(
    title = "Conteo de Entradas Corregido por Fecha y Franja Horaria",
    x = "Fecha",
    y = "Conteo Corregido",
    color = "Franja Horaria"
  ) +
  theme_minimal()
  
  