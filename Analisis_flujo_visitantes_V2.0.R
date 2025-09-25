# 1. Añadir las columnas de franja horaria y tipo de día
df_final <- df_resumen_completo %>%
  mutate(
    franja_horaria = case_when(
      hora >= 0 & hora <= 5 ~ "Madrugada",
      hora >= 6 & hora <= 12 ~ "Mañana",
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

# 2. Agrupar y resumir los datos para obtener el promedio y la desviación estándar
df_analisis_final <- df_final %>%
  group_by(tipo_dia, transit_direction_description, franja_horaria) %>%
  summarise(
    conteo_promedio = mean(conteo_por_hora, na.rm = TRUE),
    desviacion_estandar = sd(conteo_por_hora, na.rm = TRUE),
    .groups = "drop"
  )

# --- INICIO DE LA CORRECCIÓN DE DATOS --- #

# 3. Unir la tabla de datos original con la tabla de promedios y desviaciones estándar
df_correccion <- left_join(
  df_final,
  df_analisis_final,
  by = c("tipo_dia", "transit_direction_description", "franja_horaria")
)

# 4. Implementar la lógica de corrección
df_corregido <- df_correccion %>%
  mutate(
    conteo_corregido = if_else(
      # Condición para identificar si el valor está fuera de 2 desviaciones estándar
      conteo_por_hora > (conteo_promedio + 2 * desviacion_estandar) |
        conteo_por_hora < (conteo_promedio - 2 * desviacion_estandar),
      # Si la condición se cumple, se reemplaza por el promedio
      conteo_promedio,
      # Si no, se mantiene el valor original
      conteo_por_hora
    )
  )

# --- FIN DE LA CORRECCIÓN --- #

# La nueva columna 'conteo_corregido' contiene los datos ajustados.
# Ahora puedes usar esta columna para tus análisis y pivoteos posteriores.

# Ejemplo de pivot para verificar el resultado
df_pivoteado_corregido <- df_corregido %>%
  group_by(fecha, transit_direction_description, franja_horaria) %>%
  summarise(conteo_corregido = sum(conteo_corregido), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(fecha, transit_direction_description),
    names_from = franja_horaria,
    values_from = conteo_corregido
  )