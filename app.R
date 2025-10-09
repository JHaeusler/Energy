# app.R

# --- CARGA DE LIBRERÍAS ---
library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(lubridate)
library(tidyr)
library(ggplot2)

# Definir el orden de las franjas horarias y días de la semana
orden_franjas <- c("Madrugada", "Maniana", "Tarde", "Noche")
dias_espanol <- c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")

# ==============================================================================
# === FUNCIONES DE PROCESAMIENTO ETL (Todo tu código pesado va aquí) ===
# ==============================================================================

# NOTA: Reemplaza la ruta_carpeta_OF fija con la variable de entrada de la función
ejecutar_etl <- function(ruta_carpeta, categorias_filtrar) {
  
  # 1. Carga, Limpieza y Combinación de Datos (Bloque 1)
  archivos_excel <- list.files(
    path = ruta_carpeta,
    pattern = "\\.(xls|xlsx)$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(archivos_excel) == 0) {
    stop("No se encontraron archivos .xls o .xlsx en la carpeta especificada.")
  }
  
  listas <- list()
  
  for (i in 1:length(archivos_excel)) {
    tryCatch({
      data_aux <- read_excel(archivos_excel[i], skip = 1) %>% clean_names()
      
      # Estandarización de tipos de datos
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
  
  # 2. Conteo, Complemento de Tiempo y Cálculo Acumulado (Bloque 2)
  df_general <- df_combinado %>%
    filter(tema_key %in% categorias_filtrar) %>%
    mutate(
      fecha = as_date(transaction_date),
      hora = hour(transaction_date)
    )
  
  # Si df_general está vacío después del filtro, detenemos la ejecución
  if (nrow(df_general) == 0) {
    stop("El filtro de categorías resultó en un dataframe vacío.")
  }
  
  Count_hora <- df_general %>% 
    count(fecha, transit_direction_description, hora, name = "transacciones_por_hora")
  
  fechas_unicas <- unique(df_general$fecha)
  tipos_transito <- unique(df_general$transit_direction_description)
  horas_completas <- 0:23
  
  df_horas_completas <- expand_grid(
    fecha = fechas_unicas,
    transit_direction_description = tipos_transito,
    hora = horas_completas
  )
  
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
  
  # 3. Imputación de Outliers (Bloque 3)
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
  
  # Reemplazar el conteo original por el imputado en df_final
  df_final$transacciones_por_hora <- df_imputado$transacciones_imputadas
  
  # Recalcular Acumulado con valores imputados
  Acum_hora_IMPUTADO <- df_final %>%
    arrange(fecha, hora) %>%
    group_by(fecha, transit_direction_description) %>%
    mutate(
      conteo_acumulado_IMPUTADO = cumsum(transacciones_por_hora), 
      .groups = 'drop'
    )
  
  # 4. Cálculo del Flujo Neto Ajustado (Bloque 4)
  columnas_horas_ordenadas <- as.character(0:23)
  
  df_pivoteado_acumulado_IMPUTADO <- Acum_hora_IMPUTADO %>%
    pivot_wider(
      id_cols = c(fecha, transit_direction_description),
      names_from = hora,
      values_from = conteo_acumulado_IMPUTADO
    ) %>%
    select(fecha, transit_direction_description, all_of(columnas_horas_ordenadas)) %>%
    drop_na(fecha)
  
  data_exit_IMPUTADO <- df_pivoteado_acumulado_IMPUTADO %>% filter(transit_direction_description == "Exit")
  data_entry_IMPUTADO <- df_pivoteado_acumulado_IMPUTADO %>% filter(transit_direction_description == "Entry")
  
  # Asegurar que las columnas sean numéricas antes de la resta
  entry_num <- data_entry_IMPUTADO %>% select(all_of(columnas_horas_ordenadas)) %>% mutate(across(everything(), as.numeric))
  exit_num <- data_exit_IMPUTADO %>% select(all_of(columnas_horas_ordenadas)) %>% mutate(across(everything(), as.numeric))
  
  # La resta requiere que ambas matrices tengan el mismo número de filas
  # Usaremos un join de las fechas para asegurar que coincidan (aunque tu código original asume que sí)
  
  if (nrow(entry_num) != nrow(exit_num)) {
    warning("El número de días con Entry y Exit es diferente. El cálculo del Flujo Neto puede ser inexacto.")
    # Implementación más segura:
    df_temp <- full_join(data_entry_IMPUTADO, data_exit_IMPUTADO, by = "fecha", suffix = c("_Entry", "_Exit"))
    
    # Rellenar con 0 si faltan días en alguna dirección para el cálculo
    for (col in columnas_horas_ordenadas) {
      df_temp[is.na(df_temp[[paste0(col, "_Entry")]]), paste0(col, "_Entry")] <- 0
      df_temp[is.na(df_temp[[paste0(col, "_Exit")]]), paste0(col, "_Exit")] <- 0
    }
    
    entry_cols <- df_temp %>% select(starts_with("transit_direction_description_Entry"), ends_with("_Entry"))
    exit_cols <- df_temp %>% select(starts_with("transit_direction_description_Exit"), ends_with("_Exit"))
    
    # Matriz_difer_IMPUTADO <- entry_cols[, -1] - exit_cols[, -1] # Asumiendo la columna de descripción como la primera
    # Por simplicidad y siguiendo el enfoque original (aunque peligroso):
    Matriz_difer_IMPUTADO <- entry_num[, 3:26] - exit_num[, 3:26]
    
  } else {
    Matriz_difer_IMPUTADO <- entry_num[, 3:26] - exit_num[, 3:26]
  }
  
  df_flujo_neto_IMPUTADO <- data_entry_IMPUTADO %>% 
    select(fecha) %>% 
    cbind(Matriz_difer_IMPUTADO)
  
  # 5. Transformar a formato largo el Flujo Neto Ajustado (para visualización)
  df_flujo_neto_IMPUTADO_long <- df_flujo_neto_IMPUTADO %>%
    pivot_longer(
      cols = all_of(columnas_horas_ordenadas),
      names_to = "hora",
      values_to = "flujo_neto_ajustado"
    ) %>%
    mutate(
      hora = factor(hora, levels = 0:23),
      fecha_join = fecha
    ) %>%
    left_join(
      df_final %>% select(fecha, tipo_dia, franja_horaria) %>% distinct(),
      by = c("fecha_join" = "fecha")
    ) %>%
    select(-fecha_join)
  
  # Devolver todos los dataframes necesarios para las visualizaciones
  return(list(
    df_final = df_final,
    df_flujo_neto_IMPUTADO_long = df_flujo_neto_IMPUTADO_long,
    df_imputado = df_imputado # Útil para la tabla de imputación
  ))
}

# ==============================================================================
# === INTERFAZ DE USUARIO (UI) ===
# ==============================================================================

ui <- fluidPage(
  
  # Título principal de la aplicación
  titlePanel(title = div("Análisis Dinámico de Tráfico (ETL + Shiny)", 
                         style = "color: #0072B2; font-weight: bold;")),
  
  # Usar navbarPage para las pestañas principales
  navbarPage(
    title = "",
    
    # --- PESTAÑA 1: CARGA DE DATOS Y FILTROS INICIALES ---
    tabPanel("1. Carga y Filtros", icon = icon("folder-open"),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 h4("Configuración de Datos"),
                 
                 # NOTA: En Shiny, no es fácil seleccionar carpetas. Usamos un input de texto.
                 # Debes poner una ruta válida para que funcione.
                 textInput("ruta_carpeta", 
                           "Ruta de la Carpeta de Archivos Excel:", 
                           value = "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy/Datos/Old_Format/2025"),
                 
                 selectInput("sel_categorias", 
                             "Categorías (tema_key) a Incluir:", 
                             choices = c("TK54RT54", "TK55RT55", "TK56RT56", "TK57RT57", "TK58RT58",
                                         "TK60RT60", "TK72RT72", "TK76RT76", "TK77RT77", "TK78RT78"),
                             multiple = TRUE, 
                             selected = c("TK54RT54", "TK55RT55", "TK56RT56", "TK57RT57", "TK58RT58",
                                          "TK60RT60", "TK72RT72", "TK76RT76", "TK77RT77", "TK78RT78")),
                 
                 actionButton("btn_ejecutar_etl", "Ejecutar Carga y Pre-Cálculo ETL", 
                              icon = icon("cogs"), class = "btn-primary"),
                 
                 hr(),
                 p("⚠️ El proceso ETL es intensivo. Haz clic y espera la confirmación antes de ver gráficos.")
               ),
               
               mainPanel(
                 width = 8,
                 h4("Estado del Proceso ETL"),
                 verbatimTextOutput("etl_status"),
                 dataTableOutput("tabla_resumen_datos")
               )
             )
    ),
    
    # --- PESTAÑA 2: VISUALIZACIÓN DE PATRONES GENERALES ---
    tabPanel("2. Patrones Generales", icon = icon("chart-line"),
             fluidRow(
               column(12, h3("Análisis de Flujo Neto y Distribución Diaria")),
               column(12, plotOutput("plot_flujo_neto_promedio", height = "450px")), # Gráfico 2
               column(12, hr()),
               column(12, plotOutput("plot_boxplot_flujoneto", height = "450px")) # Gráfico 6
             )
    ),
    
    # --- PESTAÑA 3: ANÁLISIS DETALLADO POR FRANJA ---
    tabPanel("3. Análisis Detallado (Boxplots)", icon = icon("sliders"),
             fluidRow(
               column(12, h3("Distribución de Transacciones por Franja Horaria (L-V)")),
               column(12, plotOutput("plot_boxplot_franjas_lv", height = "450px")), # Gráfico 7
               column(12, hr()),
               column(12, plotOutput("plot_boxplot_diario_franja", height = "600px")) # Gráfico 8
             )
    ),
    
    # --- PESTAÑA 4: HEATMAP Y VARIABILIDAD ---
    tabPanel("4. Heatmap y Variabilidad", icon = icon("table"),
             fluidRow(
               column(12, h3("Visualización de Conteo por Hora y Día")),
               column(12, plotOutput("plot_heatmap", height = "600px")), # Gráfico 3
               column(12, hr()),
               column(12, plotOutput("plot_desv_estandar", height = "400px")) # Gráfico 4
             )
    )
  )
)

# ==============================================================================
# === LÓGICA DEL SERVIDOR (SERVER) ===
# ==============================================================================

server <- function(input, output, session) {
  
  # Objeto reactivo para contener todos los resultados del ETL
  # Se ejecuta SÓLO cuando se presiona 'btn_ejecutar_etl'
  datos_etl <- eventReactive(input$btn_ejecutar_etl, {
    
    # Mostrar mensaje de inicio
    output$etl_status <- renderPrint({
      req(input$ruta_carpeta, length(input$sel_categorias) > 0)
      cat(paste("Iniciando proceso ETL para:", input$ruta_carpeta, "\n"))
    })
    
    # Ejecutar la función pesada y manejar posibles errores
    tryCatch({
      # Llamada a la función
      resultados <- ejecutar_etl(input$ruta_carpeta, input$sel_categorias)
      
      # Mostrar éxito
      output$etl_status <- renderPrint({
        cat(paste("✅ Proceso ETL completado con éxito.", "\n"))
        cat(paste("Fechas analizadas:", min(resultados$df_final$fecha), "a", max(resultados$df_final$fecha), "\n"))
      })
      
      return(resultados)
      
    }, error = function(e) {
      output$etl_status <- renderPrint({
        cat(paste("❌ ERROR en el proceso ETL:", e$message, "\n"))
      })
      # Devolver NULL en caso de error para evitar que los gráficos fallen
      return(NULL)
    })
  }, ignoreNULL = FALSE) 
  
  # --- PESTAÑA 1: Carga y Resumen ---
  
  # Tabla de resumen simple (ej. primeros 6 registros de df_final)
  output$tabla_resumen_datos <- renderDataTable({
    req(datos_etl()) # Requiere que el ETL haya terminado sin error
    datos_etl()$df_final %>% 
      head(6) %>% 
      select(fecha, hora, transit_direction_description, transacciones_por_hora, conteo_acumulado)
  }, options = list(pageLength = 6))
  
  # --- PESTAÑA 2: Patrones Generales ---
  
  # Gráfico 2: Línea de Flujo Neto Promedio por Hora y Tipo de Día
  output$plot_flujo_neto_promedio <- renderPlot({
    req(datos_etl())
    
    data_long <- datos_etl()$df_flujo_neto_IMPUTADO_long
    
    df_promedio_flujo_neto_por_tipo_dia <- data_long %>%
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
      scale_x_discrete(breaks = as.character(seq(0, 23, by = 2)))
  })
  
  # Gráfico 6: Boxplot de Flujo Neto por Día de la Semana
  output$plot_boxplot_flujoneto <- renderPlot({
    req(datos_etl())
    
    df_boxplot_flujo_neto <- datos_etl()$df_flujo_neto_IMPUTADO_long %>%
      mutate(
        dia_Sem = wday(fecha, week_start = 1),
        dia_ordenado = factor(
          dia_Sem,
          levels = 1:7,
          labels = dias_espanol,
          ordered = TRUE
        )
      )
    
    df_stats_locales <- df_boxplot_flujo_neto %>%
      group_by(dia_ordenado, tipo_dia) %>%
      summarise(
        media_local = mean(flujo_neto_ajustado, na.rm = TRUE),
        p10_local = quantile(flujo_neto_ajustado, 0.10, na.rm = TRUE),
        p90_local = quantile(flujo_neto_ajustado, 0.90, na.rm = TRUE),
        .groups = 'drop'
      )
    
    ggplot(df_boxplot_flujo_neto, aes(x = dia_ordenado, y = flujo_neto_ajustado)) +
      geom_boxplot(aes(fill = tipo_dia), alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray20", linewidth = 0.8) +
      geom_point(data = df_stats_locales, 
                 aes(y = media_local, color = "Media"), 
                 size = 3.5, shape = 18) +
      geom_point(data = df_stats_locales, 
                 aes(y = p90_local, color = "P90"), 
                 size = 2, shape = 17) +
      geom_point(data = df_stats_locales, 
                 aes(y = p10_local, color = "P10"), 
                 size = 2, shape = 17) +
      labs(
        title = "Flujo Neto Ajustado: Distribución Semanal con Estadísticas Locales",
        x = "Día de la Semana",
        y = "Flujo Neto Ajustado (Entradas - Salidas)",
        fill = "Tipo de Día",
        color = "Estadística"
      ) +
      scale_color_manual(
        values = c("Media" = "black", "P10" = "red", "P90" = "red"),
        labels = c("Media", "P10", "P90")
      ) +
      theme_minimal()
  })
  
  # --- PESTAÑA 3: Análisis Detallado (Boxplots) ---
  
  # Gráfico 7: Boxplot de Conteo Absoluto por Franja Horaria (Lunes a Viernes)
  output$plot_boxplot_franjas_lv <- renderPlot({
    req(datos_etl())
    
    df_imputado <- datos_etl()$df_imputado
    
    df_boxplot_franjas_lv <- df_imputado %>%
      filter(tipo_dia == "L-V") %>%
      mutate(
        franja_ordenada = factor(franja_horaria, levels = orden_franjas, ordered = TRUE)
      )
    
    ggplot(df_boxplot_franjas_lv, aes(x = franja_ordenada, y = transacciones_imputadas)) +
      geom_boxplot(aes(fill = transit_direction_description), alpha = 0.8) +
      facet_wrap(~transit_direction_description, scales = "free_y", 
                 labeller = as_labeller(c(Entry = "Entradas (L-V)", Exit = "Salidas (L-V)"))) +
      labs(
        title = "Distribución del Conteo Absoluto por Franja Horaria (Lunes a Viernes)",
        x = "Franja Horaria",
        y = "Conteo por Hora (Transacciones Imputadas)"
      ) +
      scale_fill_manual(values = c("Entry" = "#0072B2", "Exit" = "#D55E00")) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Gráfico 8: Boxplot de Conteo Absoluto Desglosado por Día y Franja Horaria (L-V)
  output$plot_boxplot_diario_franja <- renderPlot({
    req(datos_etl())
    
    df_imputado <- datos_etl()$df_imputado
    dias_espanol_lv <- c("Lun", "Mar", "Mié", "Jue", "Vie")
    
    df_boxplot_franjas_diario <- df_imputado %>%
      filter(tipo_dia == "L-V") %>%
      mutate(
        franja_ordenada = factor(franja_horaria, levels = orden_franjas, ordered = TRUE),
        dia_Sem = wday(fecha, week_start = 1),
        dia_ordenado = factor(dia_Sem, levels = 1:5, labels = dias_espanol_lv, ordered = TRUE)
      )
    
    df_stats_locales_diario <- df_boxplot_franjas_diario %>%
      group_by(dia_ordenado, franja_ordenada, transit_direction_description) %>%
      summarise(
        media_local = mean(transacciones_por_hora, na.rm = TRUE),
        p10_local = quantile(transacciones_por_hora, 0.10, na.rm = TRUE),
        p90_local = quantile(transacciones_por_hora, 0.90, na.rm = TRUE),
        .groups = 'drop'
      )
    
    ggplot(df_boxplot_franjas_diario, aes(x = franja_ordenada, y = transacciones_por_hora)) +
      geom_boxplot(aes(fill = transit_direction_description), alpha = 0.8) +
      geom_point(data = df_stats_locales_diario, aes(y = media_local, color = "Media"), size = 2.5, shape = 18) +
      geom_point(data = df_stats_locales_diario, aes(y = p90_local, color = "P90"), size = 1.5, shape = 17) +
      geom_point(data = df_stats_locales_diario, aes(y = p10_local, color = "P10"), size = 1.5, shape = 17) +
      facet_grid(transit_direction_description ~ dia_ordenado, scales = "free_y", 
                 labeller = labeller(transit_direction_description = c(Entry = "Entradas", Exit = "Salidas"))) +
      labs(
        title = "Conteo Horario Imputado por Franja y Día (Lunes a Viernes)",
        x = "Franja Horaria",
        y = "Conteo por Hora (Transacciones Imputadas)",
        color = "Estadística"
      ) +
      scale_fill_manual(values = c("Entry" = "#0072B2", "Exit" = "#D55E00"), guide = "none") +
      scale_color_manual(
        values = c("Media" = "black", "P10" = "red", "P90" = "red")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), legend.position = "bottom")
  })
  
  # --- PESTAÑA 4: Heatmap y Variabilidad ---
  
  # Gráfico 3: Heatmap del Conteo de Transacciones Imputadas
  output$plot_heatmap <- renderPlot({
    req(datos_etl())
    
    df_final <- datos_etl()$df_final
    
    df_heatmap_data <- df_final %>%
      mutate(
        dia_del_mes = mday(fecha),
        hora_factor = factor(hora, levels = 0:23)
      ) %>%
      select(dia_del_mes, transit_direction_description, hora_factor, transacciones_por_hora, tipo_dia)
    
    ggplot(df_heatmap_data, aes(x = hora_factor, y = interaction(dia_del_mes, transit_direction_description), fill = transacciones_por_hora)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Conteo Imputado") +
      labs(
        title = "Heatmap del Conteo de Transacciones Imputadas por Hora, Día y Dirección",
        x = "Hora del Día",
        y = "Día del Mes - Dirección"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 6), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(~tipo_dia, scales = "free_y")
  })
  
  # Gráfico 4: Desviación Estándar del Conteo Imputado
  output$plot_desv_estandar <- renderPlot({
    req(datos_etl())
    
    df_final <- datos_etl()$df_final
    
    df_imputado_stats_final <- df_final %>%
      group_by(tipo_dia, franja_horaria, transit_direction_description) %>%
      summarise(
        conteo_desv_estandar_IMPUTADO = sd(transacciones_por_hora, na.rm = TRUE),
        .groups = 'drop'
      )
    
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
  })
}

# Ejecutar la Aplicación
shinyApp(ui = ui, server = server)