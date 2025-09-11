# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(purrr)

# Define la ruta a tu carpeta principal
ruta_carpeta <- "~/Personal/Escuela Pol. Feminista/Feministadística/GitHub/Energy"

# Obtener la lista de rutas de todos los archivos .xls y .xlsx
archivos_excel <- list.files(
  path = ruta_carpeta,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE,
  recursive = TRUE
)

# Crear una lista vacía para almacenar los data frames
listas <- list()

# Bucle para leer cada archivo y almacenarlo de manera segura
for (i in 1:length(archivos_excel)) {
  
  # Usar tryCatch para manejar posibles errores
  tryCatch({
    
    # Leer el archivo actual
    data_aux <- read_excel(archivos_excel[i])
    
    # Encontrar la fila del marcador "Views"
    row_aux <- which(data_aux$`Temaline Interface` == "Views")
    
    # Si se encontró el marcador, se filtra la tabla. Si no, se deja la tabla completa.
    if (length(row_aux) > 0) {
      data_aux <- data_aux[1:(row_aux[1] - 1), ]
    }
    
    # Renombrar la tercera columna
    colnames(data_aux)[3] <- "operator"
    
    # Almacenar el data frame en la lista
    listas[[i]] <- data_aux
    
  }, error = function(e) {
    # Si hay un error, muestra una advertencia y salta al siguiente archivo
    warning("Error procesando el archivo: ", archivos_excel[i], " - Mensaje: ", e$message)
  })
}

# Combina todos los data frames de la lista en uno solo
df_combinado <- bind_rows(listas)

message("Datos combinados exitosamente.")