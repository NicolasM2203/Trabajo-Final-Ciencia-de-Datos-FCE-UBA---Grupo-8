# CONFIGURACIÓN GLOBAL DEL PROYECTO
# =============================================================================

# 1. Limpiar entorno y opciones
rm(list = ls()) # Limpia la memoria antes de empezar
options(scipen = 999) # Evitar notación científica

# 2. Gestión de Librerías (Instalación automática)
# Verifica si pacman está instalado, si no, lo instala
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  here,         # Rutas relativas
  tidyverse,    # Incluye ggplot2, dplyr, readr, etc.
  readxl,       # Leer Excel
  lubridate,    # Manejo de fechas
  scales,       # Formato de ejes en gráficos
  janitor,       # Limpieza de nombres (muy recomendado)
  naniar,      # Gestión y visualización de datos faltantes (NA's)
  patchwork    # Combinar múltiples gráficos de ggplot2
)

# Definir directorios de forma reproducible
if (!exists("proyecto_dir")) {
  proyecto_dir <- here::here()  # Usa el paquete 'here'
  # Alternativa manual:
  # proyecto_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
}

# Rutas principales
dir_data_raw <- file.path(proyecto_dir, "data", "raw")
dir_data_processed <- file.path(proyecto_dir, "data", "processed")
dir_data_clean <- file.path(proyecto_dir, "data", "clean")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")
dir_outputs_tables <- file.path(proyecto_dir, "outputs", "tables")
dir_outputs_reports <- file.path(proyecto_dir, "outputs", "reports")
dir_functions <- file.path(proyecto_dir, "functions")

# Crear directorios si no existen
dirs_crear <- c(dir_data_raw, dir_data_processed, dir_data_clean,
                dir_outputs_figures, dir_outputs_tables, dir_outputs_reports, 
                dir_functions)

for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# Configuración Gráfica 
theme_set(theme_minimal() +
            theme(legend.position = "bottom"))

# Funciones para mensajes consistentes
mensaje_exito <- function(texto) {
  cat("✅", texto, "\n")
}

mensaje_proceso <- function(texto) {
  cat("🔄", texto, "...\n")
}

# 6. Cargar Funciones Personalizadas ------------------------------------------
# Busca todos los archivos .R dentro de la carpeta functions
archivos_funciones <- list.files(path = dir_functions, 
                                 pattern = "\\.R$", 
                                 full.names = TRUE)

# Los lee uno por uno (hace source)
if(length(archivos_funciones) > 0) {
  walk(archivos_funciones, source)
  mensaje_exito("Funciones personalizadas cargadas correctamente")
} else {
  mensaje_proceso("No se encontraron scripts de funciones para cargar.")
}


mensaje_exito("Configuración cargada correctamente")