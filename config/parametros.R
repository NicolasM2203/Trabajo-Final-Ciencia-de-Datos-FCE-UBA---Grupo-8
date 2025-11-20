# =============================================================================
# CONFIGURACIÓN GLOBAL DEL PROYECTO
# =============================================================================

# Limpiar entorno
rm(list = ls())

# Configurar opciones globales
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Evitar notación científica

# Librerías del proyecto
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(here)

# Definir directorios de forma reproducible
if (!exists("proyecto_dir")) {
  proyecto_dir <- here::here()  # Usa el paquete 'here'
  # Alternativa manual:
  # proyecto_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
}

# Rutas principales
dir_data_raw <- file.path(proyecto_dir, "data", "raw")
dir_data_processed <- file.path(proyecto_dir, "data", "processed")
dir_data_external <- file.path(proyecto_dir, "data", "external")
dir_outputs_figures <- file.path(proyecto_dir, "outputs", "figures")
dir_outputs_tables <- file.path(proyecto_dir, "outputs", "tables")

# Crear directorios si no existen
dirs_crear <- c(dir_data_raw, dir_data_processed, dir_data_external,
                dir_outputs_figures, dir_outputs_tables)

for (dir in dirs_crear) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# Parámetros del análisis
YEAR_ANALISIS <- 2024
TRIMESTRE_ANALISIS <- 3
UMBRAL_POBREZA <- 85000  # Ejemplo para análisis de pobreza

# Funciones para mensajes consistentes
mensaje_exito <- function(texto) {
  cat("✅", texto, "\n")
}

mensaje_proceso <- function(texto) {
  cat("🔄", texto, "...\n")
}

mensaje_exito("Configuración cargada correctamente")