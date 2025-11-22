# scripts/01_carga_datos.R

# ==============================================================================
# CARGA DE BASES DE COMPLEJIDAD ECONÓMICA PROVINCIAL
# ------------------------------------------------------------------------------
# Responsabilidad: Cargar los datos originales (raw) sin modificarlos.
# El script es autocontenido y usa rutas relativas. [3]
# ==============================================================================

# 1. Configuración de dependencias (Autocontención)
# Cargamos librerías necesarias para rutas y lectura de archivos.
library(here) # Para utilizar rutas relativas desde la raíz del proyecto. [3]
library(readr) # Librería eficiente para leer archivos CSV.

# 2. Definición de Rutas
ruta_export <- here("data", "raw", "prod_export.csv") 
ruta_potencial <- here("data", "raw", "prod_potencial.csv") 

# 3. Ejecución de la Carga de Datos (Utilizando la Función estándar para cargar datos) [2]

# Se asigna la base cargada a una variable descriptiva (df_prod_export)
df_prod_export <- read_csv(ruta_export)

# Documentación de metadatos de la carga [3]
cat("\n--- Base de Exportaciones (prod_export) ---",
    "\nFuente:", ruta_export,
    "\nDimensiones:", nrow(df_prod_export), "filas x", ncol(df_prod_export), "columnas\n")

# Se asigna la base cargada a una variable descriptiva (df_prod_potencial)
df_prod_potencial <- read_csv(ruta_potencial)

# Documentación de metadatos de la carga [3]
cat("\n--- Base de Potencial Productivo (prod_pontecial) ---",
    "\nFuente:", ruta_potencial,
    "\nDimensiones:", nrow(df_prod_potencial), "filas x", ncol(df_prod_potencial), "columnas\n")

# 4. Finalización
print("¡Carga de bases de datos de complejidad económica completada!")