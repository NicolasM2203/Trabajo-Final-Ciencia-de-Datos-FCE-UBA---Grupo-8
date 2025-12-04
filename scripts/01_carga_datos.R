# ==============================================================================
# CARGA DE BASES DE COMPLEJIDAD ECONÓMICA PROVINCIAL
# ------------------------------------------------------------------------------
# Responsabilidad: Cargar los datos originales (raw) sin modificarlos.
# ==============================================================================

# 1. Configuración de dependencias (Utilizo el archivo global con las pre-configuraciones)
library(here)
source(here("config", "global.R"))

# 2. Definición de Rutas
ruta_export    <- file.path(dir_data_raw, "prod_export.csv") #definimos las rutas especificas de los csv a partir de la variable de carpeta ya creada
ruta_potencial <- file.path(dir_data_raw, "prod_potencial.csv")

# 3. Ejecución de la Carga de Datos
# Se asigna la base cargada a una variable descriptiva 
df_prod_export <- read_csv(ruta_export)

# Documentación de metadatos de la carga 
cat("\n--- Base de Exportaciones (prod_export) ---",
    "\nFuente:", ruta_export,
    "\nDimensiones:", nrow(df_prod_export), "filas x", ncol(df_prod_export), "columnas\n")

# Se asigna la otra base cargada a una variable descriptiva 
df_prod_potencial <- read_csv(ruta_potencial)

# Documentación de metadatos de la carga 
cat("\n--- Base de Potencial Productivo (prod_pontecial) ---",
    "\nFuente:", ruta_potencial,
    "\nDimensiones:", nrow(df_prod_potencial), "filas x", ncol(df_prod_potencial), "columnas\n")

# 4. Guardado Temporal (Para que el script 02 los pueda usar)
# Guardamos en formato .rds (rápido y liviano) en la carpeta processed
mensaje_proceso("Guardando archivos intermedios para el siguiente script...")

saveRDS(df_prod_export, file.path(dir_data_processed,"df_prod_export_processed.rds"))
saveRDS(df_prod_potencial, file.path(dir_data_processed, "df_prod_potencial_processed.rds"))

# 5. Finalización
print("¡Carga de bases de datos de complejidad económica completada!")
mensaje_exito("¡Carga de bases de datos de complejidad económica completada!")