# ==============================================================================
# CARGA DE BASES DE COMPLEJIDAD ECONÓMICA PROVINCIAL
# ------------------------------------------------------------------------------
# Responsabilidad: Limpiar los datos originales.
# ==============================================================================

# 1. Configuración Utilizand el archivo global con las pre-configuraciones
library(here)
source(here("config", "global.R"))

# 2. Cargar Datos Procesados desde .rds

mensaje_proceso("Leyendo archivos intermedios para iniciar la limpieza...")

# Definimos las rutas exactas de los archivos .rds con los nombres que dejamos en la carga de datos 
ruta_rds_export    <- file.path(dir_data_processed, "df_prod_export_processed.rds")
ruta_rds_potencial <- file.path(dir_data_processed, "df_prod_potencial_processed.rds")

# Cargamos los archivos en memoria
df_prod_export    <- readRDS(ruta_rds_export)
df_prod_potencial <- readRDS(ruta_rds_potencial)

mensaje_exito("Bases cargadas correctamente. Inicia la limpieza.")

#3. Aplicación de la limpieza

print("Aplicando limpieza a la base de Exportaciones (prod_export)...")

df_prod_export_limpio <- limpiar_nombres(df_prod_export)
df_prod_export_limpio <- limpiar_prop(df_prod_export_limpio)


print("Aplicando limpieza a la base de Potencial Productivo (prod_pontecial)...")

df_prod_potencial_limpio <- limpiar_nombres(df_prod_potencial)
df_prod_potencial_limpio <- limpiar_prop(df_prod_potencial_limpio)

# 4. Guardar los datos limpios

# Se utiliza la carpeta data/clean/para los resultados de la limpieza y transformación.

ruta_salida_export <- here("data", "clean", "df_prod_export_limpio.rds")
ruta_salida_potencial <- here("data", "clean", "df_prod_potencial_limpio.rds")

saveRDS(df_prod_export_limpio, file = ruta_salida_export)
saveRDS(df_prod_potencial_limpio, file = ruta_salida_potencial)

# Documentación 
cat("\nDatos limpios guardados con exito",
    "\n ->", ruta_salida_export,
    "\n ->", ruta_salida_potencial, "\n")

# ==============================================================================
# REDUCCION DE COLUMNAS INUTILES PARA EL PROYECTO
# ------------------------------------------------------------------------------

df_prod_export_reducido <- df_prod_export_limpio %>% 
  select(-cod_provincia, -cod_ncm_6d)

df_prod_potencial_reducido <- df_prod_potencial_limpio %>%
  select(-cod_provincia, -cod_ncm_6d, -fob_mundial, -ranking_est_conservadora, -ranking_est_equilibrada, -ranking_est_desafiante)

# 5. Guardar los datos procesados

# Se utiliza la carpeta data/clean/ para los resultados de la limpieza y transformación.

ruta_salida_export <- here("data", "clean", "df_prod_export_reducido.rds")
ruta_salida_potencial <- here("data", "clean", "df_prod_potencial_reducido.rds")

saveRDS(df_prod_export_reducido, file = ruta_salida_export)
saveRDS(df_prod_potencial_reducido, file = ruta_salida_potencial)

# Documentación
cat("\nDatos reducidos guardados con exito",
    "\n ->", ruta_salida_export,
    "\n ->", ruta_salida_potencial, "\n")