# ==============================================================================
# CARGA DE BASES DE COMPLEJIDAD ECONÓMICA PROVINCIAL
# ------------------------------------------------------------------------------
# Responsabilidad: Limpiar los datos originales.
# El script es autocontenido y usa rutas relativas. [3]
# ==============================================================================

library(dplyr) 

#3. Aplicación de la limpieza

print("Aplicando limpieza a la base de Exportaciones (prod_export)...")


df_prod_export_limpio <- limpiar_prop(df_prod_export)


print("Aplicando limpieza a la base de Potencial Productivo (prod_pontecial)...")

df_prod_potencial_limpio <- limpiar_prop(df_prod_potencial)

# 4. Guardar los datos procesados

# Se utiliza la carpeta data/processed/ para los resultados de la limpieza y transformación [3].
# Es buena práctica agregar un timestamping o sufijo claro (ej: _limpio)

ruta_salida_export <- here("data", "processed", "df_prod_export_limpio.rds")
ruta_salida_potencial <- here("data", "processed", "df_prod_pontecial_limpio.rds")

saveRDS(df_prod_export_limpio, file = ruta_salida_export)
saveRDS(df_prod_potencial_limpio, file = ruta_salida_potencial)

# Documentación (Logging y documentación automática)
cat("\nDatos procesados guardados con éxito:",
    "\n ->", ruta_salida_export,
    "\n ->", ruta_salida_potencial, "\n")