# =============================================================================
# SCRIPT 04: PROCESAMIENTO AVANZADO Y TRANSFORMACIONES
# Propósito: Aplicar transformaciones para mitigar asimetría y el efecto de outliers
#            en la Base de Potencial Productivo.
# Inputs: df_prod_potencial_limpio.rds (Base de Potencial limpia)
# Outputs: df_prod_potencial_transformado.rds (Base de Potencial transformada)
# =============================================================================

# 0. CONFIGURACIÓN INICIAL Y DEPENDENCIAS
# -----------------------------------------------------------------------------
library(here) 
source(here::here("config", "global.R")) # Carga librerías, rutas y logging

mensaje_proceso("Iniciando transformaciones de variables continuas en Base de Potencial")

# Definimos rutas de carga y guardado (Autocontención)
ruta_df_potencial_in <- file.path(dir_data_clean, "df_prod_potencial_limpio.rds")
ruta_df_potencial_out <- file.path(dir_data_transformed, "df_prod_potencial_transformado.rds")

# Cargamos la base limpia
base_potencial <- readRDS(ruta_df_potencial_in)

# -----------------------------------------------------------------------------
# 1. TRANSFORMACIONES DE TIPO (Factorización)
# -----------------------------------------------------------------------------

# Convertir variables clave a tipo factor si aún no lo están (para ANOVA/Regresión)
base_potencial_transformado <- base_potencial %>%
  mutate(
    provincia = as.factor(provincia),
    seccion = as.factor(seccion)
    # tiene_vcr podría ser factor o binaria (0/1) dependiendo del uso
    # tiene_vcr = as.factor(tiene_vcr) 
  )

# -----------------------------------------------------------------------------
# 2. TRANSFORMACIÓN LOGARÍTMICA (FOB Mundial)
# -----------------------------------------------------------------------------
# Aplicamos logaritmo natural al FOB mundial para reducir asimetría y heteroscedasticidad.

mensaje_proceso("Aplicando transformación logarítmica a fob_mundial...")

base_potencial_transformado <- base_potencial_transformado %>%
  mutate(
    # Aplicamos logaritmo natural (LN)
    log_fob_mundial = log(fob_mundial)
  )

# -----------------------------------------------------------------------------
# 3. WINSORIZACIÓN (Tratamiento Robusto de Outliers)
# -----------------------------------------------------------------------------

# Winsorizamos las variables clave (Potencialidad, ICP) para reducir el impacto de 
# los outliers detectados en las colas, especialmente la asimetría.
# Usamos un umbral conservador (1% y 99%).

mensaje_proceso("Calculando límites P1 y P99 y aplicando Winsorización manual...")

# Paso A: Calcular los límites (P1 y P99) FUERA del mutate.
limites_potencialidad <- quantile(base_potencial_transformado$potencialidad, c(0.01, 0.99), na.rm = TRUE)
limites_complejidad <- quantile(base_potencial_transformado$complejidad_producto, c(0.01, 0.99), na.rm = TRUE)

# Paso B: Aplicar Winsorize MANUALMENTE usando pmin/pmax.
# Esto es más robusto que depender de librerías externas que pueden fallar por versiones.
# Lógica: Si el valor es menor al P1, se reemplaza por P1 (pmax).
#         Si el valor es mayor al P99, se reemplaza por P99 (pmin).

base_potencial_transformado <- base_potencial_transformado %>%
  mutate(
    # Winsorizar Potencialidad
    potencialidad_win = pmin(pmax(potencialidad, limites_potencialidad[1]), limites_potencialidad[2]),
    
    # Winsorizar Complejidad de Producto
    complejidad_producto_win = pmin(pmax(complejidad_producto, limites_complejidad[1]), limites_complejidad[2])
  )

# -----------------------------------------------------------------------------
# 4. VERIFICACIÓN DESCRIPTIVA POST-TRANSFORMACIÓN 
#    (Paso 6: Evaluación de Impacto)
# -----------------------------------------------------------------------------

mensaje_proceso("Evaluando impacto de las transformaciones...")

cat("\n--- DESCRIPTIVAS COMPARATIVAS POST-TRANSFORMACIÓN (Base Potencial) ---\n")

base_potencial_transformado %>%
  select(fob_mundial, log_fob_mundial, potencialidad, potencialidad_win, 
         complejidad_producto, complejidad_producto_win) %>%
  summarise(across(everything(), list(media = ~mean(., na.rm = TRUE), 
                                      mediana = ~median(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE)))) %>%
  # Imprime la tabla comparativa para verificar la reducción en la desviación estándar
  print()

# -----------------------------------------------------------------------------
# 5. EXPORTAR RESULTADOS TRANSFORMADOS
# -----------------------------------------------------------------------------

mensaje_proceso(paste("Guardando base Potencial transformada en:", ruta_df_potencial_out))
saveRDS(base_potencial_transformado, ruta_df_potencial_out)

mensaje_exito("Base Potencial transformada guardada y lista para modelado inferencial.")