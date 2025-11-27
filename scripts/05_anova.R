# =============================================================================
# SCRIPT 05: ANÁLISIS DE VARIANZA (ANOVA) - HIPÓTESIS D
# Propósito: Evaluar si la centralidad difiere significativamente por sección.
# Inputs: Base 2 (df_prod_export_reducido.rds)
# =============================================================================

library(here) 
library(tidyverse)
library(car)      # Para el Test de Levene
source(here::here("config", "global.R"))

alpha_significancia <- 0.05

mensaje_proceso("Iniciando ANOVA de Centralidad por Sección")

# Carga de la base limpia
ruta_base2_in <- file.path(dir_data_clean, "df_prod_export_reducido.rds")
base_export <- readRDS(ruta_base2_in) %>%
  # Asegurarse de que el factor sea tipo factor/nominal
  mutate(seccion_f = as.factor(seccion))

# 1. ESTADÍSTICAS DESCRIPTIVAS POR GRUPO (Paso 4)
# -----------------------------------------------------------------------------
mensaje_proceso("1. Calculando estadísticas descriptivas por Sección")

stats_por_seccion <- base_export %>%
  group_by(seccion_f) %>%
  summarise(
    N = n(),
    Media_Centralidad = mean(centralidad, na.rm = TRUE),
    Mediana_Centralidad = median(centralidad, na.rm = TRUE),
    Desvio_Std = sd(centralidad, na.rm = TRUE),
    Min = min(centralidad, na.rm = TRUE),
    Max = max(centralidad, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n--- ESTADÍSTICAS DE CENTRALIDAD POR SECCIÓN ---\n")
print(stats_por_seccion)

# 2. ELIMINACION / IMPUTACION DE VALORES PARA SECCION "OTROS"
# limpiamos la seccion otros por su nula relevancia estadistica y economica

base_export_anova <- base_export %>%
  filter(seccion != "otros") %>%
  mutate(seccion_f = as.factor(seccion))

stats_por_seccion_anova <- base_export_anova %>%
  group_by(seccion_f) %>%
  summarise(
    N = n(),
    Media_Centralidad = mean(centralidad, na.rm = TRUE),
    Mediana_Centralidad = median(centralidad, na.rm = TRUE),
    Desvio_Std = sd(centralidad, na.rm = TRUE),
    Min = min(centralidad, na.rm = TRUE),
    Max = max(centralidad, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n--- ESTADÍSTICAS DE CENTRALIDAD POR SECCIÓN PARA ANOVA ---\n")
print(stats_por_seccion_anova)


# 3. VERIFICACIÓN DE SUPUESTOS (Paso 5: Rigor Metodológico)
# -----------------------------------------------------------------------------
mensaje_proceso("2. Verificando supuestos de ANOVA")

# 2.1 Supuesto de Homogeneidad de Varianzas (Test de Levene)
# H0: Las varianzas son iguales. Queremos NO rechazar H0.

levene_test_resultado <- leveneTest(centralidad ~ seccion_f, data = base_export_anova)

cat("\n--- TEST DE LEVENE (Homogeneidad de Varianzas) ---\n")
print(levene_test_resultado)

# Defino nivel de significancia (alpha_significancia)
alpha_significancia <- 0.05

p_valor_levene <- levene_test_resultado$`Pr(>F)`[1] 

if (p_valor_levene < alpha_significancia) {
  # Si p-valor < 0.05: Rechazo H0 -> Varianza NO es homogénea
  mensaje_proceso("¡ADVERTENCIA! Se rechazó H0 (varianzas no homogéneas). Considerar ANOVA de Welch o transformación.")
  use_welch <- TRUE
} else {
  # Si p-valor >= 0.05: NO rechazo H0 -> Varianza es homogénea
  mensaje_exito("Supuesto de Homogeneidad de Varianzas cumplido.")
  use_welch <- FALSE
}


# 3. AJUSTE DEL MODELO (ANOVA)
# -----------------------------------------------------------------------------
mensaje_proceso("3. Ajustando modelo de ANOVA")

if (use_welch) {
  # Opción robusta si la homocedasticidad falla
  modelo_anova <- oneway.test(centralidad ~ seccion_f, data = base_export_anova, var.equal = FALSE)
  cat("\n--- ANOVA DE WELCH (Robusto) ---\n")
} else {
  # ANOVA tradicional (aov es útil para Tukey)
  modelo_anova <- aov(centralidad ~ seccion_f, data = base_export_anova)
  cat("\n--- ANOVA DE UN FACTOR ---\n")
  print(summary(modelo_anova))
}

# 4. INTERPRETACIÓN Y POST-HOC (Pensamiento Crítico)
# -----------------------------------------------------------------------------
# Evaluamos el p-valor del estadístico F [16, 24]

# (Si el p-valor global es significativo, proceder con Tukey)
if (use_welch) {
  # Para el test de Welch, se usa el p-valor del oneway.test
  p_value_global <- modelo_anova$p.value
} else {
  # Para aov, se extrae del summary
  p_value_global <- summary(modelo_anova)[[24]]$`Pr(>F)`[24]
}


if (p_value_global < alpha_significancia) {
  print("Rechazamos H0: Existe diferencia significativa en Centralidad por Sección.")
  
  if (!use_welch) {
    mensaje_proceso("Realizando Test Post-Hoc (Tukey HSD) para identificar diferencias específicas")
    # El test de Tukey controla el error Tipo I [16-18]
    tukey_resultado <- TukeyHSD(modelo_anova)
    print(tukey_resultado)
  } else {
    mensaje_proceso("ANOVA de Welch usado; usar Games-Howell post-hoc (requiere librerías adicionales) o T3.")
  }
  
} else {
  mensaje_exito("No se puede rechazar H0: No hay diferencia significativa en Centralidad entre las Secciones.")
}

mensaje_exito("Análisis ANOVA completado.")