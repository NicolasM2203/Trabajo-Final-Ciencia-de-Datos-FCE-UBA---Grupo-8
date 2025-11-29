# =============================================================================
# SCRIPT 05: ANÁLISIS DE VARIANZA (ANOVA)
# Propósito: Evaluar si la centralidad difiere significativamente por sección.
# Inputs: Base Export (df_prod_export_reducido.rds)
# =============================================================================

library(here) 
library(tidyverse)
library(car)      # Para el Test de Levene
library(rstatix)
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

print(p_valor_levene)

if (p_valor_levene < alpha_significancia) {
  # Si p-valor < 0.05: Rechazo H0 -> Varianza NO es homogénea
  use_welch <- TRUE
  mensaje_proceso("¡ADVERTENCIA! Se rechazó H0 (varianzas no homogéneas). Considerar ANOVA de Welch o transformación.")
} else {
  # Si p-valor >= 0.05: NO rechazo H0 -> Varianza es homogénea
  mensaje_exito("Supuesto de Homogeneidad de Varianzas cumplido.")
  use_welch <- FALSE
}

print(use_welch)

# 3. AJUSTE DEL MODELO (ANOVA)
# -----------------------------------------------------------------------------
print("3. Ajustando modelo de ANOVA")

if (use_welch) {
  # oneway.test() implementa el Welch ANOVA, la Alternativa robusta a heterogeneidad [2]
  modelo_anova <- oneway.test(centralidad ~ seccion_f, 
                              data = base_export_anova, 
                              var.equal = FALSE)
  
  cat("\n--- ANOVA DE WELCH (Robusto ante heterogeneidad) ---\n")
  print(modelo_anova) 
  
} else {
  # ANOVA tradicional (si los supuestos se cumplen)
  
  # aov() es la implementación estándar y es útil para el Test de Tukey posterior [4]
  modelo_anova <- aov(centralidad ~ seccion_f, data = base_export_anova)
  
  cat("\n--- ANOVA DE UN FACTOR ---\n")
  # El summary descompone la varianza [5] y proporciona el p-valor global [6]
  print(summary(modelo_anova)) 
}

print(modelo_anova$p.value)

# 4. INTERPRETACIÓN Y POST-HOC (Controlando el Error Tipo I)
# -----------------------------------------------------------------------------

# El ANOVA es un test omnibus: evalúa si H0: Todas las medias son iguales

# --- Extracción del p-valor global ---
if (use_welch) {
  # Para el test de Welch, el p-valor se extrae directamente del objeto htest
  p_value_global <- modelo_anova$p.value
} else {
  # Para aov(), el p-valor se extrae del summary (ejemplo adaptado de las fuentes)
  p_value_global <- summary(modelo_anova)[[7]]$`Pr(>F)`[7] 
}

print(p_value_global)


if (p_value_global < alpha_significancia) {
  print("Rechazamos H0: Existe diferencia significativa en Centralidad por Sector.")
  
  # Si el ANOVA global es significativo, debemos hacer post-hoc
  if (use_welch) { 
    # La condición es TRUE, ejecutamos Games-Howell
    
    print("Realizando Test Post-Hoc de Games-Howell (apropiado para ANOVA de Welch).")
    
    # 1. Ejecutar el Test de Games-Howell
    games_howell_resultado <- base_export_anova %>% 
      # Reemplaza 'Centralidad' con tu variable métrica real
      # Reemplaza 'Sector' con tu variable categórica real
      games_howell_test(centralidad ~ seccion_f)
    
    # 2. Imprimir los resultados (muestra los pares con la diferencia y el p-valor ajustado)
    print(games_howell_resultado)
    
  } else {
    # Si use_welch es FALSE, volvemos a Tukey por precaución
    print("Realizando Test Post-Hoc (Tukey HSD) para identificar diferencias específicas")
    tukey_resultado <- TukeyHSD(modelo_anova)
    print(tukey_resultado)
  }
  
} else {
  print("No se puede rechazar H0: No hay diferencia significativa en Centralidad entre los Sectores.")
}

# Gráfico Games_Howell

# 1. Preparación de los datos (Solo seleccionamos las columnas clave)
datos_heatmap <- games_howell_resultado %>%
  select(group1, group2, p.adj.signif) # Aseguramos tener solo las columnas necesarias

# 2. Crear el Mapa de Calor
plot_gh <- ggplot(datos_heatmap, aes(x = group1, y = group2)) +
  
  # A. Crear los recuadros del mapa de calor
  geom_tile(
    aes(fill = p.adj.signif), # Usa el código de significancia para el color de fondo
    color = "black"           # Borde de los recuadros
  ) +
  
  # B. Añadir el código de significancia como texto dentro de la celda
  geom_text(
    aes(label = p.adj.signif), # Muestra el código (ej. ***, ns)
    color = "black", 
    size = 4
  ) +
  
  # C. Configurar el color de la leyenda para que sea intuitiva
  scale_fill_manual(
    values = c(
      "ns" = "#E0E0E0",     # Gris claro para 'No Significativo'
      "*" = "#FFC0CB",      # Rosa claro para significativo al 0.05
      "**" = "#FF69B4",     # Rosado intermedio
      "***" = "#FF1493",    # Rosa intenso para muy significativo
      "****" = "#800080"    # Morado para extrema significancia 
    ),
    drop = FALSE, 
    name = "Significancia (p.adj)"
  ) +
  
  # D. Etiquetas y Títulos
  labs(
    title = "Matriz de Diferencias Significativas (Games-Howell)",
    x = "Sector",
    y = "Sector"
  ) +
  
  # E. Tema para mejorar la estética
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(), # Quita las líneas de fondo
    panel.border = element_blank()
  )

ggsave(file.path(dir_outputs_figures, "05_anova_games_howell.png"), plot = plot_gh, width = 10, height = 5)

mensaje_exito("Matriz de Games-Howell guardada.")

print("Análisis ANOVA completado.")



