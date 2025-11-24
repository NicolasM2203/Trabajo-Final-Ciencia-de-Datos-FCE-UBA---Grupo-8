# =============================================================================
# SCRIPT 03: ANÁLISIS EXPLORATORIO DE DATOS (EDA) Y DIAGNÓSTICO
# Proyecto: Complejidad Económica Provincial
# Descripción: EDA exhaustivo, detección de tipos, faltantes y outliers.
# Inputs: df_prod_export_limpio.rds (Base 1) y df_prod_potencial_limpio.rds (Base 2) 
#         (archivos limpios provenientes de 02_limpieza.R)
# Outputs: Tablas descriptivas y gráficos de distribución en /outputs/
# =============================================================================

# 0. CONFIGURACIÓN INICIAL Y CARGA DE DEPENDENCIAS
# -----------------------------------------------------------------------------
library(here) 
source(here::here("config", "global.R")) # Carga de parámetros y logging [9, 10]
library(naniar)    # Para la visualización de datos faltantes [6, 7]
library(patchwork) # Para combinar gráficos [6-8]


mensaje_proceso("Iniciando análisis exploratorio de datos (EDA)")

# Definimos las rutas de carga 
ruta_carga_export <- file.path(dir_data_clean, "df_prod_export_limpio.rds")
ruta_carga_potencial <- file.path(dir_data_clean, "df_prod_potencial_limpio.rds")

# Cargar los datos limpios y procesados (Autocontención) [11, 12]
df_prod_export_limpio <- readRDS(ruta_carga_export) 
df_prod_potencial_limpio <- readRDS(ruta_carga_potencial) 

mensaje_exito("Bases de datos limpias cargadas correctamente.")

# -----------------------------------------------------------------------------
# 1. ESTRUCTURA GENERAL, DIMENSIONES Y TIPOS DE DATOS (CONSIGNA 3)
# -----------------------------------------------------------------------------

mensaje_proceso("1. Verificando estructura general, dimensiones y tipos de datos")

# 1.1 Base Potencial (Potencialidad - Hipótesis C)
cat("\n--- ESTRUCTURA BASE (Potencial) ---\n")
cat("Dimensiones:", nrow(df_prod_export_limpio), "filas x", ncol(df_prod_export_limpio), "columnas\n") # Estructura general [13]
glimpse(df_prod_export_limpio) # Identificación de columnas y tipos de datos [13, 14]

# 1.2 Base Export (Centralidad - Hipótesis D)
cat("\n--- ESTRUCTURA BASE (Export) ---\n")
cat("Dimensiones:", nrow(df_prod_potencial_limpio), "filas x", ncol(df_prod_potencial_limpio), "columnas\n")
glimpse(df_prod_potencial_limpio) 

# Nota: Se verifica que las variables clave sean numéricas (potencialidad, centralidad, complejidad_producto, distancia) 
# y que 'seccion' sea categórica (factor/character) para el ANOVA.

# -----------------------------------------------------------------------------
# 2. DATOS FALTANTES (NAs) (CONSIGNA 3 y 5)
# -----------------------------------------------------------------------------

mensaje_proceso("2. Cuantificación y patrón de datos faltantes (NAs)")

# Cuantificación de NAs por columna [15]
reporte_na_base_export <- df_prod_export_limpio %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "N_NA_{.col}")) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "N_NA") %>%
  mutate(P_NA = round(N_NA / nrow(df_prod_export_limpio) * 100, 2)) %>%
  filter(N_NA > 0)

cat("\n--- REPORTE DE VALORES FALTANTES (BASE EXPORT) ---\n")
if (nrow(reporte_na_base_export) > 0) {
  print(reporte_na_base_export)
  # Visualización del patrón de faltantes (si existen NAs)
  p_vis_na_1 <- vis_miss(df_prod_export_limpio) + 
    labs(title = "Patrón de Datos Faltantes - Base Export")
  ggsave(file.path(dir_outputs_figures, "eda_na_base_export.png"), plot = p_vis_na_1, width = 8, height = 6)
  mensaje_proceso("Gráfico de patrón de faltantes Base Export guardado.")
} else {
  mensaje_exito("Base Export no contiene valores faltantes.")
}

# (Repetir para Base 2)
reporte_na_base_potencial <- df_prod_potencial_limpio %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "N_NA_{.col}")) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "N_NA") %>%
  mutate(P_NA = round(N_NA / nrow(df_prod_potencial_limpio) * 100, 2)) %>%
  filter(N_NA > 0)

cat("\n--- REPORTE DE VALORES FALTANTES (BASE POTENCIAL) ---\n")
if (nrow(reporte_na_base_potencial) > 0) {
  print(reporte_na_base_potencial)
  # Visualización del patrón de faltantes (si existen NAs)
  p_vis_na_2 <- vis_miss(df_prod_potencial_limpio) + 
    labs(title = "Patrón de Datos Faltantes - Base Potencial")
  ggsave(file.path(dir_outputs_figures, "eda_na_base_potencial.png"), plot = p_vis_na_2, width = 8, height = 6)
  mensaje_proceso("Gráfico de patrón de faltantes Base Potencial guardado.")
} else {
  mensaje_exito("Base Potencial no contiene valores faltantes.")
}

# -----------------------------------------------------------------------------
# 3. ESTADÍSTICAS DESCRIPTIVAS Y OUTLIERS (CONSIGNA 4 y 5)
# -----------------------------------------------------------------------------

mensaje_proceso("3. Cálculo de descriptivas (media, mediana, desvío, IQR) y detección de outliers")

# Utilizaremos una función para calcular descriptivas completas (simulando una función propia o utilizando `summarise` avanzado) [16, 17]
calcular_descriptivas <- function(df, variables) {
  df %>%
    select(all_of(variables)) %>%
    summarise(
      n = n(),
      across(everything(), 
             list(Media = ~ mean(., na.rm = TRUE),
                  Mediana = ~ median(., na.rm = TRUE),
                  Desvio_Std = ~ sd(., na.rm = TRUE),
                  IQR = ~ IQR(., na.rm = TRUE),
                  Min = ~ min(., na.rm = TRUE),
                  Max = ~ max(., na.rm = TRUE)),
             .names = "{.col}_{.fn}")
    ) %>%
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "Estadistica") %>%
    rename(Valor = V1)
}

# 3.1 Base 1: Variables Continuas (Potencialidad y Distancia para Hipótesis C)
vars_base1_cont <- c("potencialidad", "complejidad_producto", "distancia", "fob_mundial")
descriptivas_base1 <- calcular_descriptivas(df_export_limpio, vars_base1_cont)

cat("\n--- DESCRIPTIVAS CLAVE (BASE 1) ---\n")
print(descriptivas_base1, n = Inf)

# NOTA CRÍTICA: Observar Media vs Mediana. Diferencias grandes indican asimetría (outliers) [18]
# Si Media > Mediana, se necesitará considerar la transformación logarítmica [19-21].

# 3.2 Base 2: Variables Continuas (Centralidad para Hipótesis D)
vars_base2_cont <- c("centralidad", "complejidad_producto", "complejidad_provincia")
descriptivas_base2 <- calcular_descriptivas(df_potencial_limpio, vars_base2_cont)

cat("\n--- DESCRIPTIVAS CLAVE (BASE 2) ---\n")
print(descriptivas_base2, n = Inf)

# -----------------------------------------------------------------------------
# 4. VISUALIZACIÓN DE DISTRIBUCIONES (CONSIGNA 4 y 5)
# -----------------------------------------------------------------------------

mensaje_proceso("4. Generando visualizaciones clave para diagnóstico de modelado")

# 4.1 Base 1: Distribución y Outliers de Potencialidad (Variable Dependiente Híp. C)
# Histograma (para ver forma/asimetría) y Boxplot (para ver outliers IQR) [22-24]

# Histograma de Potencialidad
p_pot_hist <- ggplot(df_export_limpio, aes(x = potencialidad)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(linewidth = 1, color = "red") +
  labs(title = "Distribución de Potencialidad",
       subtitle = "Diagnóstico de asimetría para Regresión Híp. C",
       x = "Potencialidad", y = "Densidad") +
  theme_minimal(base_size = 12) 

# Boxplot de Potencialidad (Detección formal de outliers) [23, 25]
p_pot_box <- ggplot(df_export_limpio, aes(y = potencialidad)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.5) +
  labs(title = "Boxplot de Potencialidad", y = "Potencialidad") +
  theme_minimal(base_size = 12) +
  theme(axis.title.x = element_blank())

# Combinar y guardar gráficos (Calidad expositiva) [8]
p_pot_final <- p_pot_hist + p_pot_box + patchwork::plot_layout(widths = c(3, 1))
ggsave(file.path(dir_outputs_figures, "03_distribucion_potencialidad.png"), plot = p_pot_final, width = 10, height = 5)

mensaje_exito("Gráfico de Potencialidad (Híp. C) guardado.")


# 4.2 Base 2: Boxplots de Centralidad por Sección (Visualización para ANOVA Híp. D)
# Se comprueba visualmente si la centralidad media difiere entre sectores [26, 27]
p_centralidad_seccion <- df_potencial_limpio %>%
  # Convertir seccion a factor si no lo está
  mutate(seccion_f = as.factor(seccion)) %>%
  ggplot(aes(x = seccion_f, y = centralidad, fill = seccion_f)) +
  # Usar geom_boxplot o geom_violin [28, 29]
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Ocultar outliers para usar jitter
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") + # Mostrar la media [30]
  labs(title = "Centralidad del Producto según Sección Económica",
       subtitle = "Comparación de medianas y medias (rombo rojo) para ANOVA",
       x = "Sección Económica", y = "Centralidad del Producto") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(file.path(dir_outputs_figures, "03_boxplot_centralidad_seccion.png"), plot = p_centralidad_seccion, width = 12, height = 6)
mensaje_exito("Gráfico Boxplot/Jitter para ANOVA (Híp. D) guardado.")


# -----------------------------------------------------------------------------
# 5. PRIMERAS OBSERVACIONES Y ANOMALÍAS (CONSIGNA 3)
# -----------------------------------------------------------------------------

mensaje_proceso("5. Primeras y últimas observaciones (Head/Tail)")

# Se observan los primeros 10 registros para buscar errores o patrones iniciales
cat("\n--- PRIMERAS 10 OBSERVACIONES BASE 1 ---\n")
print(head(df_export_limpio, 10))

cat("\n--- ÚLTIMAS 10 OBSERVACIONES BASE 2 ---\n")
print(tail(df_potencial_limpio, 10))

# -----------------------------------------------------------------------------
# 6. DOCUMENTACIÓN DE HALLAZGOS Y PASO FINAL
# -----------------------------------------------------------------------------

mensaje_exito("EDA exhaustivo completado. Los resultados descriptivos y visuales guiarán las decisiones sobre transformaciones (log/cuadrática) y la verificación de supuestos para la inferencia.")

# Al final del script, se recomienda guardar una tabla de las descriptivas
# importantes en /output/tables/ para el informe final
write_csv(descriptivas_base1, file.path(dir_outputs_tables, "03_descriptivas_base1.csv"))
write_csv(descriptivas_base2, file.path(dir_outputs_tables, "03_descriptivas_base2.csv"))