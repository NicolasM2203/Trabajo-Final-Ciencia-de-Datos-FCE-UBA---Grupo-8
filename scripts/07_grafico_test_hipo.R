# =============================================================================
# SCRIPT 07: VISUALIZACIÓN Y STORYTELLING
# Proyecto: Complejidad Económica Provincial
# Descripción: Generación de gráficos editorializados para el informe final.
#              1. Ranking de Sectores (Contexto ANOVA)
#              2. Relación Potencialidad-Complejidad-Distancia (Hipótesis C)
# Inputs: df_prod_export_reducido.rds (Base 1) y df_prod_potencial_transformado.rds (Base 2)
# Outputs: Imágenes .png en alta calidad en output/figures/
# =============================================================================

# 0. CONFIGURACIÓN Y TEMA GRÁFICO
# -----------------------------------------------------------------------------
library(here) 
library(tidyverse)
library(ggrepel)   # Para etiquetas inteligentes
library(patchwork) # Para combinar gráficos
library(scales)    # Para formato de ejes

source(here::here("config", "global.R")) 

mensaje_proceso("Iniciando generación de gráficos editorializados...")

# Definimos PALETA DE COLORES (Storytelling visual)
col_principal <- "#2C3E50" # Azul oscuro
col_acento    <- "#E74C3C" # Rojo (Destacados)
col_secundario<- "#95A5A6" # Gris (Contexto)

# TEMA PERSONALIZADO
tema_grupo9 <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "black"),
    plot.subtitle = element_text(size = 12, color = "gray40", margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "gray60", hjust = 0),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

theme_set(tema_grupo9)

# -----------------------------------------------------------------------------
# GRÁFICO 1: RANKING ESTRUCTURAL (RESULTADO DEL ANOVA)
# Objetivo: Mostrar que no todos los sectores son iguales.
# -----------------------------------------------------------------------------

ruta_base1 <- file.path(dir_data_clean, "df_prod_export_reducido.rds")
df_base1 <- readRDS(ruta_base1)

mensaje_proceso("Generando Gráfico 1: Ranking de Sectores...")

# Preparar datos
df_viz_1 <- df_base1 %>%
  filter(seccion != "otros") %>%
  mutate(seccion = fct_reorder(seccion, centralidad, .fun = median)) %>%
  mutate(destacado = ifelse(seccion %in% c("maquinaria", "vehiculos", "electronica"), "Complejo", "Tradicional"))

# Crear Gráfico
p1 <- ggplot(df_viz_1, aes(x = seccion, y = centralidad)) +
  geom_violin(aes(fill = destacado), alpha = 0.2, color = NA) +
  geom_boxplot(width = 0.2, aes(color = destacado), alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c("Complejo" = col_acento, "Tradicional" = col_secundario)) +
  scale_color_manual(values = c("Complejo" = col_acento, "Tradicional" = col_principal)) +
  coord_flip() +
  labs(
    title = "Dime qué produces y te diré qué tan conectado estás",
    subtitle = "Distribución de la Centralidad del Producto según Sector Económico.\nLos sectores de alta complejidad técnica dominan el centro del espacio.",
    x = "", 
    y = "Índice de Centralidad (Mayor es mejor)",
    caption = "Fuente: Elaboración propia en base a datos de exportaciones.",
    color = "Tipo de Sector", fill = "Tipo de Sector"
  )

ggsave(file.path(dir_outputs_figures, "G1_ranking_sectores_anova.png"), p1, width = 12, height = 7, bg = "white")


# -----------------------------------------------------------------------------
# GRÁFICO 2: POTENCIALIDAD vs COMPLEJIDAD y DISTANCIA (HIPÓTESIS C)
# Objetivo: Scatter Plot que muestre el hallazgo de la regresión.
# -----------------------------------------------------------------------------

ruta_base2 <- file.path(dir_data_transformed, "df_prod_potencial_transformado.rds")
df_base2 <- readRDS(ruta_base2)

mensaje_proceso("Generando Gráfico 2: Scatter Plot Multidimensional...")

# 1. Preparación de Datos
df_viz_2 <- df_base2 %>%
  mutate(
    distancia_cat = ntile(distancia, 3), 
    distancia_factor = case_when(
      distancia_cat == 1 ~ "1. Baja Distancia (Cercanos)",
      distancia_cat == 2 ~ "2. Distancia Media",
      distancia_cat == 3 ~ "3. Alta Distancia (Lejanos)"
    )
  )

set.seed(999)
df_fondo_2 <- df_viz_2 %>% sample_n(10000) 

# --- SELECCIÓN MANUAL DE PUNTOS POR NOMBRE EXACTO ---
# Definimos la lista exacta de nombres que quieres ver
nombres_objetivo <- c(
  "carne bovina, deshuesada, congelada",
  "aceite soja en bruto, desgomado",
  "manteca,grasa y aceite de cacao",
  "alcohol isobutilico",
  "guitarras y contrabajos electricos",
  "azucares, eteres, acetales y sales",
  "pilas de litio,c/volumen exterior <= a 300cm3"
)

# Filtramos buscando coincidencia exacta (usando tolower para evitar problemas de mayúsculas)
df_etiquetas_2 <- df_viz_2 %>%
  mutate(nombre_min = tolower(ncm_6d)) %>% # Normalizamos a minúsculas
  filter(nombre_min %in% nombres_objetivo) %>%
  # Nos aseguramos de que no haya duplicados (si hay 2 aceites iguales, tomamos el primero)
  group_by(nombre_min) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    # Creamos la etiqueta final truncada y prolija
    etiqueta_producto = str_trunc(ncm_6d, 30) 
  )

# Verificación en consola: Debe imprimir exactamente 7 productos o menos
cat("\nProductos encontrados y listos para etiquetar:\n")
print(df_etiquetas_2$etiqueta_producto)


# Crear Gráfico
p2 <- ggplot(df_fondo_2, aes(x = complejidad_producto_win, y = potencialidad_win)) +
  
  # A. Puntos de fondo (Scatter)
  geom_point(aes(color = distancia_factor), alpha = 0.3, size = 1.5) +
  
  # B. Líneas de Tendencia por Grupo (MCO)
  geom_smooth(aes(color = distancia_factor, fill = distancia_factor), 
              method = "lm", alpha = 0.15, linewidth = 1) +
  
  # C. Etiquetas Inteligentes (Solo los 7 puntos seleccionados)
  geom_point(data = df_etiquetas_2, aes(color = distancia_factor), 
             size = 4, shape = 21, fill = "white", stroke = 1.5) + # Círculo blanco para resaltar
  
  geom_label_repel(data = df_etiquetas_2, 
                   aes(label = etiqueta_producto, color = distancia_factor), 
                   size = 3,                 
                   fontface = "bold",        
                   box.padding = 0.8,        # Más espacio para que respire
                   point.padding = 0.5,      
                   force = 20,               # Fuerza de repulsión alta para separarlas bien
                   show.legend = FALSE,
                   max.overlaps = Inf) +
  
  # D. Escalas y Colores
  scale_color_manual(values = c(
    "1. Baja Distancia (Cercanos)" = "#2ECC71", 
    "2. Distancia Media" = "#F1C40F",           
    "3. Alta Distancia (Lejanos)" = "#E74C3C"   
  )) +
  scale_fill_manual(values = c(
    "1. Baja Distancia (Cercanos)" = "#2ECC71",
    "2. Distancia Media" = "#F1C40F",
    "3. Alta Distancia (Lejanos)" = "#E74C3C"
  )) +
  
  # E. Textos Editorializados
  labs(
    title = "Complejidad y Distancia: Motores de la Diversificación Productiva",
    subtitle = "Relación entre el valor de un producto (Complejidad) y la oportunidad que genera (Potencialidad),\nsegmentada por la dificultad para alcanzarlo (Distancia).",
    x = "Índice de Complejidad de Producto (Winsorizado)",
    y = "Índice de Potencialidad (Winsorizado)",
    color = "Distancia a la Capacidad Actual",
    fill = "Distancia a la Capacidad Actual",
    caption = "Nota: La pendiente positiva confirma que la complejidad impulsa el potencial. La separación de las líneas\nconfirma el hallazgo de la regresión: los productos 'Lejanos' tienen mayor potencial para el mismo nivel de complejidad."
  ) +
  
  theme(legend.position = "bottom")

ggsave(file.path(dir_outputs_figures, "G2_scatter_potencialidad_complejidad.png"), p2, width = 12, height = 8, bg = "white")

mensaje_exito("Gráficos generados exitosamente en output/figures/")
mensaje_exito("¡Listos para pegar en las diapositivas!")