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
# 1. Cargar Datos Transformados (Base 2)
# -----------------------------------------------------------------------------
ruta_base2 <- file.path(dir_data_transformed, "df_prod_potencial_transformado.rds")
df_base2 <- readRDS(ruta_base2)

# 2. Preparación de Datos para el Gráfico
# -----------------------------------------------------------------------------
df_viz_2 <- df_base2 %>%
  mutate(
    # Creamos los terciles de distancia para colorear
    distancia_cat = ntile(distancia, 3), 
    distancia_factor = case_when(
      distancia_cat == 1 ~ "1. Baja Distancia (Cercanos)",
      distancia_cat == 2 ~ "2. Distancia Media",
      distancia_cat == 3 ~ "3. Alta Distancia (Lejanos)"
    ),
    # Normalizamos nombres para facilitar la búsqueda
    nombre_min = tolower(ncm_6d)
  )

# Fondo del gráfico (Muestra aleatoria para no saturar)
set.seed(999)
df_fondo_2 <- df_viz_2 %>% sample_n(10000) 

# 3. SELECCIÓN INTELIGENTE DE ETIQUETAS (CON COLOR CORRECTO)
# -----------------------------------------------------------------------------

# Definimos las listas de productos por el color/distancia que QUEREMOS mostrar
# (Esto asegura que la narrativa visual sea coherente)

target_baja_dist <- c(
  "carne bovina, deshuesada, congelada",
  "alcohol isobutilico",
  "azucares, eteres, acetales y sales"
)

target_media_dist <- c(
  "aceite soja en bruto, desgomado"
)

target_alta_dist <- c(
  "manteca,grasa y aceite de cacao",
  "guitarras y contrabajos electricos",
  "pilas de litio,c/volumen exterior <= a 300cm3"
)

# Unimos todo en una sola lista para filtrar primero
todos_los_targets <- c(target_baja_dist, target_media_dist, target_alta_dist)

df_etiquetas_2 <- df_viz_2 %>%
  # 1. Filtramos solo los productos de interés
  filter(nombre_min %in% todos_los_targets) %>%
  
  # 2. Agrupamos por producto para elegir LA MEJOR provincia para cada uno
  group_by(nombre_min) %>%
  
  # 3. ORDENAMIENTO INTELIGENTE (Aquí ocurre la magia de los colores)
  arrange(case_when(
    # Si lo queremos ROJO (Alta), ordenamos de mayor a menor distancia
    nombre_min %in% target_alta_dist ~ desc(distancia),
    
    # Si lo queremos AMARILLO (Media), priorizamos los del tercil 2
    nombre_min %in% target_media_dist ~ abs(distancia_cat - 2), 
    
    # Si lo queremos VERDE (Baja), ordenamos de menor a mayor distancia
    TRUE ~ distancia 
  )) %>%
  
  # 4. Nos quedamos con el mejor candidato de cada producto
  slice(1) %>%
  ungroup() %>%
  
  # 5. Creamos la etiqueta limpia
  mutate(etiqueta_producto = str_trunc(ncm_6d, 35))

# Verificación en consola
cat("\nProductos seleccionados y su categoría asignada:\n")
print(df_etiquetas_2 %>% select(etiqueta_producto, distancia_factor))


# 4. CREACIÓN DEL GRÁFICO
# -----------------------------------------------------------------------------

# Definición de Colores (Semáforo)
col_verde    <- "#2ECC71"
col_amarillo <- "#F1C40F"
col_rojo     <- "#E74C3C"

p2 <- ggplot(df_fondo_2, aes(x = complejidad_producto_win, y = potencialidad_win)) +
  
  # A. Nube de puntos de fondo
  geom_point(aes(color = distancia_factor), alpha = 0.2, size = 1.5) +
  
  # B. Líneas de Tendencia
  geom_smooth(aes(color = distancia_factor, fill = distancia_factor), 
              method = "lm", alpha = 0.1, linewidth = 0.8) +
  
  # C. Puntos Destacados (Círculo blanco + Borde de color)
  geom_point(data = df_etiquetas_2, aes(color = distancia_factor), 
             size = 4, shape = 21, fill = "white", stroke = 2) +
  
  # D. Etiquetas (Texto)
  geom_label_repel(data = df_etiquetas_2, 
                   aes(label = etiqueta_producto, color = distancia_factor),
                   size = 3.5, fontface = "bold",
                   box.padding = 0.8, point.padding = 0.5,
                   force = 20, show.legend = FALSE,
                   max.overlaps = Inf) +
  
  # E. Escalas
  scale_color_manual(values = c(
    "1. Baja Distancia (Cercanos)" = col_verde,
    "2. Distancia Media" = col_amarillo,
    "3. Alta Distancia (Lejanos)" = col_rojo
  )) +
  scale_fill_manual(values = c(
    "1. Baja Distancia (Cercanos)" = col_verde,
    "2. Distancia Media" = col_amarillo,
    "3. Alta Distancia (Lejanos)" = col_rojo
  )) +
  
  # F. Textos
  labs(
    title = "Complejidad y Distancia: Motores de la Diversificación Productiva",
    subtitle = "Relación entre el valor de un producto (Complejidad) y la oportunidad que genera (Potencialidad),\nsegmentada por la dificultad para alcanzarlo (Distancia).",
    x = "Índice de Complejidad de Producto (Winsorizado)",
    y = "Índice de Potencialidad (Winsorizado)",
    color = "Distancia a la Capacidad Actual",
    fill = "Distancia a la Capacidad Actual",
    caption = "Nota: La pendiente positiva confirma que la complejidad impulsa el potencial. La separación de las líneas\nconfirma el hallazgo de la regresión: los productos 'Lejanos' tienen mayor potencial para el mismo nivel de complejidad."
  ) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# 5. GUARDADO
# -----------------------------------------------------------------------------
ruta_figura <- file.path(dir_outputs_figures, "G2_scatter_potencialidad_final_v2.png")
ggsave(ruta_figura, p2, width = 12, height = 8, bg = "white")

mensaje_exito(paste("Gráfico final generado y guardado en:", ruta_figura))