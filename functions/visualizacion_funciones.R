# =============================================================================
# FUNCIONES PARA VISUALIZACIÓN
# =============================================================================

#' Tema estándar para gráficos del proyecto
tema_proyecto <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

#' Paleta de colores estándar del proyecto
colores_proyecto <- c(
  primario = "#2E86C1",
  secundario = "#E74C3C", 
  terciario = "#F39C12",
  gris_claro = "#BDC3C7",
  gris_oscuro = "#34495E"
)

#' Guardar gráfico con formato estándar
#' @param plot objeto ggplot
#' @param nombre_archivo nombre sin extensión
#' @param ancho ancho en pulgadas
#' @param alto alto en pulgadas
guardar_grafico <- function(plot, nombre_archivo, ancho = 10, alto = 6) {
  
  # Crear nombre con fecha
  fecha_actual <- format(Sys.Date(), "%Y%m%d")
  nombre_completo <- paste0(fecha_actual, "_", nombre_archivo)
  
  # Guardar en PNG (alta calidad)
  ggsave(
    filename = file.path(dir_outputs_figures, paste0(nombre_completo, ".png")),
    plot = plot,
    width = ancho, height = alto,
    dpi = 300, bg = "white"
  )
  
  # También en PDF (vectorial)
  ggsave(
    filename = file.path(dir_outputs_figures, paste0(nombre_completo, ".pdf")),
    plot = plot,
    width = ancho, height = alto
  )
  
  mensaje_exito(paste("Gráfico guardado:", nombre_completo))
}
