# =============================================================================
# FUNCIONES PARA LIMPIEZA DE DATOS
# =============================================================================

#' Limpiar nombres de columnas según convenciones del proyecto
#' @param df data.frame con nombres a limpiar
#' @return data.frame con nombres en snake_case
limpiar_nombres <- function(df) {
  nombres_nuevos <- names(df) %>%
    str_to_lower() %>%                    # Todo minúsculas
    str_replace_all("[^a-zA-Z0-9_]", "_") %>%  # Solo letras, números y _
    str_replace_all("_{2,}", "_") %>%     # Eliminar _ múltiples
  
  names(df) <- nombres_nuevos
  return(df)
}

limpiar_prop <- function(df) {
  df_limpio <- df %>%
  mutate(
    across(where(is.character) | where(is.factor), ~ {
      texto_minusculas <- tolower(.)
      texto_sin_tildes <- stringr::str_replace_all(
        texto_minusculas,
        pattern = c(
          "á" = "a",
          "é" = "e",
          "í" = "i",
          "ó" = "o",
          "ú" = "u",
          "ñ" = "n",
          "ü" = "u" 
        )
      )
      return(texto_sin_tildes)
    })
  )

return(df_limpio)
}


#' Validar rangos de variables numéricas
#' @param vector vector numérico a validar
#' @param min_val valor mínimo esperado
#' @param max_val valor máximo esperado
#' @return logical vector indicando valores válidos
validar_rango <- function(vector, min_val = -Inf, max_val = Inf) {
  vector >= min_val & vector <= max_val & !is.na(vector)
}

#' Detectar y reportar datos atípicos usando IQR
#' @param df data.frame
#' @param columna nombre de la columna a analizar
#' @return lista con información sobre atípicos
detectar_atipicos <- function(df, columna) {
  vector_datos <- df[[columna]]
  
  Q1 <- quantile(vector_datos, 0.25, na.rm = TRUE)
  Q3 <- quantile(vector_datos, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  
  atipicos <- vector_datos < limite_inferior | vector_datos > limite_superior
  
  list(
    n_atipicos = sum(atipicos, na.rm = TRUE),
    porcentaje = round(100 * sum(atipicos, na.rm = TRUE) / length(vector_datos), 2),
    limite_inferior = limite_inferior,
    limite_superior = limite_superior,
    indices_atipicos = which(atipicos)
  )
}