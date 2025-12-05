# =============================================================================
# SCRIPT 06: ANÁLISIS DE REGRESIÓN MÚLTIPLE (HIPÓTESIS C)
# Descripción: Test de la Hipótesis C sobre la Base de Potencial Productivo.
#              Modelo: Potencialidad ~ Complejidad + Distancia + Controles
# Inputs: df_prod_potencial_transformado.rds (Desde carpeta 'transformed')
# Outputs: Resultados de regresión robustos en 'output/tables'
# =============================================================================

# 0. CONFIGURACIÓN INICIAL Y DEPENDENCIAS
# -----------------------------------------------------------------------------
library(here) 
library(tidyverse)
library(lmtest)    # Test de Breusch-Pagan (Heterocedasticidad)
library(car)       # VIF (Multicolinealidad)
library(sandwich)  # Errores estándar robustos (HC1)

source(here::here("config", "global.R")) 

# Definimos constante de significancia
ALPHA_SIGNIFICANCIA <- 0.05
ALPHA <- 0.05

mensaje_proceso("Iniciando análisis de Regresión Múltiple (Hipótesis C)...")




# 1. CARGA DE DATOS 

ruta_base_potencial <- file.path(dir_data_transformed, "df_prod_potencial_transformado.rds")
df_pot_transf <- readRDS(ruta_base_potencial)

mensaje_exito("Base de Potencial cargada correctamente desde 'transformed'.")




# 2. ESPECIFICACIÓN Y ESTIMACIÓN DEL MODELO MCO

# HIPÓTESIS C:
# Y = Potencialidad (Winsorizada)
# X1 = Complejidad Producto (Winsorizada)
# X2 = Distancia
# Control 1 = Tamaño de Mercado (Log FOB Mundial)
# Control 2 = Sector (Sección)

# Definimos la fórmula
formula_hipotesis_c <- potencialidad_win ~ complejidad_producto_win + distancia + log_fob_mundial + seccion

mensaje_proceso("Estimando modelo por Mínimos Cuadrados Ordinarios (MCO)...")

modelo_c <- lm(formula = formula_hipotesis_c, data = df_pot_transf)

# Imprimimos resumen preliminar (con errores estándar clásicos)
cat("\n--- RESUMEN PRELIMINAR (MCO Clásico) ---\n")
print(summary(modelo_c))






# 3. DIAGNÓSTICO DE SUPUESTOS 


mensaje_proceso("Ejecutando diagnósticos del modelo...")

# 3.1 Multicolinealidad (VIF)
# ---------------------------
cat("\n--- DIAGNÓSTICO VIF (Factor de Inflación de Varianza) ---\n")
# Usamos try() por seguridad si hay colinealidad perfecta con las dummies de sección
try({
  vif_res <- vif(modelo_c)
  
  # Ajuste para mostrar GVIF si hay variables categóricas
  if("GVIF" %in% colnames(as.data.frame(vif_res))) {
    print(vif_res[, "GVIF"]) # Mostramos solo la columna relevante
  } else {
    print(vif_res)
  }
  
  if(any(vif_res > 10, na.rm=TRUE)) { 
    mensaje_alerta("Se detectó Multicolinealidad Severa (VIF > 10).")
  } else {
    mensaje_exito("Multicolinealidad bajo control.")
  }
})


# 3.2 Heterocedasticidad (Test de Breusch-Pagan)
# ---------------------------------------------
# H0: Homocedasticidad (Varianza de los errores es constante)
# H1: Heterocedasticidad (Varianza cambia)

bp_test <- bptest(modelo_c)

cat("\n--- TEST DE BREUSCH-PAGAN ---\n")
print(bp_test)

if (bp_test$p.value < ALPHA_SIGNIFICANCIA) {
  mensaje_exito("Se rechaza H0: Heterocedasticidad detectada.")
  mensaje_proceso("CORRECCIÓN: Se utilizarán Errores Estándar Robustos (HC1) para la inferencia final.")
} else {
  mensaje_proceso("No se rechaza H0: Homocedasticidad plausible.")
}











# 4. RESULTADOS FINALES CON INFERENCIA ROBUSTA

# Calculamos la matriz de covarianza robusta (White / HC1)
# Esto ajusta los p-valores para que sean válidos incluso con heterocedasticidad.
res_robustos <- coeftest(modelo_c, vcov = vcovHC(modelo_c, type = "HC1"))

cat("\n======================================================\n")
cat(" RESULTADOS FINALES ROBUSTOS (HIPÓTESIS C)\n")
cat("======================================================\n")
print(res_robustos)







# 5. INTERPRETACIÓN AUTOMÁTICA (CLARIDAD EXPOSITIVA)

cat("\n======================================================\n")
cat("   INTERPRETACIÓN EJECUTIVA DE LA HIPÓTESIS C\n")
cat("======================================================\n")

# --- Variable COMPLEJIDAD ---
coef_comp <- res_robustos["complejidad_producto_win", "Estimate"]
pval_comp <- res_robustos["complejidad_producto_win", "Pr(>|t|)"]
es_sig_comp <- pval_comp < ALPHA

# --- Variable DISTANCIA ---
coef_dist <- res_robustos["distancia", "Estimate"]
pval_dist <- res_robustos["distancia", "Pr(>|t|)"]
es_sig_dist <- pval_dist < ALPHA

# Generamos el reporte en consola
cat("\n--- ANÁLISIS DE SIGNOS Y SIGNIFICANCIA ---\n")

# Reporte para Complejidad (Esperábamos Positivo +)
cat(paste0("1. Complejidad (X1): Coef = ", round(coef_comp, 4), 
           " | P-Valor = ", format.pval(pval_comp, digits=3), "\n"))
if(es_sig_comp && coef_comp > 0) {
  cat("   ✅ CONFIRMA H1: A mayor complejidad, mayor potencialidad.\n")
} else {
  cat("   ❌ NO CONFIRMA H1 (No significativo o signo contrario).\n")
}

cat("\n")

# Reporte para Distancia (Esperábamos Negativo -, pero nos dio Positivo +)
cat(paste0("2. Distancia (X2):   Coef = ", round(coef_dist, 4), 
           " | P-Valor = ", format.pval(pval_dist, digits=3), "\n"))

if(es_sig_dist && coef_dist < 0) {
  cat("   ✅ CONFIRMA H1: A mayor distancia, menor potencialidad.\n")
} else if(es_sig_dist && coef_dist > 0) {
  cat("   🔄 REFUTACION: Relación POSITIVA y SIGNIFICATIVA.\n")
  cat("      Interpretación: Los productos con mayor potencial estratégico son los más 'lejanos'.\n")
} else {
  cat("   ❌ NO SIGNIFICATIVO: La distancia no parece influir.\n")
}






# 6. CONCLUSIÓN AUTOMÁTICA Y GUARDADO

# Extraemos coeficientes de interés
coef_dist <- res_robustos["distancia", "Estimate"]
pval_dist <- res_robustos["distancia", "Pr(>|t|)"]

coef_comp <- res_robustos["complejidad_producto_win", "Estimate"]
pval_comp <- res_robustos["complejidad_producto_win", "Pr(>|t|)"]

cat("\n*** CONCLUSIÓN DE LA HIPÓTESIS ***\n")

# Evaluación Distancia (Esperamos signo negativo)
if (coef_dist < 0 && pval_dist < ALPHA_SIGNIFICANCIA) {
  cat("✅ DISTANCIA: Confirma Hipótesis. A mayor distancia, menor potencialidad (p < 0.05).\n")
} else {
  cat("❌ DISTANCIA: No confirma Hipótesis (Signo incorrecto o no significativo).\n")
}

# Evaluación Complejidad (Esperamos signo positivo)
if (coef_comp > 0 && pval_comp < ALPHA_SIGNIFICANCIA) {
  cat("✅ COMPLEJIDAD: Confirma Hipótesis. Productos más complejos aumentan la potencialidad.\n")
} else {
  cat("❌ COMPLEJIDAD: No confirma Hipótesis.\n")
}

# Guardar tabla de resultados para el informe
ruta_salida_tabla <- file.path(dir_outputs_tables, "regresion_hipotesis_c_robusta.txt")
capture.output(res_robustos, file = ruta_salida_tabla)

mensaje_exito(paste("Resultados guardados en:", ruta_salida_tabla))

cat("\n*** CONCLUSIÓN DE LA HIPÓTESIS C ***\n")

# Interpretación para la Distancia
if (coef_dist < 0 && pval_dist < ALPHA_SIGNIFICANCIA) {
  cat("✅ DISTANCIA: Confirma Hipótesis inicial (Relación Negativa).\n")
} else if (coef_dist > 0 && pval_dist < ALPHA_SIGNIFICANCIA) {
  cat("🔄 HALLAZGO INTERESANTE (DISTANCIA): La hipótesis inicial de relación negativa se rechaza.\n")
  cat("   Se encontró una relación POSITIVA y SIGNIFICATIVA (Coef =", round(coef_dist, 4), ").\n")
  cat("   INTERPRETACIÓN ECONÓMICA: Esto sugiere un 'trade-off' entre factibilidad y valor.\n")
  cat("   Los productos con mayor potencialidad estratégica son aquellos que están más 'lejos'\n")
  cat("   de las capacidades actuales de la provincia (mayor distancia).\n")
} else {
  cat("❌ DISTANCIA: No significativa.\n")
}

# Interpretación para la Complejidad
if (coef_comp > 0 && pval_comp < ALPHA_SIGNIFICANCIA) {
  cat("✅ COMPLEJIDAD: Confirma Hipótesis. Mayor complejidad implica mayor potencialidad.\n")
}

mensaje_exito("Análisis completado. ¡Revisar hallazgo sobre Distancia en el informe!")