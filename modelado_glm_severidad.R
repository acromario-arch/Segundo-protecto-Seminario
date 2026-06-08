# ============================================================================
# MODELADO GLM DE SEVERIDAD - TARIFICACIÓN DE SEGUROS DE AUTOS
# ============================================================================
# Objetivo: Construir y validar modelo GLM Gamma/Lognormal
# para predecir la severidad (costo promedio por siniestro)
# ============================================================================

# Librerías necesarias
library(sqldf)
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)
library(ranger)
library(Metrics)

# ============================================================================
# PASO 1: CARGAR DATOS Y CREAR BASE DE SEVERIDAD
# ============================================================================

cat("\n========== PASO 1: CONSTRUCCIÓN DE BASE DE SEVERIDAD ==========\n")

# Cargar la base de datos maestra
base_modelacion <- read.csv("generales_tot_fin.csv", stringsAsFactors = FALSE)

cat("Base original - Dimensiones:", nrow(base_modelacion), "filas,", 
    ncol(base_modelacion), "columnas\n")

# Seleccionar variables necesarias
variables_necesarias <- c("id", "exposicion", "frecuencia", "severidad_promedio",
                          "costo_total", "entidad", "tipo_veh", "marca_tipo", 
                          "cve_amis", "modelo", "uso", "cobertura", "sa", 
                          "deducible", "prima_emi")

base_siniestros <- base_modelacion %>%
  select(any_of(variables_necesarias))

# Verificar variables disponibles
variables_faltantes <- setdiff(variables_necesarias, names(base_siniestros))
if(length(variables_faltantes) > 0) {
  cat("⚠️ ADVERTENCIA: Variables faltantes:", paste(variables_faltantes, collapse=", "), "\n")
}

cat("\nVariables disponibles:", names(base_siniestros), "\n")

# ============================================================================
# PASO 2: FILTROS PARA CONSTRUCCIÓN DE BASE DE SEVERIDAD
# ============================================================================

cat("\n========== PASO 2: FILTROS PARA SEVERIDAD ==========\n")

cat("\nObservaciones originales:", nrow(base_siniestros), "\n")

# Filtro 1: Solo registros con frecuencia > 0
base_siniestros <- base_siniestros %>%
  filter(frecuencia > 0)

cat("Después de filtrar frecuencia > 0:", nrow(base_siniestros), "registros\n")

# Filtro 2: Excluir severidades nulas o negativas
base_siniestros <- base_siniestros %>%
  filter(!is.na(severidad_promedio),
         severidad_promedio > 0)

cat("Después de excluir severidades inválidas:", nrow(base_siniestros), "registros\n")

# Filtro 3: Remover faltantes críticos
base_siniestros <- base_siniestros %>%
  filter(!is.na(entidad),
         !is.na(tipo_veh),
         !is.na(uso))

cat("Después de remover faltantes críticos:", nrow(base_siniestros), "registros\n")

cat("\n✓ Base de severidad construida exitosamente\n")
cat("  Número final de observaciones:", nrow(base_siniestros), "\n")

# Guardar tamaño de muestra
n_severidad <- nrow(base_siniestros)

# ============================================================================
# PASO 3: ANÁLISIS EXPLORATORIO DE SEVERIDAD
# ============================================================================

cat("\n========== PASO 3: ANÁLISIS EXPLORATORIO DE SEVERIDAD ==========\n")

cat("\n--- ESTADÍSTICAS DESCRIPTIVAS ---\n")
cat("Media:", mean(base_siniestros$severidad_promedio), "\n")
cat("Mediana:", median(base_siniestros$severidad_promedio), "\n")
cat("Desv. Estándar:", sd(base_siniestros$severidad_promedio), "\n")
cat("Mínimo:", min(base_siniestros$severidad_promedio), "\n")
cat("Máximo:", max(base_siniestros$severidad_promedio), "\n")

# Estadísticas completas
est_completa <- data.frame(
  Estadístico = c("Media", "Mediana", "Desv.Estándar", "Mínimo", "Máximo",
                  "Q1 (25%)", "Q2 (50%)", "Q3 (75%)", "Rango", "Rango Intercuartil"),
  Valor = c(
    mean(base_siniestros$severidad_promedio),
    median(base_siniestros$severidad_promedio),
    sd(base_siniestros$severidad_promedio),
    min(base_siniestros$severidad_promedio),
    max(base_siniestros$severidad_promedio),
    quantile(base_siniestros$severidad_promedio, 0.25),
    quantile(base_siniestros$severidad_promedio, 0.50),
    quantile(base_siniestros$severidad_promedio, 0.75),
    max(base_siniestros$severidad_promedio) - min(base_siniestros$severidad_promedio),
    IQR(base_siniestros$severidad_promedio)
  )
)

print(est_completa)

# Coeficiente de asimetría (skewness)
library(moments)
skewness_valor <- skewness(base_siniestros$severidad_promedio)

cat("\n--- FORMA DE LA DISTRIBUCIÓN ---\n")
cat("Coeficiente de Asimetría (Skewness):", skewness_valor, "\n")

if(skewness_valor > 1) {
  cat("Interpretación: Distribución MUY asimétrica a la derecha (cola larga)\n")
  cat("              → Apropiada para Gamma o Lognormal\n")
} else if(skewness_valor > 0.5) {
  cat("Interpretación: Distribución moderadamente asimétrica a la derecha\n")
  cat("              → Gamma o Lognormal pueden ser buenos\n")
} else {
  cat("Interpretación: Distribución aproximadamente simétrica\n")
}

# ============================================================================
# PASO 4: IDENTIFICACIÓN DE VALORES EXTREMOS
# ============================================================================

cat("\n========== PASO 4: IDENTIFICACIÓN DE VALORES EXTREMOS ==========\n")

# Calcular percentiles
percentiles <- c(0.90, 0.95, 0.99, 0.995, 0.999)
valores_percentiles <- quantile(base_siniestros$severidad_promedio, percentiles)

cat("\n--- PERCENTILES EXTREMOS ---\n")
for(i in 1:length(percentiles)) {
  pct <- percentiles[i] * 100
  val <- valores_percentiles[i]
  cat(sprintf("P%.1f: $%.2f\n", pct, val))
}

# Crear tabla de análisis de extremos
analisis_extremos <- data.frame(
  Percentil = c("P90", "P95", "P99", "P99.5", "P99.9"),
  Valor = valores_percentiles,
  Observaciones = NA,
  Porcentaje = NA
)

for(i in 1:nrow(analisis_extremos)) {
  obs <- sum(base_siniestros$severidad_promedio > analisis_extremos$Valor[i])
  analisis_extremos$Observaciones[i] <- obs
  analisis_extremos$Porcentaje[i] <- round(obs/n_severidad*100, 2)
}

cat("\n--- TABLA DE EXTREMOS ---\n")
print(analisis_extremos)

# Identificar outliers por método IQR
Q1 <- quantile(base_siniestros$severidad_promedio, 0.25)
Q3 <- quantile(base_siniestros$severidad_promedio, 0.75)
IQR_val <- Q3 - Q1
lim_inf <- Q1 - 1.5 * IQR_val
lim_sup <- Q3 + 1.5 * IQR_val

outliers_iqr <- base_siniestros %>%
  filter(severidad_promedio < lim_inf | severidad_promedio > lim_sup)

cat("\n--- OUTLIERS (MÉTODO IQR) ---\n")
cat("Límite inferior:", lim_inf, "\n")
cat("Límite superior:", lim_sup, "\n")
cat("Número de outliers:", nrow(outliers_iqr), 
    "(", round(nrow(outliers_iqr)/n_severidad*100, 2), "%)\n")

# ============================================================================
# PASO 5: VISUALIZACIONES DE SEVERIDAD
# ============================================================================

cat("\n========== PASO 5: VISUALIZACIONES DE SEVERIDAD ==========\n")

# Histograma
g_hist <- ggplot(base_siniestros, aes(x=severidad_promedio)) +
  geom_histogram(bins=50, fill="steelblue", alpha=0.7) +
  scale_x_continuous(limits=c(0, quantile(base_siniestros$severidad_promedio, 0.95))) +
  theme_minimal() +
  labs(title="Distribución de Severidad Promedio (hasta P95)",
       x="Severidad Promedio ($)", y="Frecuencia")

print(g_hist)

# Boxplot
g_box <- ggplot(base_siniestros, aes(y=severidad_promedio)) +
  geom_boxplot(fill="coral", alpha=0.7) +
  scale_y_continuous(limits=c(0, quantile(base_siniestros$severidad_promedio, 0.95))) +
  theme_minimal() +
  labs(title="Boxplot de Severidad Promedio",
       y="Severidad Promedio ($)")

print(g_box)

# Histograma de log-severidad
g_hist_log <- ggplot(base_siniestros, aes(x=log(severidad_promedio))) +
  geom_histogram(bins=50, fill="green", alpha=0.7) +
  theme_minimal() +
  labs(title="Distribución de Log(Severidad)",
       x="Log(Severidad Promedio)", y="Frecuencia")

print(g_hist_log)

# ============================================================================
# PASO 6: DECISIÓN SOBRE TRATAMIENTO DE EXTREMOS
# ============================================================================

cat("\n========== PASO 6: DECISIÓN SOBRE TRATAMIENTO DE EXTREMOS ==========\n")

cat("\n--- ESCENARIOS DE TRATAMIENTO ---\n")

# Escenario 1: Sin tratamiento
n_sin_tratamiento <- n_severidad
media_sin_tratamiento <- mean(base_siniestros$severidad_promedio)

# Escenario 2: Winsorizar al P99
p99_valor <- quantile(base_siniestros$severidad_promedio, 0.99)
base_winsor <- base_siniestros %>%
  mutate(severidad_winsor = pmin(severidad_promedio, p99_valor))
n_winsor <- n_severidad
media_winsor <- mean(base_winsor$severidad_winsor)

# Escenario 3: Truncar al P99
base_trunc <- base_siniestros %>%
  filter(severidad_promedio <= p99_valor)
n_trunc <- nrow(base_trunc)
media_trunc <- mean(base_trunc$severidad_promedio)

# Escenario 4: Truncar al P99.5
p995_valor <- quantile(base_siniestros$severidad_promedio, 0.995)
base_trunc995 <- base_siniestros %>%
  filter(severidad_promedio <= p995_valor)
n_trunc995 <- nrow(base_trunc995)
media_trunc995 <- mean(base_trunc995$severidad_promedio)

# Tabla comparativa
escenarios <- data.frame(
  Escenario = c("Sin tratamiento", "Winsorizar P99", "Truncar P99", "Truncar P99.5"),
  N_Observaciones = c(n_sin_tratamiento, n_winsor, n_trunc, n_trunc995),
  Media_Severidad = c(media_sin_tratamiento, media_winsor, media_trunc, media_trunc995),
  Obs_Eliminadas = c(0, 0, 100-n_trunc, 100-n_trunc995),
  Porcentaje_Perdida = c(0, 0, 
                         round((n_severidad - n_trunc)/n_severidad*100, 2),
                         round((n_severidad - n_trunc995)/n_severidad*100, 2))
)

cat("\nComparación de escenarios:\n")
print(escenarios)

# Decisión: Usar datos sin tratamiento pero marcar extremos para análisis
cat("\n--- DECISIÓN RECOMENDADA ---\n")
cat("✓ Mantener TODOS los datos sin truncar\n")
cat("  Razones:\n")
cat("  1. Los valores extremos son siniestros reales (no errores)\n")
cat("  2. Gamma y Lognormal pueden manejar colas largas\n")
cat("  3. Preserva información actuarial importante\n")
cat("  4. Los modelos GLM están diseñados para esto\n")

# Usar base sin tratamiento
base_severidad <- base_siniestros

cat("\n✓ Base de severidad lista para modelado\n")
cat("  Observaciones finales:", nrow(base_severidad), "\n")

# ============================================================================
# PASO 7: PREPARACIÓN DE VARIABLES
# ============================================================================

cat("\n========== PASO 7: PREPARACIÓN DE VARIABLES ==========\n")

# Convertir variables categóricas a factor
variables_factor <- c("entidad", "tipo_veh", "marca_tipo", "cve_amis", 
                      "modelo", "uso", "cobertura")

base_severidad <- base_severidad %>%
  mutate(across(all_of(variables_factor), 
                ~factor(., exclude = NULL)))

# Revisar niveles con pocos registros
cat("\n--- REVISIÓN DE NIVELES CON POCOS REGISTROS ---\n")

umbral_min <- 30

for(var in variables_factor) {
  if(var %in% names(base_severidad)) {
    dist <- table(base_severidad[[var]])
    pocos_reg <- names(dist[dist < umbral_min])
    if(length(pocos_reg) > 0) {
      cat("\n⚠️", var, "- Niveles con <", umbral_min, "registros:\n")
      print(dist[dist < umbral_min])
      
      # Agrupar en "OTROS"
      base_severidad[[var]] <- fct_collapse(base_severidad[[var]],
                                            OTROS = pocos_reg)
      cat("   → Agrupados en 'OTROS'\n")
    }
  }
}

cat("\n✓ Variables preparadas para modelado\n")

# ============================================================================
# PASO 8: DIVISIÓN TRAIN/TEST
# ============================================================================

cat("\n========== PASO 8: DIVISIÓN TRAIN/TEST ==========\n")

set.seed(123)

# Índices para entrenamiento (70%)
indice_train <- createDataPartition(base_severidad$severidad_promedio, 
                                    p=0.7, list=FALSE)

datos_train <- base_severidad[indice_train, ]
datos_test <- base_severidad[-indice_train, ]

cat("Tamaño conjunto de entrenamiento:", nrow(datos_train), "registros\n")
cat("Tamaño conjunto de prueba:", nrow(datos_test), "registros\n")
cat("Proporción:", round(nrow(datos_train)/nrow(base_severidad)*100, 1), 
    "% entrenamiento,", 
    round(nrow(datos_test)/nrow(base_severidad)*100, 1), "% prueba\n")

# ============================================================================
# PASO 9: CONSTRUCCIÓN MODELO GLM GAMMA
# ============================================================================

cat("\n========== PASO 9: CONSTRUCCIÓN MODELO GLM GAMMA ==========\n")

# Fórmula del modelo
formula_glm <- severidad_promedio ~
  entidad +
  tipo_veh +
  uso +
  marca_tipo +
  cve_amis +
  modelo +
  cobertura +
  sa +
  deducible

# Ajuste del modelo Gamma con enlace log
modelo_gamma <- glm(formula_glm,
                    family=Gamma(link="log"),
                    data=datos_train,
                    control=glm.control(maxit=100))

cat("\n✓ Modelo Gamma ajustado exitosamente\n")

# ============================================================================
# PASO 10: RESUMEN Y DIAGNÓSTICO MODELO GAMMA
# ============================================================================

cat("\n========== PASO 10: RESUMEN MODELO GAMMA ==========\n")

cat("\n--- SUMMARY COMPLETO ---\n")
print(summary(modelo_gamma))

# Métricas clave
aic_gamma <- AIC(modelo_gamma)
deviance_residual_gamma <- modelo_gamma$deviance
gl_residuales_gamma <- modelo_gamma$df.residual
deviance_nula_gamma <- modelo_gamma$null.deviance

cat("\n--- MÉTRICAS CLAVE ---\n")
cat("AIC:", round(aic_gamma, 2), "\n")
cat("Null Deviance:", round(deviance_nula_gamma, 2), "con", gl_residuales_gamma, "g.l.\n")
cat("Residual Deviance:", round(deviance_residual_gamma, 2), "\n")

# Coeficientes y significancia
cat("\n--- COEFICIENTES Y SIGNIFICANCIA ---\n")
coef_gamma <- as.data.frame(summary(modelo_gamma)$coefficients)
coef_gamma$Variable <- rownames(coef_gamma)
colnames(coef_gamma) <- c("Coeficiente", "Err.Std", "t.value", "Pr(>|t|)", "Variable")
coef_gamma <- coef_gamma[, c("Variable", "Coeficiente", "Err.Std", "t.value", "Pr(>|t|)")]

# Añadir interpretación
coef_gamma$Exp.Coef <- exp(coef_gamma$Coeficiente)
coef_gamma$Significancia <- ifelse(coef_gamma$`Pr(>|t|)` < 0.001, "***",
                                   ifelse(coef_gamma$`Pr(>|t|)` < 0.01, "**",
                                   ifelse(coef_gamma$`Pr(>|t|)` < 0.05, "*",
                                   ifelse(coef_gamma$`Pr(>|t|)` < 0.1, ".", " "))))

print(coef_gamma)

# ============================================================================
# PASO 11: CONSTRUCCIÓN MODELO LOGNORMAL
# ============================================================================

cat("\n========== PASO 11: CONSTRUCCIÓN MODELO LOGNORMAL ==========\n")

# Crear variable respuesta transformada
datos_train_ln <- datos_train %>%
  mutate(log_severidad = log(severidad_promedio))

# Ajuste del modelo con regresión lineal en escala log
modelo_lognormal <- glm(log_severidad ~
                        entidad +
                        tipo_veh +
                        uso +
                        marca_tipo +
                        cve_amis +
                        modelo +
                        cobertura +
                        sa +
                        deducible,
                      family=gaussian(link="identity"),
                      data=datos_train_ln,
                      control=glm.control(maxit=100))

cat("\n✓ Modelo Lognormal ajustado exitosamente\n")

# ============================================================================
# PASO 12: RESUMEN Y DIAGNÓSTICO MODELO LOGNORMAL
# ============================================================================

cat("\n========== PASO 12: RESUMEN MODELO LOGNORMAL ==========\n")

cat("\n--- SUMMARY COMPLETO ---\n")
print(summary(modelo_lognormal))

# Métricas clave
aic_lognormal <- AIC(modelo_lognormal)
deviance_residual_lognormal <- modelo_lognormal$deviance
gl_residuales_lognormal <- modelo_lognormal$df.residual

cat("\n--- MÉTRICAS CLAVE ---\n")
cat("AIC:", round(aic_lognormal, 2), "\n")
cat("Residual Deviance:", round(deviance_residual_lognormal, 2), "\n")

# Coeficientes
cat("\n--- COEFICIENTES ---\n")
coef_lognormal <- as.data.frame(summary(modelo_lognormal)$coefficients)
coef_lognormal$Variable <- rownames(coef_lognormal)
colnames(coef_lognormal) <- c("Coeficiente", "Err.Std", "t.value", "Pr(>|t|)", "Variable")
coef_lognormal <- coef_lognormal[, c("Variable", "Coeficiente", "Err.Std", "t.value", "Pr(>|t|)")]

coef_lognormal$Exp.Coef <- exp(coef_lognormal$Coeficiente)
coef_lognormal$Significancia <- ifelse(coef_lognormal$`Pr(>|t|)` < 0.001, "***",
                                       ifelse(coef_lognormal$`Pr(>|t|)` < 0.01, "**",
                                       ifelse(coef_lognormal$`Pr(>|t|)` < 0.05, "*",
                                       ifelse(coef_lognormal$`Pr(>|t|)` < 0.1, ".", " "))))

print(coef_lognormal)

# ============================================================================
# PASO 13: PREDICCIONES EN TRAIN Y TEST
# ============================================================================

cat("\n========== PASO 13: PREDICCIONES EN TRAIN Y TEST ==========\n")

# Predicciones Gamma
pred_gamma_train <- predict(modelo_gamma, newdata=datos_train, type="response")
pred_gamma_test <- predict(modelo_gamma, newdata=datos_test, type="response")

# Predicciones Lognormal (transformar de vuelta a escala original)
datos_test_ln <- datos_test %>%
  mutate(log_severidad = log(severidad_promedio))

pred_lognormal_train_log <- predict(modelo_lognormal, newdata=datos_train_ln, type="response")
pred_lognormal_train <- exp(pred_lognormal_train_log)

pred_lognormal_test_log <- predict(modelo_lognormal, newdata=datos_test_ln, type="response")
pred_lognormal_test <- exp(pred_lognormal_test_log)

cat("✓ Predicciones generadas para ambos modelos\n")

# ============================================================================
# PASO 14: MÉTRICAS PREDICTIVAS
# ============================================================================

cat("\n========== PASO 14: MÉTRICAS PREDICTIVAS ==========\n")

# GAMMA
rmse_gamma_train <- sqrt(mean((datos_train$severidad_promedio - pred_gamma_train)^2))
mae_gamma_train <- mean(abs(datos_train$severidad_promedio - pred_gamma_train))

rmse_gamma_test <- sqrt(mean((datos_test$severidad_promedio - pred_gamma_test)^2))
mae_gamma_test <- mean(abs(datos_test$severidad_promedio - pred_gamma_test))

# LOGNORMAL
rmse_lognormal_train <- sqrt(mean((datos_train$severidad_promedio - pred_lognormal_train)^2))
mae_lognormal_train <- mean(abs(datos_train$severidad_promedio - pred_lognormal_train))

rmse_lognormal_test <- sqrt(mean((datos_test$severidad_promedio - pred_lognormal_test)^2))
mae_lognormal_test <- mean(abs(datos_test$severidad_promedio - pred_lognormal_test))

cat("\n--- MODELO GAMMA ---\n")
cat("Entrenamiento - RMSE:", round(rmse_gamma_train, 2), 
    "MAE:", round(mae_gamma_train, 2), "\n")
cat("Prueba - RMSE:", round(rmse_gamma_test, 2), 
    "MAE:", round(mae_gamma_test, 2), "\n")

cat("\n--- MODELO LOGNORMAL ---\n")
cat("Entrenamiento - RMSE:", round(rmse_lognormal_train, 2), 
    "MAE:", round(mae_lognormal_train, 2), "\n")
cat("Prueba - RMSE:", round(rmse_lognormal_test, 2), 
    "MAE:", round(mae_lognormal_test, 2), "\n")

# ============================================================================
# PASO 15: COMPARACIÓN GAMMA VS LOGNORMAL
# ============================================================================

cat("\n========== PASO 15: COMPARACIÓN GAMMA VS LOGNORMAL ==========\n")

comparativa_severidad <- data.frame(
  Métrica = c("AIC", "RMSE Train", "MAE Train", "RMSE Test", "MAE Test",
              "Interpretabilidad", "Estabilidad"),
  Gamma = c(
    round(aic_gamma, 2),
    round(rmse_gamma_train, 2),
    round(mae_gamma_train, 2),
    round(rmse_gamma_test, 2),
    round(mae_gamma_test, 2),
    "Escala original",
    "Moderada"
  ),
  Lognormal = c(
    round(aic_lognormal, 2),
    round(rmse_lognormal_train, 2),
    round(mae_lognormal_train, 2),
    round(rmse_lognormal_test, 2),
    round(mae_lognormal_test, 2),
    "Log-escala",
    "Alta"
  )
)

cat("\n--- COMPARATIVA DE MODELOS ---\n")
print(comparativa_severidad)

# Selección del modelo
cat("\n--- SELECCIÓN DEL MODELO ---\n")

if(aic_gamma < aic_lognormal) {
  cat("✓ RECOMENDACIÓN: MODELO GAMMA\n")
  cat("   → AIC menor en Gamma (", round(aic_gamma, 2), " vs ", 
      round(aic_lognormal, 2), ")\n")
  cat("   → Mejor desempeño predictivo\n")
  modelo_seleccionado_sev <- modelo_gamma
  predicciones_test_final <- pred_gamma_test
  nombre_modelo_sev <- "Gamma"
} else {
  cat("✓ RECOMENDACIÓN: MODELO LOGNORMAL\n")
  cat("   → AIC menor en Lognormal (", round(aic_lognormal, 2), " vs ", 
      round(aic_gamma, 2), ")\n")
  cat("   → Mejor manejo de colas largas\n")
  modelo_seleccionado_sev <- modelo_lognormal
  predicciones_test_final <- pred_lognormal_test
  nombre_modelo_sev <- "Lognormal"
}

# ============================================================================
# PASO 16: ANÁLISIS DE IMPORTANCIA DE VARIABLES
# ============================================================================

cat("\n========== PASO 16: ANÁLISIS DE IMPORTANCIA DE VARIABLES ==========\n")

# Usar coeficientes del modelo seleccionado
if(nombre_modelo_sev == "Gamma") {
  coef_importancia_sev <- coef_gamma
} else {
  coef_importancia_sev <- coef_lognormal
}

# Reordenar por valor absoluto de t
coef_importancia_sev$abs_t <- abs(coef_importancia_sev$t.value)
coef_importancia_sev <- coef_importancia_sev %>%
  arrange(desc(abs_t))

cat("\n--- TOP 15 VARIABLES MÁS SIGNIFICATIVAS ---\n")
print(head(coef_importancia_sev[, c("Variable", "Coeficiente", "Err.Std", 
                                     "Pr(>|t|)", "Significancia", "Exp.Coef")], 15))

# Gráfico de importancia
g_importancia_sev <- coef_importancia_sev %>%
  filter(Variable != "(Intercept)") %>%
  head(20) %>%
  ggplot(aes(x=reorder(Variable, Coeficiente), y=Coeficiente, 
             fill=ifelse(Coeficiente>0, "Positivo", "Negativo"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal() +
  labs(title=paste("Top 20 Variables -", nombre_modelo_sev),
       x="Variable", y="Coeficiente") +
  scale_fill_manual(values=c("Positivo"="green", "Negativo"="red"))

print(g_importancia_sev)

# ============================================================================
# PASO 17: TABLA RESUMEN DE VARIABLES
# ============================================================================

cat("\n========== PASO 17: TABLA RESUMEN DE VARIABLES ==========\n")

tabla_resumen_sev <- coef_importancia_sev %>%
  select(Variable, Coeficiente, Err.Std, `Pr(>|t|)`, Significancia, Exp.Coef) %>%
  mutate(
    Interpretacion = case_when(
      Variable == "(Intercept)" ~ "Intercepto del modelo",
      Significancia == "***" ~ "MUY significativa (p<0.001)",
      Significancia == "**" ~ "Muy significativa (p<0.01)",
      Significancia == "*" ~ "Significativa (p<0.05)",
      Significancia == "." ~ "Moderadamente significativa (p<0.1)",
      TRUE ~ "No significativa"
    ),
    Efecto = case_when(
      Coeficiente > 0 ~ paste0("Aumenta severidad ~", round((Exp.Coef-1)*100, 1), "%"),
      Coeficiente < 0 ~ paste0("Reduce severidad ~", round((1-Exp.Coef)*100, 1), "%"),
      TRUE ~ "Sin efecto"
    )
  ) %>%
  select(Variable, Coeficiente, Err.Std, `Pr(>|t|)`, Significancia, 
         Interpretacion, Efecto)

print(tabla_resumen_sev)

# ============================================================================
# PASO 18: DIAGNÓSTICOS DEL MODELO
# ============================================================================

cat("\n========== PASO 18: DIAGNÓSTICOS DEL MODELO ==========\n")

# Residuos
residuos_sev <- residuals(modelo_seleccionado_sev, type="deviance")

# Q-Q plot
g_qq_sev <- ggplot() +
  geom_qq(aes(sample=residuos_sev)) +
  geom_qq_line(aes(sample=residuos_sev), color="red") +
  theme_minimal() +
  labs(title=paste("Q-Q Plot -", nombre_modelo_sev))

print(g_qq_sev)

# Residuos vs ajustados
residuos_df_sev <- data.frame(
  ajustados = fitted(modelo_seleccionado_sev),
  residuos = residuals(modelo_seleccionado_sev, type="deviance")
)

g_resid_sev <- ggplot(residuos_df_sev, aes(x=ajustados, y=residuos)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_minimal() +
  labs(title=paste("Residuos vs Valores Ajustados -", nombre_modelo_sev),
       x="Valores Ajustados", y="Residuos de Deviance")

print(g_resid_sev)

# ============================================================================
# PASO 19: REPORTE FINAL
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("REPORTE FINAL - MODELADO GLM DE SEVERIDAD\n")
cat(strrep("=", 80), "\n")

cat("\n1. MODELO SELECCIONADO\n")
cat("   └─ ", nombre_modelo_sev, "\n\n")

cat("2. TRATAMIENTO DE VALORES EXTREMOS\n")
cat("   └─ Decisión: Mantener TODOS los datos sin truncar\n")
cat("   └─ Justificación:\n")
cat("      • Son siniestros reales documentados\n")
cat("      • Gamma y Lognormal manejan colas largas\n")
cat("      • Preserva información actuarial\n")
cat("      • Observaciones finales:", nrow(base_severidad), "\n")

cat("\n3. VARIABLES RELEVANTES (p < 0.05)\n")
vars_sig_sev <- coef_importancia_sev %>%
  filter(Variable != "(Intercept)", `Pr(>|t|)` < 0.05)
if(nrow(vars_sig_sev) > 0) {
  for(i in 1:min(10, nrow(vars_sig_sev))) {
    var_info <- vars_sig_sev[i, ]
    cat("   └─ ", var_info$Variable, " (coef: ", 
        round(var_info$Coeficiente, 4), ", p: ", 
        round(var_info$`Pr(>|t|)`, 4), ")\n", sep="")
  }
} else {
  cat("   └─ Ninguna variable significativa\n")
}

cat("\n4. MÉTRICAS DE CALIDAD PREDICTIVA\n")
if(nombre_modelo_sev == "Gamma") {
  cat("   Entrenamiento:\n")
  cat("   ├─ RMSE:", round(rmse_gamma_train, 2), "\n")
  cat("   └─ MAE: ", round(mae_gamma_train, 2), "\n")
  cat("   Prueba:\n")
  cat("   ├─ RMSE:", round(rmse_gamma_test, 2), "\n")
  cat("   └─ MAE: ", round(mae_gamma_test, 2), "\n")
} else {
  cat("   Entrenamiento:\n")
  cat("   ├─ RMSE:", round(rmse_lognormal_train, 2), "\n")
  cat("   └─ MAE: ", round(mae_lognormal_train, 2), "\n")
  cat("   Prueba:\n")
  cat("   ├─ RMSE:", round(rmse_lognormal_test, 2), "\n")
  cat("   └─ MAE: ", round(mae_lognormal_test, 2), "\n")
}

cat("\n5. BONDAD DE AJUSTE\n")
cat("   ├─ AIC: ", round(AIC(modelo_seleccionado_sev), 2), "\n")
cat("   └─ Deviance: ", round(modelo_seleccionado_sev$deviance, 2), "\n")

cat("\n6. CONCLUSIONES ACTUARIALES\n")
cat("   ├─ Modelo ", nombre_modelo_sev, " es apropiado para severidad\n")
cat("   ├─ Distribución de severidad MUY asimétrica (Skewness: ",
    round(skewness_valor, 2), ")\n")
cat("   ├─ Variables críticas: ", 
    paste(head(vars_sig_sev$Variable, 3), collapse=", "), "\n")
cat("   ├─ Capacidad predictiva: ",
    ifelse(rmse_gamma_test < 100, "Buena", "Moderada"), "\n")
cat("   ├─ Percentiles extremos (P95-P99.9) preservados\n")
cat("   └─ Modelo listo para cálculo de prima pura\n")

cat("\n", strrep("=", 80), "\n")

# ============================================================================
# PASO 20: GUARDAR RESULTADOS
# ============================================================================

cat("\n========== PASO 20: GUARDANDO RESULTADOS ==========\n")

# Preparar tabla para Excel
tabla_excel_sev <- tabla_resumen_sev %>%
  arrange(`Pr(>|t|)`) %>%
  mutate(
    Coeficiente = round(Coeficiente, 6),
    Err.Std = round(Err.Std, 6),
    `Pr(>|t|)` = round(`Pr(>|t|)`, 6)
  )

write_xlsx(tabla_excel_sev, "coeficientes_modelo_severidad.xlsx")
cat("✓ Tabla de coeficientes guardada: coeficientes_modelo_severidad.xlsx\n")

# Guardar predicciones
predicciones_severidad <- data.frame(
  id = datos_test$id,
  severidad_real = datos_test$severidad_promedio,
  prediccion = predicciones_test_final,
  residuo = datos_test$severidad_promedio - predicciones_test_final,
  error_porcentual = abs(datos_test$severidad_promedio - predicciones_test_final) / 
                     datos_test$severidad_promedio * 100
)

write_xlsx(predicciones_severidad, "predicciones_test_severidad.xlsx")
cat("✓ Predicciones guardadas: predicciones_test_severidad.xlsx\n")

# Guardar análisis de extremos
write_xlsx(analisis_extremos, "resumen_analisis_extremos.xlsx")
cat("✓ Análisis de extremos guardado: resumen_analisis_extremos.xlsx\n")

# Guardar modelo
saveRDS(modelo_seleccionado_sev, "modelo_glm_severidad.rds")
cat("✓ Modelo guardado: modelo_glm_severidad.rds\n")

# Resumen de entrenamiento
resumen_sev <- data.frame(
  Metrica = c("N Total Siniestros", "N Entrenamiento", "N Prueba",
              "Modelo Seleccionado", "RMSE Test", "MAE Test",
              "AIC", "Variables Significativas", "Skewness",
              "P99 Valor", "Criterio de Extremos"),
  Valor = c(nrow(base_severidad), nrow(datos_train), nrow(datos_test),
            nombre_modelo_sev,
            if(nombre_modelo_sev == "Gamma") round(rmse_gamma_test, 2) 
            else round(rmse_lognormal_test, 2),
            if(nombre_modelo_sev == "Gamma") round(mae_gamma_test, 2) 
            else round(mae_lognormal_test, 2),
            round(AIC(modelo_seleccionado_sev), 2),
            nrow(vars_sig_sev),
            round(skewness_valor, 4),
            round(p99_valor, 2),
            "Sin tratamiento - Mantener todos")
)

write_xlsx(resumen_sev, "resumen_entrenamiento_severidad.xlsx")
cat("✓ Resumen de entrenamiento guardado: resumen_entrenamiento_severidad.xlsx\n")

cat("\n✓ MODELADO DE SEVERIDAD COMPLETADO EXITOSAMENTE\n")
cat("\nArchivos generados:\n")
cat("1. coeficientes_modelo_severidad.xlsx\n")
cat("2. predicciones_test_severidad.xlsx\n")
cat("3. resumen_analisis_extremos.xlsx\n")
cat("4. modelo_glm_severidad.rds\n")
cat("5. resumen_entrenamiento_severidad.xlsx\n")

# ============================================================================
# FIN DEL SCRIPT
# ============================================================================
