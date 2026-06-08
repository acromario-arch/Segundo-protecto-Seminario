# ============================================================================
# COMPARACIÓN DE METODOLOGÍAS: GLM vs ÁRBOL vs RANDOM FOREST
# TARIFICACIÓN DE SEGUROS DE AUTOS
# ============================================================================
# Objetivo: Construir y comparar Árbol de Decisión y Random Forest
# contra GLM para frecuencia y severidad
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
# PASO 1: CARGAR DATOS
# ============================================================================

cat("\n========== PASO 1: CARGA DE DATOS ==========\n")

base_completa <- read.csv("generales_tot_fin.csv", stringsAsFactors = FALSE)

cat("Base de datos cargada:", nrow(base_completa), "registros\n")

# Seleccionar variables
variables_necesarias <- c("id", "exposicion", "frecuencia", "severidad_promedio",
                          "costo_total", "entidad", "tipo_veh", "marca_tipo", 
                          "cve_amis", "modelo", "uso", "cobertura", "sa", 
                          "deducible", "prima_emi")

base_trabajo <- base_completa %>%
  select(any_of(variables_necesarias))

# ============================================================================
# PASO 2: DIVISIÓN ÚNICA TRAIN/TEST
# ============================================================================

cat("\n========== PASO 2: DIVISIÓN TRAIN/TEST (Única partición) ==========\n")

set.seed(123)

# División para frecuencia (todas las observaciones)
indice_train <- createDataPartition(base_trabajo$frecuencia, 
                                    p=0.7, list=FALSE)

datos_train <- base_trabajo[indice_train, ]
datos_test <- base_trabajo[-indice_train, ]

cat("Entrenamiento:", nrow(datos_train), "registros (70%)\n")
cat("Prueba:", nrow(datos_test), "registros (30%)\n")

# Preparar variables categóricas
variables_factor <- c("entidad", "tipo_veh", "marca_tipo", "cve_amis", 
                      "modelo", "uso", "cobertura")

for(df in list(datos_train, datos_test)) {
  df <- df %>%
    mutate(across(all_of(variables_factor), 
                  ~factor(., exclude = NULL)))
}

datos_train <- datos_train %>%
  mutate(across(all_of(variables_factor), ~factor(., exclude = NULL)))

datos_test <- datos_test %>%
  mutate(across(all_of(variables_factor), ~factor(., exclude = NULL)))

# ============================================================================
# PASO 3: CARGAR MODELOS GLM
# ============================================================================

cat("\n========== PASO 3: CARGAR MODELOS GLM ==========\n")

modelo_glm_freq <- readRDS("modelo_glm_frecuencia.rds")
modelo_glm_sev <- readRDS("modelo_glm_severidad.rds")

cat("✓ Modelos GLM cargados\n")

# Generar predicciones GLM
pred_glm_freq_train <- predict(modelo_glm_freq, newdata=datos_train, type="response")
pred_glm_freq_test <- predict(modelo_glm_freq, newdata=datos_test, type="response")

pred_glm_sev_train <- predict(modelo_glm_sev, newdata=datos_train, type="response")
pred_glm_sev_test <- predict(modelo_glm_sev, newdata=datos_test, type="response")

# Prima pura GLM
prima_glm_train <- pred_glm_freq_train * pred_glm_sev_train
prima_glm_test <- pred_glm_freq_test * pred_glm_sev_test

cat("✓ Predicciones GLM generadas\n")

# ============================================================================
# PASO 4: ÁRBOL DE DECISIÓN - FRECUENCIA
# ============================================================================

cat("\n========== PASO 4: ÁRBOL DE DECISIÓN - FRECUENCIA ==========\n")

# Fórmula
formula_freq <- frecuencia ~
  entidad + tipo_veh + uso + marca_tipo + cve_amis + 
  modelo + cobertura + sa + deducible

# Construir árbol inicial
arbol_freq <- rpart(formula_freq,
                    data=datos_train,
                    method="anova",
                    control=rpart.control(xval=10))

cat("Árbol inicial - número de nodos:", nrow(arbol_freq$frame), "\n")

# Poda automática usando cp óptimo
cp_optimo_freq <- arbol_freq$cptable[which.min(arbol_freq$cptable[,"xerror"]), "CP"]
arbol_freq_podado <- prune(arbol_freq, cp=cp_optimo_freq)

cat("CP óptimo:", cp_optimo_freq, "\n")
cat("Árbol podado - número de nodos:", nrow(arbol_freq_podado$frame), "\n")

# Predicciones árbol frecuencia
pred_arbol_freq_train <- predict(arbol_freq_podado, newdata=datos_train)
pred_arbol_freq_test <- predict(arbol_freq_podado, newdata=datos_test)

# Métricas árbol frecuencia
rmse_arbol_freq_train <- sqrt(mean((datos_train$frecuencia - pred_arbol_freq_train)^2))
mae_arbol_freq_train <- mean(abs(datos_train$frecuencia - pred_arbol_freq_train))

rmse_arbol_freq_test <- sqrt(mean((datos_test$frecuencia - pred_arbol_freq_test)^2))
mae_arbol_freq_test <- mean(abs(datos_test$frecuencia - pred_arbol_freq_test))

cat("\nMétricas Árbol Frecuencia:\n")
cat("  Train - RMSE:", round(rmse_arbol_freq_train, 4), 
    "MAE:", round(mae_arbol_freq_train, 4), "\n")
cat("  Test  - RMSE:", round(rmse_arbol_freq_test, 4), 
    "MAE:", round(mae_arbol_freq_test, 4), "\n")

# ============================================================================
# PASO 5: ÁRBOL DE DECISIÓN - SEVERIDAD
# ============================================================================

cat("\n========== PASO 5: ÁRBOL DE DECISIÓN - SEVERIDAD ==========\n")

# Filtrar solo observaciones con frecuencia > 0
datos_train_sev <- datos_train %>% filter(frecuencia > 0)
datos_test_sev <- datos_test %>% filter(frecuencia > 0)

cat("Observaciones con frecuencia > 0:\n")
cat("  Train:", nrow(datos_train_sev), "\n")
cat("  Test:", nrow(datos_test_sev), "\n")

# Fórmula severidad
formula_sev <- severidad_promedio ~
  entidad + tipo_veh + uso + marca_tipo + cve_amis + 
  modelo + cobertura + sa + deducible

# Construir árbol severidad
arbol_sev <- rpart(formula_sev,
                   data=datos_train_sev,
                   method="anova",
                   control=rpart.control(xval=10))

cat("Árbol inicial - número de nodos:", nrow(arbol_sev$frame), "\n")

# Poda automática
cp_optimo_sev <- arbol_sev$cptable[which.min(arbol_sev$cptable[,"xerror"]), "CP"]
arbol_sev_podado <- prune(arbol_sev, cp=cp_optimo_sev)

cat("CP óptimo:", cp_optimo_sev, "\n")
cat("Árbol podado - número de nodos:", nrow(arbol_sev_podado$frame), "\n")

# Predicciones árbol severidad
pred_arbol_sev_train <- predict(arbol_sev_podado, newdata=datos_train_sev)
pred_arbol_sev_test <- predict(arbol_sev_podado, newdata=datos_test_sev)

# Métricas árbol severidad
rmse_arbol_sev_train <- sqrt(mean((datos_train_sev$severidad_promedio - pred_arbol_sev_train)^2))
mae_arbol_sev_train <- mean(abs(datos_train_sev$severidad_promedio - pred_arbol_sev_train))

rmse_arbol_sev_test <- sqrt(mean((datos_test_sev$severidad_promedio - pred_arbol_sev_test)^2))
mae_arbol_sev_test <- mean(abs(datos_test_sev$severidad_promedio - pred_arbol_sev_test))

cat("\nMétricas Árbol Severidad:\n")
cat("  Train - RMSE:", round(rmse_arbol_sev_train, 4), 
    "MAE:", round(mae_arbol_sev_train, 4), "\n")
cat("  Test  - RMSE:", round(rmse_arbol_sev_test, 4), 
    "MAE:", round(mae_arbol_sev_test, 4), "\n")

# ============================================================================
# PASO 6: PRIMA PURA ÁRBOL
# ============================================================================

cat("\n========== PASO 6: PRIMA PURA ÁRBOL ==========\n")

# Predicciones para toda la base (usar predicciones disponibles)
prima_arbol_train <- pred_arbol_freq_train * pred_arbol_sev_train
prima_arbol_test <- pred_arbol_freq_test * pred_arbol_sev_test

# Métricas prima pura árbol (solo en datos_train_sev y datos_test_sev)
rmse_prima_arbol_train <- sqrt(mean((datos_train_sev$costo_total - prima_arbol_train)^2))
mae_prima_arbol_train <- mean(abs(datos_train_sev$costo_total - prima_arbol_train))
corr_prima_arbol_train <- cor(datos_train_sev$costo_total, prima_arbol_train, use="complete.obs")

rmse_prima_arbol_test <- sqrt(mean((datos_test_sev$costo_total - prima_arbol_test)^2))
mae_prima_arbol_test <- mean(abs(datos_test_sev$costo_total - prima_arbol_test))
corr_prima_arbol_test <- cor(datos_test_sev$costo_total, prima_arbol_test, use="complete.obs")

cat("Prima Pura Árbol (Test):\n")
cat("  RMSE:", round(rmse_prima_arbol_test, 2), "\n")
cat("  MAE:", round(mae_prima_arbol_test, 2), "\n")
cat("  Correlación:", round(corr_prima_arbol_test, 4), "\n")

# ============================================================================
# PASO 7: RANDOM FOREST - FRECUENCIA
# ============================================================================

cat("\n========== PASO 7: RANDOM FOREST - FRECUENCIA ==========\n")

set.seed(123)

rf_freq <- ranger(formula_freq,
                  data=datos_train,
                  num.trees=200,
                  mtry=sqrt(ncol(datos_train)-1),
                  importance="impurity",
                  seed=123)

cat("✓ Random Forest Frecuencia entrenado\n")
cat("  Número de árboles:", rf_freq$num.trees, "\n")
cat("  mtry:", floor(sqrt(ncol(datos_train)-1)), "\n")

# Predicciones RF frecuencia
pred_rf_freq_train <- predict(rf_freq, data=datos_train)$predictions
pred_rf_freq_test <- predict(rf_freq, data=datos_test)$predictions

# Métricas RF frecuencia
rmse_rf_freq_train <- sqrt(mean((datos_train$frecuencia - pred_rf_freq_train)^2))
mae_rf_freq_train <- mean(abs(datos_train$frecuencia - pred_rf_freq_train))

rmse_rf_freq_test <- sqrt(mean((datos_test$frecuencia - pred_rf_freq_test)^2))
mae_rf_freq_test <- mean(abs(datos_test$frecuencia - pred_rf_freq_test))

cat("\nMétricas RF Frecuencia:\n")
cat("  Train - RMSE:", round(rmse_rf_freq_train, 4), 
    "MAE:", round(mae_rf_freq_train, 4), "\n")
cat("  Test  - RMSE:", round(rmse_rf_freq_test, 4), 
    "MAE:", round(mae_rf_freq_test, 4), "\n")

# ============================================================================
# PASO 8: RANDOM FOREST - SEVERIDAD
# ============================================================================

cat("\n========== PASO 8: RANDOM FOREST - SEVERIDAD ==========\n")

set.seed(123)

rf_sev <- ranger(formula_sev,
                 data=datos_train_sev,
                 num.trees=200,
                 mtry=sqrt(ncol(datos_train_sev)-1),
                 importance="impurity",
                 seed=123)

cat("✓ Random Forest Severidad entrenado\n")
cat("  Número de árboles:", rf_sev$num.trees, "\n")
cat("  Observaciones:", nrow(datos_train_sev), "\n")

# Predicciones RF severidad
pred_rf_sev_train <- predict(rf_sev, data=datos_train_sev)$predictions
pred_rf_sev_test <- predict(rf_sev, data=datos_test_sev)$predictions

# Métricas RF severidad
rmse_rf_sev_train <- sqrt(mean((datos_train_sev$severidad_promedio - pred_rf_sev_train)^2))
mae_rf_sev_train <- mean(abs(datos_train_sev$severidad_promedio - pred_rf_sev_train))

rmse_rf_sev_test <- sqrt(mean((datos_test_sev$severidad_promedio - pred_rf_sev_test)^2))
mae_rf_sev_test <- mean(abs(datos_test_sev$severidad_promedio - pred_rf_sev_test))

cat("\nMétricas RF Severidad:\n")
cat("  Train - RMSE:", round(rmse_rf_sev_train, 4), 
    "MAE:", round(mae_rf_sev_train, 4), "\n")
cat("  Test  - RMSE:", round(rmse_rf_sev_test, 4), 
    "MAE:", round(mae_rf_sev_test, 4), "\n")

# ============================================================================
# PASO 9: PRIMA PURA RANDOM FOREST
# ============================================================================

cat("\n========== PASO 9: PRIMA PURA RANDOM FOREST ==========\n")

# Prima pura RF
prima_rf_train <- pred_rf_freq_train[indice_train[rownames(datos_train_sev)]] * pred_rf_sev_train
prima_rf_test <- pred_rf_freq_test[-indice_train[rownames(datos_test_sev)]] * pred_rf_sev_test

# Simplificar: usar datos_train_sev y datos_test_sev directamente
prima_rf_train_sev <- pred_rf_freq_train[as.numeric(rownames(datos_train_sev))] * pred_rf_sev_train
prima_rf_test_sev <- pred_rf_freq_test[as.numeric(rownames(datos_test_sev))] * pred_rf_sev_test

# Métricas prima pura RF
rmse_prima_rf_train <- sqrt(mean((datos_train_sev$costo_total - prima_rf_train_sev)^2))
mae_prima_rf_train <- mean(abs(datos_train_sev$costo_total - prima_rf_train_sev))
corr_prima_rf_train <- cor(datos_train_sev$costo_total, prima_rf_train_sev, use="complete.obs")

rmse_prima_rf_test <- sqrt(mean((datos_test_sev$costo_total - prima_rf_test_sev)^2))
mae_prima_rf_test <- mean(abs(datos_test_sev$costo_total - prima_rf_test_sev))
corr_prima_rf_test <- cor(datos_test_sev$costo_total, prima_rf_test_sev, use="complete.obs")

cat("Prima Pura Random Forest (Test):\n")
cat("  RMSE:", round(rmse_prima_rf_test, 2), "\n")
cat("  MAE:", round(mae_prima_rf_test, 2), "\n")
cat("  Correlación:", round(corr_prima_rf_test, 4), "\n")

# ============================================================================
# PASO 10: COMPARACIÓN GLM
# ============================================================================

cat("\n========== PASO 10: MÉTRICAS GLM EN DATOS CON FRECUENCIA > 0 ==========\n")

pred_glm_freq_train_sev <- pred_glm_freq_train[as.numeric(rownames(datos_train_sev))]
pred_glm_sev_train_sev <- pred_glm_sev_train[as.numeric(rownames(datos_train_sev))]
prima_glm_train_sev <- pred_glm_freq_train_sev * pred_glm_sev_train_sev

pred_glm_freq_test_sev <- pred_glm_freq_test[as.numeric(rownames(datos_test_sev))]
pred_glm_sev_test_sev <- pred_glm_sev_test[as.numeric(rownames(datos_test_sev))]
prima_glm_test_sev <- pred_glm_freq_test_sev * pred_glm_sev_test_sev

rmse_prima_glm_train <- sqrt(mean((datos_train_sev$costo_total - prima_glm_train_sev)^2))
mae_prima_glm_train <- mean(abs(datos_train_sev$costo_total - prima_glm_train_sev))
corr_prima_glm_train <- cor(datos_train_sev$costo_total, prima_glm_train_sev, use="complete.obs")

rmse_prima_glm_test <- sqrt(mean((datos_test_sev$costo_total - prima_glm_test_sev)^2))
mae_prima_glm_test <- mean(abs(datos_test_sev$costo_total - prima_glm_test_sev))
corr_prima_glm_test <- cor(datos_test_sev$costo_total, prima_glm_test_sev, use="complete.obs")

cat("Prima Pura GLM (Test):\n")
cat("  RMSE:", round(rmse_prima_glm_test, 2), "\n")
cat("  MAE:", round(mae_prima_glm_test, 2), "\n")
cat("  Correlación:", round(corr_prima_glm_test, 4), "\n")

# ============================================================================
# PASO 11: IMPORTANCIA DE VARIABLES - RANDOM FOREST
# ============================================================================

cat("\n========== PASO 11: IMPORTANCIA DE VARIABLES - RANDOM FOREST ==========\n")

# Importancia RF frecuencia
importancia_rf_freq <- as.data.frame(rf_freq$variable.importance) %>%
  rownames_to_column("Variable") %>%
  rename(Importancia = `rf_freq$variable.importance`) %>%
  arrange(desc(Importancia)) %>%
  head(10)

cat("\nTop 10 Variables - Random Forest Frecuencia:\n")
print(importancia_rf_freq)

# Importancia RF severidad
importancia_rf_sev <- as.data.frame(rf_sev$variable.importance) %>%
  rownames_to_column("Variable") %>%
  rename(Importancia = `rf_sev$variable.importance`) %>%
  arrange(desc(Importancia)) %>%
  head(10)

cat("\nTop 10 Variables - Random Forest Severidad:\n")
print(importancia_rf_sev)

# ============================================================================
# PASO 12: TABLA COMPARATIVA FINAL
# ============================================================================

cat("\n========== PASO 12: TABLA COMPARATIVA FINAL ==========\n")

tabla_comparativa <- data.frame(
  Modelo = c("GLM", "Árbol de Decisión", "Random Forest"),
  RMSE = c(round(rmse_prima_glm_test, 2),
           round(rmse_prima_arbol_test, 2),
           round(rmse_prima_rf_test, 2)),
  MAE = c(round(mae_prima_glm_test, 2),
          round(mae_prima_arbol_test, 2),
          round(mae_prima_rf_test, 2)),
  Correlacion = c(round(corr_prima_glm_test, 4),
                  round(corr_prima_arbol_test, 4),
                  round(corr_prima_rf_test, 4)),
  Interpretabilidad = c("Alta", "Alta", "Baja"),
  Velocidad = c("Muy Rápido", "Rápido", "Moderado")
)

cat("\n--- COMPARATIVA DE MODELOS ---\n")
print(tabla_comparativa)

# ============================================================================
# PASO 13: REPORTE EJECUTIVO
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("REPORTE EJECUTIVO - COMPARACIÓN DE METODOLOGÍAS\n")
cat(strrep("=", 80), "\n")

# Determinar mejor modelo
mejor_rmse <- which.min(tabla_comparativa$RMSE)
mejor_modelo <- tabla_comparativa$Modelo[mejor_rmse]

cat("\n1. DESEMPEÑO PREDICTIVO\n")
cat("   └─ Mejor modelo (RMSE):", mejor_modelo, "\n")
cat("      • RMSE:", tabla_comparativa$RMSE[mejor_rmse], "\n")
cat("      • MAE:", tabla_comparativa$MAE[mejor_rmse], "\n")
cat("      • Correlación:", tabla_comparativa$Correlacion[mejor_rmse], "\n")

cat("\n2. DIFERENCIAS ENTRE METODOLOGÍAS\n")
cat("   GLM:\n")
cat("   ├─ Componentes separados (frecuencia × severidad)\n")
cat("   ├─ Interpretabilidad directa de coeficientes\n")
cat("   ├─ Supuestos distribucionales (Poisson, Gamma, Lognormal)\n")
cat("   └─ RMSE Test:", tabla_comparativa$RMSE[1], "\n")

cat("\n   Árbol de Decisión:\n")
cat("   ├─ Decisiones binarias en nodos\n")
cat("   ├─ Visualizable y explicable\n")
cat("   ├─ No requiere transformaciones\n")
cat("   └─ RMSE Test:", tabla_comparativa$RMSE[2], "\n")

cat("\n   Random Forest:\n")
cat("   ├─ Ensamble de 200 árboles\n")
cat("   ├─ Captura interacciones complejas\n")
cat("   ├─ Mayor poder predictivo típicamente\n")
cat("   └─ RMSE Test:", tabla_comparativa$RMSE[3], "\n")

cat("\n3. VENTAJAS Y DESVENTAJAS\n")
cat("   GLM:\n")
cat("   ✓ Muy interpretable\n")
cat("   ✓ Rápido de entrenar\n")
cat("   ✓ Teoría estadística sólida\n")
cat("   ✗ Puede ser restrictivo\n")
cat("   ✗ No captura bien no-linealidades\n")

cat("\n   Árbol:\n")
cat("   ✓ Fácil de interpretar\n")
cat("   ✓ Maneja variables categóricas bien\n")
cat("   ✓ Rápido de predecir\n")
cat("   ✗ Menor poder predictivo\n")
cat("   ✗ Puede overfitear\n")

cat("\n   Random Forest:\n")
cat("   ✓ Mayor poder predictivo\n")
cat("   ✓ Maneja no-linealidades\n")
cat("   ✓ Robusto a outliers\n")
cat("   ✗ Caja negra\n")
cat("   ✗ Más lento de entrenar\n")

cat("\n4. INTERPRETABILIDAD\n")
cat("   GLM: ★★★★★ (Máxima - coeficientes directos)\n")
cat("   Árbol: ★★★★☆ (Alta - árbol visualizable)\n")
cat("   RF: ★★☆☆☆ (Baja - importancia de variables)\n")

cat("\n5. CONCLUSIONES PRELIMINARES\n")
cat("   • Para tarificación recomendamos:", mejor_modelo, "\n")
cat("   • Considera GLM como línea base por interpretabilidad\n")
cat("   • Random Forest para validación y benchmarking\n")
cat("   • Árbol como compromiso interpretabilidad-predicción\n")

cat("\n", strrep("=", 80), "\n")

# ============================================================================
# PASO 14: GUARDAR RESULTADOS
# ============================================================================

cat("\n========== PASO 14: GUARDANDO RESULTADOS ==========\n")

# Tabla comparativa
write_xlsx(tabla_comparativa, "comparativa_modelos.xlsx")
cat("✓ Tabla comparativa guardada: comparativa_modelos.xlsx\n")

# Importancia de variables
importancia_lista <- list(
  "RF_Frecuencia_Top10" = importancia_rf_freq,
  "RF_Severidad_Top10" = importancia_rf_sev
)

write_xlsx(importancia_lista, "importancia_variables_rf.xlsx")
cat("✓ Importancia de variables guardada: importancia_variables_rf.xlsx\n")

# Resumen de métricas por modelo
metricas_resumen <- data.frame(
  Modelo = c("GLM", "Árbol", "Random Forest"),
  RMSE_Frecuencia_Test = c(round(rmse_glm_freq_test, 4), 
                            round(rmse_arbol_freq_test, 4),
                            round(rmse_rf_freq_test, 4)),
  MAE_Frecuencia_Test = c(round(mae_glm_freq_test, 4),
                           round(mae_arbol_freq_test, 4),
                           round(mae_rf_freq_test, 4)),
  RMSE_Severidad_Test = c(round(rmse_glm_sev_test, 4),
                           round(rmse_arbol_sev_test, 4),
                           round(rmse_rf_sev_test, 4)),
  MAE_Severidad_Test = c(round(mae_glm_sev_test, 4),
                          round(mae_arbol_sev_test, 4),
                          round(mae_rf_sev_test, 4)),
  RMSE_Prima_Test = c(round(rmse_prima_glm_test, 2),
                      round(rmse_prima_arbol_test, 2),
                      round(rmse_prima_rf_test, 2)),
  MAE_Prima_Test = c(round(mae_prima_glm_test, 2),
                     round(mae_prima_arbol_test, 2),
                     round(mae_prima_rf_test, 2))
)

write_xlsx(metricas_resumen, "metricas_detalladas_modelos.xlsx")
cat("✓ Métricas detalladas guardadas: metricas_detalladas_modelos.xlsx\n")

# Guardar árboles podados
saveRDS(arbol_freq_podado, "arbol_frecuencia_podado.rds")
saveRDS(arbol_sev_podado, "arbol_severidad_podado.rds")
cat("✓ Árboles guardados: arbol_frecuencia_podado.rds, arbol_severidad_podado.rds\n")

# Guardar RF
saveRDS(rf_freq, "rf_frecuencia.rds")
saveRDS(rf_sev, "rf_severidad.rds")
cat("✓ Random Forests guardados: rf_frecuencia.rds, rf_severidad.rds\n")

cat("\n✓ COMPARACIÓN DE METODOLOGÍAS COMPLETADA\n")
cat("\nArchivos generados:\n")
cat("1. comparativa_modelos.xlsx\n")
cat("2. importancia_variables_rf.xlsx\n")
cat("3. metricas_detalladas_modelos.xlsx\n")
cat("4. arbol_frecuencia_podado.rds\n")
cat("5. arbol_severidad_podado.rds\n")
cat("6. rf_frecuencia.rds\n")
cat("7. rf_severidad.rds\n")

# ============================================================================
# FIN DEL SCRIPT
# ============================================================================
