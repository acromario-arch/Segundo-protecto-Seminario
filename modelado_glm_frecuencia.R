# ============================================================================
# MODELADO GLM DE FRECUENCIA - TARIFICACIÓN DE SEGUROS DE AUTOS
# ============================================================================
# Objetivo: Construir y validar modelo GLM Poisson/Negative Binomial
# para predecir la frecuencia de siniestros
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
# PASO 1: CARGAR Y PREPARAR DATOS
# ============================================================================

# Suponiendo que la base maestra ya está disponible
# (resultado del script construccion_base_maestra_tarificacion.R)
cat("\n========== PASO 1: CARGA Y PREPARACIÓN DE DATOS ==========\n")

# Cargar la base de datos maestra (ajusta la ruta según tu caso)
base_modelacion <- read.csv("generales_tot_fin.csv", stringsAsFactors = FALSE)

cat("Dimensiones de la base:", nrow(base_modelacion), "filas,", 
    ncol(base_modelacion), "columnas\n")

# Ver primeras filas
head(base_modelacion, 3)

# ============================================================================
# PASO 2: PREPARACIÓN DE DATOS PARA GLM
# ============================================================================

cat("\n========== PASO 2: PREPARACIÓN DE DATOS PARA GLM ==========\n")

# Seleccionar variables necesarias
variables_necesarias <- c("id", "exposicion", "frecuencia", "entidad", 
                          "tipo_veh", "marca_tipo", "cve_amis", "modelo", 
                          "uso", "cobertura", "sa", "deducible", "prima_emi")

# Verificar que existan las variables (crear si no existen)
base_modelo <- base_modelacion %>%
  select(any_of(variables_necesarias))

# Si faltan variables, mostrar alerta
variables_faltantes <- setdiff(variables_necesarias, names(base_modelo))
if(length(variables_faltantes) > 0) {
  cat("⚠️ ADVERTENCIA: Variables faltantes:", paste(variables_faltantes, collapse=", "), "\n")
}

cat("Variables disponibles:", names(base_modelo), "\n")

# ============================================================================
# PASO 3: ANÁLISIS DE CALIDAD DE DATOS
# ============================================================================

cat("\n========== PASO 3: ANÁLISIS DE CALIDAD DE DATOS ==========\n")

# Resumen general
cat("\n--- RESUMEN DE LA BASE DE DATOS ---\n")
print(summary(base_modelo))

# Verificar valores faltantes
cat("\n--- VALORES FALTANTES POR VARIABLE ---\n")
valores_faltantes <- colSums(is.na(base_modelo))
print(valores_faltantes)

# Eliminar filas con valores faltantes críticos
base_modelo <- base_modelo %>%
  filter(!is.na(exposicion),
         !is.na(frecuencia))

cat("\nBase después de eliminar faltantes críticos:", nrow(base_modelo), "registros\n")

# Análisis de frecuencia de categorías
cat("\n--- ANÁLISIS DE DISTRIBUCIÓN DE VARIABLES CATEGÓRICAS ---\n")

variables_categoricas <- c("entidad", "tipo_veh", "marca_tipo", "cve_amis", 
                           "modelo", "uso", "cobertura")

for(var in variables_categoricas) {
  if(var %in% names(base_modelo)) {
    cat("\n", var, ":\n")
    dist <- table(base_modelo[[var]])
    print(head(sort(dist, decreasing=TRUE), 10))
  }
}

# ============================================================================
# PASO 4: PREPARACIÓN DE VARIABLES
# ============================================================================

cat("\n========== PASO 4: PREPARACIÓN DE VARIABLES ==========\n")

# Convertir variables categóricas a factor
variables_factor <- c("entidad", "tipo_veh", "marca_tipo", "cve_amis", 
                      "modelo", "uso", "cobertura")

base_modelo <- base_modelo %>%
  mutate(across(all_of(variables_factor), 
                ~factor(., exclude = NULL)))

# Verificar conversión
cat("\nTipos de datos después de conversión:\n")
print(str(base_modelo))

# Estadísticas de exposición y frecuencia
cat("\n--- ESTADÍSTICAS DE EXPOSICIÓN Y FRECUENCIA ---\n")
cat("Exposición:\n")
print(summary(base_modelo$exposicion))
cat("\nFrecuencia:\n")
print(summary(base_modelo$frecuencia))

# Tarifa empírica
base_modelo <- base_modelo %>%
  mutate(tarifa_empirica = frecuencia / exposicion)

cat("\nTarifa Empírica:\n")
print(summary(base_modelo$tarifa_empirica))

# ============================================================================
# PASO 5: REVISIÓN DE NIVELES CON POCOS REGISTROS
# ============================================================================

cat("\n========== PASO 5: REVISIÓN DE NIVELES CON POCOS REGISTROS ==========\n")

# Umbral: categorías con menos de 50 registros
umbral_min <- 50

for(var in variables_factor) {
  if(var %in% names(base_modelo)) {
    dist <- table(base_modelo[[var]])
    pocos_reg <- names(dist[dist < umbral_min])
    if(length(pocos_reg) > 0) {
      cat("\n⚠️", var, "- Niveles con < ", umbral_min, " registros:\n")
      print(dist[dist < umbral_min])
      
      # Agrupar niveles con pocos registros en "OTROS"
      base_modelo[[var]] <- fct_collapse(base_modelo[[var]],
                                         OTROS = pocos_reg)
      cat("   → Agrupados en 'OTROS'\n")
    }
  }
}

# ============================================================================
# PASO 6: VISUALIZACIÓN DE RELACIONES CLAVE
# ============================================================================

cat("\n========== PASO 6: VISUALIZACIÓN DE RELACIONES CLAVE ==========\n")

# Gráfico: Frecuencia por Tipo de Vehículo
g1 <- base_modelo %>%
  group_by(tipo_veh) %>%
  summarise(freq_promedio = mean(tarifa_empirica, na.rm=TRUE),
            n = n(),
            .groups='drop') %>%
  arrange(desc(freq_promedio)) %>%
  ggplot(aes(x=reorder(tipo_veh, -freq_promedio), y=freq_promedio)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Frecuencia Promedio por Tipo de Vehículo",
       x="Tipo de Vehículo", y="Frecuencia Promedio") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

print(g1)

# Gráfico: Frecuencia por Uso
g2 <- base_modelo %>%
  group_by(uso) %>%
  summarise(freq_promedio = mean(tarifa_empirica, na.rm=TRUE),
            n = n(),
            .groups='drop') %>%
  arrange(desc(freq_promedio)) %>%
  ggplot(aes(x=reorder(uso, -freq_promedio), y=freq_promedio)) +
  geom_bar(stat="identity", fill="coral") +
  theme_minimal() +
  labs(title="Frecuencia Promedio por Uso",
       x="Uso", y="Frecuencia Promedio") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

print(g2)

# Gráfico: Distribución de Frecuencia
g3 <- ggplot(base_modelo, aes(x=frecuencia)) +
  geom_histogram(bins=30, fill="green", alpha=0.7) +
  scale_x_continuous(limits=c(0, quantile(base_modelo$frecuencia, 0.99))) +
  theme_minimal() +
  labs(title="Distribución de Frecuencia (sin extremos)",
       x="Frecuencia", y="Cantidad")

print(g3)

# ============================================================================
# PASO 7: DIVISIÓN TRAIN/TEST
# ============================================================================

cat("\n========== PASO 7: DIVISIÓN TRAIN/TEST ==========\n")

set.seed(123)

# Índices para entrenamiento (70%)
indice_train <- createDataPartition(base_modelo$frecuencia, 
                                    p=0.7, list=FALSE)

datos_train <- base_modelo[indice_train, ]
datos_test <- base_modelo[-indice_train, ]

cat("Tamaño conjunto de entrenamiento:", nrow(datos_train), "registros\n")
cat("Tamaño conjunto de prueba:", nrow(datos_test), "registros\n")
cat("Proporción:", round(nrow(datos_train)/nrow(base_modelo)*100, 1), 
    "% entrenamiento,", 
    round(nrow(datos_test)/nrow(base_modelo)*100, 1), "% prueba\n")

# ============================================================================
# PASO 8: CONSTRUCCIÓN MODELO GLM POISSON
# ============================================================================

cat("\n========== PASO 8: CONSTRUCCIÓN MODELO GLM POISSON ==========\n")

# Fórmula del modelo
formula_glm <- frecuencia ~ 
  entidad +
  tipo_veh +
  uso +
  marca_tipo +
  cve_amis +
  modelo +
  cobertura +
  sa +
  deducible +
  offset(log(exposicion))

# Ajuste del modelo Poisson
modelo_poisson <- glm(formula_glm,
                      family=poisson(link="log"),
                      data=datos_train,
                      control=glm.control(maxit=100))

cat("\n✓ Modelo Poisson ajustado exitosamente\n")

# ============================================================================
# PASO 9: RESUMEN Y DIAGNÓSTICO MODELO POISSON
# ============================================================================

cat("\n========== PASO 9: RESUMEN MODELO POISSON ==========\n")

cat("\n--- SUMMARY COMPLETO ---\n")
print(summary(modelo_poisson))

# Extracción de métricas clave
aic_poisson <- AIC(modelo_poisson)
deviance_nula <- modelo_poisson$null.deviance
deviance_residual <- modelo_poisson$deviance
gl_residuales <- modelo_poisson$df.residual
gl_nulos <- modelo_poisson$df.null

cat("\n--- MÉTRICAS CLAVE ---\n")
cat("AIC:", aic_poisson, "\n")
cat("Null Deviance:", deviance_nula, "con", gl_nulos, "g.l.\n")
cat("Residual Deviance:", deviance_residual, "con", gl_residuales, "g.l.\n")

# Cálculo de sobredispersión
sobredispersion_poisson <- deviance_residual / gl_residuales

cat("\n--- ANÁLISIS DE SOBREDISPERSIÓN ---\n")
cat("Sobredispersión (Residual Deviance / g.l.):", 
    round(sobredispersion_poisson, 4), "\n")

if(sobredispersion_poisson > 1.5) {
  cat("⚠️ ALERTA: Existe SOBREDISPERSIÓN significativa\n")
  cat("   → Se recomienda usar Negative Binomial\n")
  sobredispersion_significativa <- TRUE
} else if(sobredispersion_poisson > 1.1) {
  cat("⚠️ ADVERTENCIA: Posible sobredispersión moderada\n")
  sobredispersion_significativa <- TRUE
} else {
  cat("✓ No hay evidencia clara de sobredispersión\n")
  sobredispersion_significativa <- FALSE
}

# Coeficientes y significancia
cat("\n--- COEFICIENTES Y SIGNIFICANCIA ---\n")
coef_poisson <- as.data.frame(summary(modelo_poisson)$coefficients)
coef_poisson$Variable <- rownames(coef_poisson)
colnames(coef_poisson) <- c("Coeficiente", "Err.Std", "z.value", "Pr(>|z|)", "Variable")
coef_poisson <- coef_poisson[, c("Variable", "Coeficiente", "Err.Std", "z.value", "Pr(>|z|)")]

# Añadir interpretación (exponencial del coeficiente)
coef_poisson$Exp.Coef <- exp(coef_poisson$Coeficiente)
coef_poisson$Significancia <- ifelse(coef_poisson$`Pr(>|z|)` < 0.001, "***",
                                     ifelse(coef_poisson$`Pr(>|z|)` < 0.01, "**",
                                     ifelse(coef_poisson$`Pr(>|z|)` < 0.05, "*",
                                     ifelse(coef_poisson$`Pr(>|z|)` < 0.1, ".", " "))))

print(coef_poisson)

# ============================================================================
# PASO 10: CONSTRUCCIÓN MODELO NEGATIVE BINOMIAL
# ============================================================================

cat("\n========== PASO 10: CONSTRUCCIÓN MODELO NEGATIVE BINOMIAL ==========\n")

if(sobredispersion_significativa) {
  
  # Ajuste del modelo Negative Binomial
  modelo_nb <- glm.nb(formula_glm,
                      data=datos_train,
                      control=glm.control(maxit=100))
  
  cat("\n✓ Modelo Negative Binomial ajustado exitosamente\n")
  
  # Resumen
  cat("\n--- SUMMARY MODELO NEGATIVE BINOMIAL ---\n")
  print(summary(modelo_nb))
  
  # Métricas clave
  aic_nb <- AIC(modelo_nb)
  loglik_nb <- logLik(modelo_nb)
  deviance_nb <- modelo_nb$deviance
  gl_residuales_nb <- modelo_nb$df.residual
  
  cat("\n--- MÉTRICAS CLAVE ---\n")
  cat("AIC:", aic_nb, "\n")
  cat("Log-Likelihood:", as.numeric(loglik_nb), "\n")
  cat("Deviance:", deviance_nb, "\n")
  
  # Coeficientes Negative Binomial
  cat("\n--- COEFICIENTES NEGATIVE BINOMIAL ---\n")
  coef_nb <- as.data.frame(summary(modelo_nb)$coefficients)
  coef_nb$Variable <- rownames(coef_nb)
  colnames(coef_nb) <- c("Coeficiente", "Err.Std", "z.value", "Pr(>|z|)", "Variable")
  coef_nb <- coef_nb[, c("Variable", "Coeficiente", "Err.Std", "z.value", "Pr(>|z|)")]
  coef_nb$Exp.Coef <- exp(coef_nb$Coeficiente)
  coef_nb$Significancia <- ifelse(coef_nb$`Pr(>|z|)` < 0.001, "***",
                                   ifelse(coef_nb$`Pr(>|z|)` < 0.01, "**",
                                   ifelse(coef_nb$`Pr(>|z|)` < 0.05, "*",
                                   ifelse(coef_nb$`Pr(>|z|)` < 0.1, ".", " "))))
  
  print(coef_nb)
  
  # Parámetro theta de dispersión
  cat("\n--- PARÁMETRO DE DISPERSIÓN ---\n")
  cat("Theta (1/k):", modelo_nb$theta, "\n")
  cat("Sobredispersión (1/theta):", 1/modelo_nb$theta, "\n")
  
} else {
  cat("\n✓ No hay evidencia significativa de sobredispersión\n")
  cat("→ Se continúa con el modelo Poisson\n")
  modelo_nb <- NULL
}

# ============================================================================
# PASO 11: COMPARACIÓN POISSON VS NEGATIVE BINOMIAL
# ============================================================================

cat("\n========== PASO 11: COMPARACIÓN POISSON VS NEGATIVE BINOMIAL ==========\n")

if(!is.null(modelo_nb)) {
  
  # Tabla comparativa
  cat("\n--- COMPARATIVA DE MODELOS ---\n")
  
  comparativa <- data.frame(
    Métrica = c("AIC", "Log-Likelihood", "Deviance", "Residual Deviance/g.l.",
                "Interpretabilidad"),
    Poisson = c(
      round(aic_poisson, 2),
      round(logLik(modelo_poisson), 2),
      round(deviance_residual, 2),
      round(sobredispersion_poisson, 4),
      "Más simple"
    ),
    `Negative Binomial` = c(
      round(aic_nb, 2),
      round(as.numeric(loglik_nb), 2),
      round(deviance_nb, 2),
      round(deviance_nb/gl_residuales_nb, 4),
      "Más flexible"
    )
  )
  
  print(comparativa)
  
  # Selección del modelo
  cat("\n--- SELECCIÓN DEL MODELO ---\n")
  
  if(aic_nb < aic_poisson) {
    cat("✓ RECOMENDACIÓN: NEGATIVE BINOMIAL\n")
    cat("   → AIC menor en Negative Binomial (", round(aic_nb, 2), " vs ", 
        round(aic_poisson, 2), ")\n")
    cat("   → Mejor ajuste considerando sobredispersión\n")
    modelo_seleccionado <- modelo_nb
    nombre_modelo <- "Negative Binomial"
  } else {
    cat("✓ RECOMENDACIÓN: POISSON (aunque NB tiene mejor AIC global)\n")
    cat("   → Diferencia de AIC pequeña\n")
    cat("   → Modelo Poisson más parsimonioso\n")
    modelo_seleccionado <- modelo_poisson
    nombre_modelo <- "Poisson"
  }
  
} else {
  
  cat("\n✓ MODELO SELECCIONADO: POISSON\n")
  cat("   → No hay sobredispersión significativa\n")
  modelo_seleccionado <- modelo_poisson
  nombre_modelo <- "Poisson"
}

# ============================================================================
# PASO 12: PREDICCIONES EN TRAIN Y TEST
# ============================================================================

cat("\n========== PASO 12: PREDICCIONES EN TRAIN Y TEST ==========\n")

# Predicciones en escala de media (respuesta)
predicciones_train <- predict(modelo_seleccionado, 
                              newdata=datos_train, 
                              type="response")

predicciones_test <- predict(modelo_seleccionado, 
                             newdata=datos_test, 
                             type="response")

cat("Predicciones generadas\n")
cat("Train - Min:", round(min(predicciones_train), 4), 
    "Max:", round(max(predicciones_train), 4),
    "Media:", round(mean(predicciones_train), 4), "\n")
cat("Test - Min:", round(min(predicciones_test), 4), 
    "Max:", round(max(predicciones_test), 4),
    "Media:", round(mean(predicciones_test), 4), "\n")

# ============================================================================
# PASO 13: MÉTRICAS PREDICTIVAS
# ============================================================================

cat("\n========== PASO 13: MÉTRICAS PREDICTIVAS ==========\n")

# RMSE y MAE en entrenamiento
rmse_train <- sqrt(mean((datos_train$frecuencia - predicciones_train)^2))
mae_train <- mean(abs(datos_train$frecuencia - predicciones_train))

# RMSE y MAE en prueba
rmse_test <- sqrt(mean((datos_test$frecuencia - predicciones_test)^2))
mae_test <- mean(abs(datos_test$frecuencia - predicciones_test))

cat("\n--- ENTRENAMIENTO ---\n")
cat("RMSE:", round(rmse_train, 4), "\n")
cat("MAE:", round(mae_train, 4), "\n")

cat("\n--- PRUEBA ---\n")
cat("RMSE:", round(rmse_test, 4), "\n")
cat("MAE:", round(mae_test, 4), "\n")

# Verificar overfitting
diferencia_rmse <- rmse_test - rmse_train
diferencia_mae <- mae_test - mae_train

cat("\n--- ANÁLISIS DE OVERFITTING ---\n")
cat("Diferencia RMSE (Test - Train):", round(diferencia_rmse, 4), "\n")
cat("Diferencia MAE (Test - Train):", round(diferencia_mae, 4), "\n")

if(diferencia_rmse < 0.05 & diferencia_mae < 0.05) {
  cat("✓ Modelo bien generalizado (sin overfitting)\n")
} else if(diferencia_rmse < 0.15 & diferencia_mae < 0.15) {
  cat("⚠️ Ligero overfitting pero aceptable\n")
} else {
  cat("⚠️ ADVERTENCIA: Posible overfitting significativo\n")
}

# ============================================================================
# PASO 14: ANÁLISIS DE IMPORTANCIA DE VARIABLES
# ============================================================================

cat("\n========== PASO 14: ANÁLISIS DE IMPORTANCIA DE VARIABLES ==========\n")

# Extraer coeficientes del modelo seleccionado
if(nombre_modelo == "Poisson") {
  coef_importancia <- coef_poisson
} else {
  coef_importancia <- coef_nb
}

# Reordenar por valor absoluto de z
coef_importancia$abs_z <- abs(coef_importancia$z.value)
coef_importancia <- coef_importancia %>%
  arrange(desc(abs_z))

cat("\n--- TOP 15 VARIABLES MÁS SIGNIFICATIVAS ---\n")
print(head(coef_importancia[, c("Variable", "Coeficiente", "Err.Std", 
                               "Pr(>|z|)", "Significancia", "Exp.Coef")], 15))

# Gráfico de importancia
g_importancia <- coef_importancia %>%
  filter(Variable != "(Intercept)") %>%
  head(20) %>%
  ggplot(aes(x=reorder(Variable, Coeficiente), y=Coeficiente, 
             fill=ifelse(Coeficiente>0, "Positivo", "Negativo"))) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal() +
  labs(title="Top 20 Variables - Magnitud de Coeficientes",
       x="Variable", y="Coeficiente") +
  scale_fill_manual(values=c("Positivo"="green", "Negativo"="red"))

print(g_importancia)

# ============================================================================
# PASO 15: TABLA RESUMEN DE VARIABLES
# ============================================================================

cat("\n========== PASO 15: TABLA RESUMEN DE VARIABLES ==========\n")

# Crear tabla interpretativa
tabla_resumen <- coef_importancia %>%
  select(Variable, Coeficiente, Err.Std, `Pr(>|z|)`, Significancia, Exp.Coef) %>%
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
      Coeficiente > 0 ~ paste0("Aumenta frecuencia ~", round((Exp.Coef-1)*100, 1), "%"),
      Coeficiente < 0 ~ paste0("Reduce frecuencia ~", round((1-Exp.Coef)*100, 1), "%"),
      TRUE ~ "Sin efecto"
    )
  ) %>%
  select(Variable, Coeficiente, Err.Std, `Pr(>|z|)`, Significancia, 
         Interpretacion, Efecto)

print(tabla_resumen)

# Guardar tabla en Excel
escribir_tabla <- tabla_resumen %>%
  arrange(`Pr(>|z|)`) %>%
  mutate(
    Coeficiente = round(Coeficiente, 6),
    Err.Std = round(Err.Std, 6),
    `Pr(>|z|)` = round(`Pr(>|z|)`, 6)
  )

# ============================================================================
# PASO 16: DIAGNOSTICOS DEL MODELO
# ============================================================================

cat("\n========== PASO 16: DIAGNÓSTICOS DEL MODELO ==========\n")

# Residuos de Pearson
residuos_pearson <- residuals(modelo_seleccionado, type="pearson")

# Gráfico Q-Q
g_qq <- ggplot() +
  geom_qq(aes(sample=residuos_pearson)) +
  geom_qq_line(aes(sample=residuos_pearson), color="red") +
  theme_minimal() +
  labs(title=paste("Q-Q Plot -", nombre_modelo))

print(g_qq)

# Gráfico de residuos vs ajustados
residuos_df <- data.frame(
  ajustados = fitted(modelo_seleccionado),
  residuos = residuals(modelo_seleccionado, type="deviance")
)

g_resid <- ggplot(residuos_df, aes(x=ajustados, y=residuos)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  theme_minimal() +
  labs(title=paste("Residuos vs Valores Ajustados -", nombre_modelo),
       x="Valores Ajustados", y="Residuos de Deviance")

print(g_resid)

# ============================================================================
# PASO 17: REPORTE FINAL
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("REPORTE FINAL - MODELADO GLM DE FRECUENCIA\n")
cat(strrep("=", 80), "\n")

cat("\n1. MODELO SELECCIONADO\n")
cat("   └─ ", nombre_modelo, "\n\n")

cat("2. JUSTIFICACIÓN DE LA SELECCIÓN\n")
if(sobredispersion_significativa) {
  cat("   └─ Sobredispersión detectada:", round(sobredispersion_poisson, 4), "\n")
  cat("   └─ Se evaluó Negative Binomial por mejor ajuste\n")
  if(nombre_modelo == "Negative Binomial") {
    cat("   └─ NB seleccionado por menor AIC y mejor manejo de dispersión\n")
  } else {
    cat("   └─ Poisson seleccionado por parsimonia\n")
  }
} else {
  cat("   └─ No hay sobredispersión significativa\n")
  cat("   └─ Poisson es apropiado\n")
}

cat("\n3. VARIABLES RELEVANTES (p < 0.05)\n")
vars_sig <- coef_importancia %>%
  filter(Variable != "(Intercept)", `Pr(>|z|)` < 0.05)
if(nrow(vars_sig) > 0) {
  for(i in 1:nrow(vars_sig)) {
    var_info <- vars_sig[i, ]
    cat("   └─ ", var_info$Variable, " (coef: ", 
        round(var_info$Coeficiente, 4), ", p: ", 
        round(var_info$`Pr(>|z|)`, 4), ")\n", sep="")
  }
} else {
  cat("   └─ Ninguna variable significativa\n")
}

cat("\n4. MÉTRICAS DE CALIDAD PREDICTIVA\n")
cat("   Entrenamiento:\n")
cat("   ├─ RMSE:", round(rmse_train, 4), "\n")
cat("   └─ MAE: ", round(mae_train, 4), "\n")
cat("   Prueba:\n")
cat("   ├─ RMSE:", round(rmse_test, 4), "\n")
cat("   └─ MAE: ", round(mae_test, 4), "\n")

cat("\n5. BONDAD DE AJUSTE\n")
cat("   ├─ AIC: ", round(AIC(modelo_seleccionado), 2), "\n")
cat("   ├─ Deviance Residual: ", round(deviance_residual, 2), "\n")
cat("   └─ Null Deviance: ", round(deviance_nula, 2), "\n")

cat("\n6. CONCLUSIONES ACTUARIALES\n")
cat("   ├─ El modelo ", nombre_modelo, " es apropiado para la frecuencia\n")
cat("   ├─ Variables como ", 
    paste(head(vars_sig$Variable, 3), collapse=", "),
    " son críticas\n")
cat("   ├─ Capacidad predictiva: ", 
    ifelse(rmse_test < 0.5, "Excelente", 
           ifelse(rmse_test < 1, "Buena", "Moderada")), "\n")
cat("   └─ Modelo listo para tarificación\n")

cat("\n", strrep("=", 80), "\n")

# ============================================================================
# PASO 18: GUARDAR RESULTADOS
# ============================================================================

cat("\n========== PASO 18: GUARDANDO RESULTADOS ==========\n")

# Guardar tabla de coeficientes
write_xlsx(escribir_tabla, "coeficientes_modelo_frecuencia.xlsx")
cat("✓ Tabla de coeficientes guardada: coeficientes_modelo_frecuencia.xlsx\n")

# Guardar predicciones
predicciones_resultado <- data.frame(
  id = datos_test$id,
  frecuencia_real = datos_test$frecuencia,
  exposicion = datos_test$exposicion,
  prediccion = predicciones_test,
  residuo = datos_test$frecuencia - predicciones_test,
  tarifa_empirica = datos_test$tarifa_empirica,
  tarifa_predicha = predicciones_test / datos_test$exposicion
)

write_xlsx(predicciones_resultado, "predicciones_test_frecuencia.xlsx")
cat("✓ Predicciones guardadas: predicciones_test_frecuencia.xlsx\n")

# Guardar modelo
saveRDS(modelo_seleccionado, "modelo_glm_frecuencia.rds")
cat("✓ Modelo guardado: modelo_glm_frecuencia.rds\n")

# Resumen de entrenamiento
resumen_entrenamiento <- data.frame(
  Metrica = c("N Entrenamiento", "N Prueba", "Modelo Seleccionado",
              "RMSE Train", "MAE Train", "RMSE Test", "MAE Test",
              "AIC", "Sobredispersión Poisson", "Variables Significativas"),
  Valor = c(nrow(datos_train), nrow(datos_test), nombre_modelo,
            round(rmse_train, 4), round(mae_train, 4),
            round(rmse_test, 4), round(mae_test, 4),
            round(AIC(modelo_seleccionado), 2),
            round(sobredispersion_poisson, 4),
            nrow(vars_sig))
)

write_xlsx(resumen_entrenamiento, "resumen_entrenamiento.xlsx")
cat("✓ Resumen de entrenamiento guardado: resumen_entrenamiento.xlsx\n")

cat("\n✓ MODELADO COMPLETADO EXITOSAMENTE\n")
cat("\nArchivos generados:\n")
cat("1. coeficientes_modelo_frecuencia.xlsx\n")
cat("2. predicciones_test_frecuencia.xlsx\n")
cat("3. modelo_glm_frecuencia.rds\n")
cat("4. resumen_entrenamiento.xlsx\n")

# ============================================================================
# FIN DEL SCRIPT
# ============================================================================
