# ============================================================================
# CÁLCULO DE PRIMA PURA MEDIANTE GLM - TARIFICACIÓN DE SEGUROS DE AUTOS
# ============================================================================
# Objetivo: Construir prima pura combinando modelos GLM de frecuencia y severidad
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
# PASO 1: CARGAR DATOS Y MODELOS
# ============================================================================

cat("\n========== PASO 1: CARGA DE DATOS Y MODELOS ==========\n")

# Cargar la base de datos maestra
base_completa <- read.csv("generales_tot_fin.csv", stringsAsFactors = FALSE)

cat("Base de datos cargada:", nrow(base_completa), "registros\n")

# Cargar modelos GLM guardados
modelo_frecuencia <- readRDS("modelo_glm_frecuencia.rds")
modelo_severidad <- readRDS("modelo_glm_severidad.rds")

cat("✓ Modelo de frecuencia cargado\n")
cat("✓ Modelo de severidad cargado\n")

# ============================================================================
# PASO 2: PREPARACIÓN DE DATOS
# ============================================================================

cat("\n========== PASO 2: PREPARACIÓN DE DATOS ==========\n")

# Seleccionar variables necesarias
variables_necesarias <- c("id", "exposicion", "frecuencia", "severidad_promedio",
                          "costo_total", "entidad", "tipo_veh", "marca_tipo", 
                          "cve_amis", "modelo", "uso", "cobertura", "sa", 
                          "deducible", "prima_emi")

base_prima <- base_completa %>%
  select(any_of(variables_necesarias))

cat("Observaciones en base:", nrow(base_prima), "\n")

# Convertir variables categóricas a factor (igual que en entrenamiento)
variables_factor <- c("entidad", "tipo_veh", "marca_tipo", "cve_amis", 
                      "modelo", "uso", "cobertura")

base_prima <- base_prima %>%
  mutate(across(all_of(variables_factor), 
                ~factor(., exclude = NULL)))

# Agrupar niveles con pocos registros (siguiendo criterios de entrenamiento)
umbral_min <- 30

for(var in variables_factor) {
  if(var %in% names(base_prima)) {
    dist <- table(base_prima[[var]])
    pocos_reg <- names(dist[dist < umbral_min])
    if(length(pocos_reg) > 0) {
      base_prima[[var]] <- fct_collapse(base_prima[[var]],
                                        OTROS = pocos_reg)
    }
  }
}

cat("✓ Variables preparadas para predicción\n")

# ============================================================================
# PASO 3: GENERAR PREDICCIONES DE FRECUENCIA
# ============================================================================

cat("\n========== PASO 3: PREDICCIONES DE FRECUENCIA ==========\n")

# Predicciones de frecuencia esperada
frecuencia_esperada <- predict(modelo_frecuencia, 
                               newdata=base_prima, 
                               type="response")

cat("Predicciones de frecuencia generadas\n")
cat("  Min:", round(min(frecuencia_esperada), 4), "\n")
cat("  Max:", round(max(frecuencia_esperada), 4), "\n")
cat("  Media:", round(mean(frecuencia_esperada), 4), "\n")

# ============================================================================
# PASO 4: GENERAR PREDICCIONES DE SEVERIDAD
# ============================================================================

cat("\n========== PASO 4: PREDICCIONES DE SEVERIDAD ==========\n")

# Predicciones de severidad esperada
severidad_esperada <- predict(modelo_severidad, 
                              newdata=base_prima, 
                              type="response")

cat("Predicciones de severidad generadas\n")
cat("  Min:", round(min(severidad_esperada), 2), "\n")
cat("  Max:", round(max(severidad_esperada), 2), "\n")
cat("  Media:", round(mean(severidad_esperada), 2), "\n")

# ============================================================================
# PASO 5: CALCULAR PRIMA PURA GLM
# ============================================================================

cat("\n========== PASO 5: CÁLCULO DE PRIMA PURA GLM ==========\n")

# Prima pura = Frecuencia esperada × Severidad esperada
prima_pura_glm <- frecuencia_esperada * severidad_esperada

cat("Prima pura GLM calculada\n")
cat("  Min:", round(min(prima_pura_glm), 2), "\n")
cat("  Max:", round(max(prima_pura_glm), 2), "\n")
cat("  Media:", round(mean(prima_pura_glm), 2), "\n")
cat("  Mediana:", round(median(prima_pura_glm), 2), "\n")

# ============================================================================
# PASO 6: INCORPORAR PREDICCIONES A LA BASE
# ============================================================================

cat("\n========== PASO 6: INCORPORACIÓN DE PREDICCIONES ==========\n")

base_prima <- base_prima %>%
  mutate(
    frecuencia_observada = frecuencia,
    frecuencia_esperada = frecuencia_esperada,
    severidad_observada = severidad_promedio,
    severidad_esperada = severidad_esperada,
    costo_observado = costo_total,
    prima_pura_glm = prima_pura_glm,
    prima_emi_original = prima_emi
  ) %>%
  select(id, exposicion, 
         frecuencia_observada, frecuencia_esperada,
         severidad_observada, severidad_esperada,
         costo_observado, prima_pura_glm, prima_emi_original,
         entidad, tipo_veh, marca_tipo, cve_amis, modelo, 
         uso, cobertura, sa, deducible)

cat("✓ Base con predicciones completa\n")
cat("  Dimensiones:", nrow(base_prima), "×", ncol(base_prima), "\n")

# Ver primeras filas
head(base_prima)

# ============================================================================
# PASO 7: MÉTRICAS DE DESEMPEÑO GLOBAL
# ============================================================================

cat("\n========== PASO 7: MÉTRICAS DE DESEMPEÑO GLOBAL ==========\n")

# Calcular RMSE
rmse_prima <- sqrt(mean((base_prima$costo_observado - base_prima$prima_pura_glm)^2, na.rm=TRUE))

# Calcular MAE
mae_prima <- mean(abs(base_prima$costo_observado - base_prima$prima_pura_glm), na.rm=TRUE)

# Calcular MAPE (solo para registros con costo > 0)
base_mape <- base_prima %>%
  filter(costo_observado > 0)

mape_prima <- mean(abs(base_mape$costo_observado - base_mape$prima_pura_glm) / 
                   base_mape$costo_observado, na.rm=TRUE) * 100

# Correlación
corr_prima <- cor(base_prima$costo_observado, base_prima$prima_pura_glm, use="complete.obs")

# Suma de costos
suma_observada <- sum(base_prima$costo_observado, na.rm=TRUE)
suma_estimada <- sum(base_prima$prima_pura_glm, na.rm=TRUE)
desviacion_suma <- suma_estimada - suma_observada
desviacion_suma_pct <- (desviacion_suma / suma_observada) * 100

cat("\n--- MÉTRICAS DE BONDAD DE AJUSTE ---\n")
cat("RMSE:", round(rmse_prima, 2), "\n")
cat("MAE:", round(mae_prima, 2), "\n")
cat("MAPE:", round(mape_prima, 2), "%\n")
cat("Correlación:", round(corr_prima, 4), "\n")

cat("\n--- ANÁLISIS DE SUMA TOTAL ---\n")
cat("Suma observada:", format(suma_observada, big.mark=","), "\n")
cat("Suma estimada:", format(suma_estimada, big.mark=","), "\n")
cat("Desviación absoluta:", format(desviacion_suma, big.mark=","), "\n")
cat("Desviación %:", round(desviacion_suma_pct, 2), "%\n")

# Crear tabla de métricas
metricas_globales <- data.frame(
  Metrica = c("RMSE", "MAE", "MAPE (%)", "Correlación",
              "Suma Observada", "Suma Estimada", "Desviación %"),
  Valor = c(round(rmse_prima, 2), 
            round(mae_prima, 2),
            round(mape_prima, 2),
            round(corr_prima, 4),
            format(suma_observada, big.mark=","),
            format(suma_estimada, big.mark=","),
            round(desviacion_suma_pct, 2))
)

# ============================================================================
# PASO 8: GRÁFICOS DE DESEMPEÑO
# ============================================================================

cat("\n========== PASO 8: GRÁFICOS DE DESEMPEÑO ==========\n")

# Gráfico 1: Observado vs Estimado
g_obs_est <- ggplot(base_prima, aes(x=costo_observado, y=prima_pura_glm)) +
  geom_point(alpha=0.3, size=1) +
  geom_abline(intercept=0, slope=1, color="red", linetype="dashed") +
  scale_x_continuous(limits=c(0, quantile(base_prima$costo_observado, 0.95))) +
  scale_y_continuous(limits=c(0, quantile(base_prima$prima_pura_glm, 0.95))) +
  theme_minimal() +
  labs(title="Costo Observado vs Prima Pura GLM",
       x="Costo Observado ($)", y="Prima Pura GLM ($)") +
  annotate("text", x=Inf, y=-Inf, 
           label=paste("Corr:", round(corr_prima, 3)),
           hjust=1.1, vjust=-0.5, size=4)

print(g_obs_est)

# Gráfico 2: Distribución de prima_pura_glm
g_dist_prima <- ggplot(base_prima, aes(x=prima_pura_glm)) +
  geom_histogram(bins=50, fill="steelblue", alpha=0.7) +
  scale_x_continuous(limits=c(0, quantile(base_prima$prima_pura_glm, 0.95))) +
  theme_minimal() +
  labs(title="Distribución de Prima Pura GLM",
       x="Prima Pura GLM ($)", y="Frecuencia")

print(g_dist_prima)

# Gráfico 3: Boxplot de prima_pura_glm
g_box_prima <- ggplot(base_prima, aes(y=prima_pura_glm)) +
  geom_boxplot(fill="coral", alpha=0.7) +
  scale_y_continuous(limits=c(0, quantile(base_prima$prima_pura_glm, 0.99))) +
  theme_minimal() +
  labs(title="Boxplot de Prima Pura GLM",
       y="Prima Pura GLM ($)")

print(g_box_prima)

# Gráfico 4: Residuos
base_prima$residuos <- base_prima$costo_observado - base_prima$prima_pura_glm

g_residuos <- ggplot(base_prima, aes(x=prima_pura_glm, y=residuos)) +
  geom_point(alpha=0.3, size=1) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  scale_x_continuous(limits=c(0, quantile(base_prima$prima_pura_glm, 0.95))) +
  theme_minimal() +
  labs(title="Residuos vs Prima Pura Estimada",
       x="Prima Pura GLM ($)", y="Residuos ($)")

print(g_residuos)

# ============================================================================
# PASO 9: PERCENTILES DE PRIMA PURA
# ============================================================================

cat("\n========== PASO 9: PERCENTILES DE PRIMA PURA ==========\n")

percentiles_prima <- data.frame(
  Percentil = c("P10", "P25", "P50 (Mediana)", "P75", "P90", "P95", "P99"),
  Valor = c(
    quantile(base_prima$prima_pura_glm, 0.10),
    quantile(base_prima$prima_pura_glm, 0.25),
    quantile(base_prima$prima_pura_glm, 0.50),
    quantile(base_prima$prima_pura_glm, 0.75),
    quantile(base_prima$prima_pura_glm, 0.90),
    quantile(base_prima$prima_pura_glm, 0.95),
    quantile(base_prima$prima_pura_glm, 0.99)
  )
)

cat("\n--- PERCENTILES DE PRIMA PURA GLM ---\n")
print(percentiles_prima)

# ============================================================================
# PASO 10: ANÁLISIS POR SEGMENTOS
# ============================================================================

cat("\n========== PASO 10: ANÁLISIS POR SEGMENTOS ==========\n")

# Función para calcular resumen por segmento
calcular_resumen_segmento <- function(data, variable_seg) {
  
  resumen <- data %>%
    group_by(!!sym(variable_seg)) %>%
    summarise(
      n = n(),
      freq_obs_promedio = mean(frecuencia_observada, na.rm=TRUE),
      freq_est_promedio = mean(frecuencia_esperada, na.rm=TRUE),
      sev_obs_promedio = mean(severidad_observada, na.rm=TRUE),
      sev_est_promedio = mean(severidad_esperada, na.rm=TRUE),
      costo_obs_promedio = mean(costo_observado, na.rm=TRUE),
      prima_pura_promedio = mean(prima_pura_glm, na.rm=TRUE),
      prima_emi_promedio = mean(prima_emi_original, na.rm=TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      diferencia_absoluta = prima_pura_promedio - prima_emi_promedio,
      diferencia_porcentual = (diferencia_absoluta / prima_emi_promedio) * 100,
      desviacion = ifelse(diferencia_porcentual > 10, "Sobretarificado",
                          ifelse(diferencia_porcentual < -10, "Subtarificado",
                                 "Bien tarificado"))
    ) %>%
    arrange(desc(abs(diferencia_porcentual)))
  
  return(resumen)
}

# Segmento 1: ENTIDAD
cat("\n--- SEGMENTO: ENTIDAD ---\n")
segmento_entidad <- calcular_resumen_segmento(base_prima, "entidad")
print(head(segmento_entidad, 10))

# Segmento 2: COBERTURA
cat("\n--- SEGMENTO: COBERTURA ---\n")
segmento_cobertura <- calcular_resumen_segmento(base_prima, "cobertura")
print(segmento_cobertura)

# Segmento 3: TIPO DE VEHÍCULO
cat("\n--- SEGMENTO: TIPO DE VEHÍCULO ---\n")
segmento_tipo_veh <- calcular_resumen_segmento(base_prima, "tipo_veh")
print(segmento_tipo_veh)

# Segmento 4: USO
cat("\n--- SEGMENTO: USO ---\n")
segmento_uso <- calcular_resumen_segmento(base_prima, "uso")
print(segmento_uso)

# Segmento 5: MARCA_TIPO
cat("\n--- SEGMENTO: MARCA_TIPO ---\n")
segmento_marca_tipo <- calcular_resumen_segmento(base_prima, "marca_tipo")
print(head(segmento_marca_tipo, 10))

# Segmento 6: CVE_AMIS
cat("\n--- SEGMENTO: CVE_AMIS ---\n")
segmento_cve_amis <- calcular_resumen_segmento(base_prima, "cve_amis")
print(head(segmento_cve_amis, 10))

# ============================================================================
# PASO 11: IDENTIFICAR SEGMENTOS PROBLEMÁTICOS
# ============================================================================

cat("\n========== PASO 11: IDENTIFICAR SEGMENTOS PROBLEMÁTICOS ==========\n")

# Segmentos sobretarificados (prima GLM > prima emi en más de 10%)
cat("\n--- SEGMENTOS SOBRETARIFICADOS (GLM > EMI > 10%) ---\n")

sobretarificados_entidad <- segmento_entidad %>%
  filter(diferencia_porcentual > 10) %>%
  select(entidad, diferencia_porcentual, n)
print(sobretarificados_entidad)

# Segmentos subtarificados (prima GLM < prima emi en más de 10%)
cat("\n--- SEGMENTOS SUBTARIFICADOS (GLM < EMI > -10%) ---\n")

subtarificados_entidad <- segmento_entidad %>%
  filter(diferencia_porcentual < -10) %>%
  select(entidad, diferencia_porcentual, n)
print(subtarificados_entidad)

# Mayor desviación
cat("\n--- SEGMENTOS CON MAYOR DESVIACIÓN ABSOLUTA ---\n")

mayor_desviacion <- segmento_entidad %>%
  arrange(desc(abs(diferencia_porcentual))) %>%
  head(10) %>%
  select(entidad, diferencia_porcentual, prima_pura_promedio, prima_emi_promedio, n)
print(mayor_desviacion)

# ============================================================================
# PASO 12: ANÁLISIS DE PRECISIÓN POR SEGMENTOS
# ============================================================================

cat("\n========== PASO 12: ANÁLISIS DE PRECISIÓN POR SEGMENTOS ==========\n")

# Función para calcular métricas por segmento
metricas_por_segmento <- function(data, variable_seg) {
  
  metricas <- data %>%
    group_by(!!sym(variable_seg)) %>%
    summarise(
      n = n(),
      rmse = sqrt(mean((costo_observado - prima_pura_glm)^2, na.rm=TRUE)),
      mae = mean(abs(costo_observado - prima_pura_glm), na.rm=TRUE),
      correlacion = cor(costo_observado, prima_pura_glm, use="complete.obs"),
      .groups = 'drop'
    ) %>%
    arrange(desc(rmse))
  
  return(metricas)
}

# Precisión por cobertura
cat("\n--- PRECISIÓN POR COBERTURA ---\n")
precision_cobertura <- metricas_por_segmento(base_prima, "cobertura")
print(precision_cobertura)

# Precisión por tipo de vehículo
cat("\n--- PRECISIÓN POR TIPO DE VEHÍCULO ---\n")
precision_tipo_veh <- metricas_por_segmento(base_prima, "tipo_veh")
print(precision_tipo_veh)

# Precisión por uso
cat("\n--- PRECISIÓN POR USO ---\n")
precision_uso <- metricas_por_segmento(base_prima, "uso")
print(precision_uso)

# ============================================================================
# PASO 13: GRÁFICOS POR SEGMENTOS
# ============================================================================

cat("\n========== PASO 13: GRÁFICOS POR SEGMENTOS ==========\n")

# Gráfico 1: Prima pura por cobertura
g_prima_cobertura <- segmento_cobertura %>%
  ggplot(aes(x=reorder(cobertura, -prima_pura_promedio), y=prima_pura_promedio)) +
  geom_bar(stat="identity", fill="steelblue", alpha=0.7) +
  geom_hline(yintercept=mean(base_prima$prima_pura_glm), 
             color="red", linetype="dashed", label="Promedio General") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Prima Pura GLM Promedio por Cobertura",
       x="Cobertura", y="Prima Pura Promedio ($)")

print(g_prima_cobertura)

# Gráfico 2: Prima pura por tipo de vehículo
g_prima_tipo_veh <- segmento_tipo_veh %>%
  ggplot(aes(x=reorder(tipo_veh, -prima_pura_promedio), y=prima_pura_promedio)) +
  geom_bar(stat="identity", fill="coral", alpha=0.7) +
  geom_hline(yintercept=mean(base_prima$prima_pura_glm), 
             color="red", linetype="dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Prima Pura GLM Promedio por Tipo de Vehículo",
       x="Tipo de Vehículo", y="Prima Pura Promedio ($)")

print(g_prima_tipo_veh)

# Gráfico 3: Prima pura por uso
g_prima_uso <- segmento_uso %>%
  ggplot(aes(x=reorder(uso, -prima_pura_promedio), y=prima_pura_promedio)) +
  geom_bar(stat="identity", fill="green", alpha=0.7) +
  geom_hline(yintercept=mean(base_prima$prima_pura_glm), 
             color="red", linetype="dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Prima Pura GLM Promedio por Uso",
       x="Uso", y="Prima Pura Promedio ($)")

print(g_prima_uso)

# Gráfico 4: Desviación porcentual por cobertura
g_desviacion_cobertura <- segmento_cobertura %>%
  ggplot(aes(x=reorder(cobertura, diferencia_porcentual), 
             y=diferencia_porcentual,
             fill=ifelse(diferencia_porcentual>0, "Sobretarificado", "Subtarificado"))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0, color="black", size=0.5) +
  geom_hline(yintercept=10, color="red", linetype="dashed", alpha=0.5) +
  geom_hline(yintercept=-10, color="red", linetype="dashed", alpha=0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Desviación Porcentual GLM vs EMI por Cobertura",
       x="Cobertura", y="Desviación Porcentual (%)",
       fill="Tipo") +
  scale_fill_manual(values=c("Sobretarificado"="red", "Subtarificado"="blue"))

print(g_desviacion_cobertura)

# ============================================================================
# PASO 14: TABLA COMPARATIVA GLM VS EMI
# ============================================================================

cat("\n========== PASO 14: TABLA COMPARATIVA GLM VS EMI ==========\n")

# Por cobertura
tabla_comparativa_cobertura <- segmento_cobertura %>%
  select(cobertura, n, prima_pura_promedio, prima_emi_promedio, 
         diferencia_absoluta, diferencia_porcentual, desviacion) %>%
  arrange(diferencia_porcentual)

cat("\n--- COMPARATIVA GLM vs EMI POR COBERTURA ---\n")
print(tabla_comparativa_cobertura)

# Por tipo de vehículo
tabla_comparativa_tipo_veh <- segmento_tipo_veh %>%
  select(tipo_veh, n, prima_pura_promedio, prima_emi_promedio, 
         diferencia_absoluta, diferencia_porcentual, desviacion)

cat("\n--- COMPARATIVA GLM vs EMI POR TIPO DE VEHÍCULO ---\n")
print(tabla_comparativa_tipo_veh)

# ============================================================================
# PASO 15: REPORTE FINAL
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("REPORTE FINAL - PRIMA PURA MEDIANTE GLM\n")
cat(strrep("=", 80), "\n")

cat("\n1. DESEMPEÑO GLOBAL DE LA PRIMA PURA GLM\n")
cat("   ├─ RMSE:", round(rmse_prima, 2), "\n")
cat("   ├─ MAE:", round(mae_prima, 2), "\n")
cat("   ├─ MAPE:", round(mape_prima, 2), "%\n")
cat("   ├─ Correlación observado-estimado:", round(corr_prima, 4), "\n")
cat("   └─ Desviación en suma total:", round(desviacion_suma_pct, 2), "%\n")

cat("\n2. DISTRIBUCIÓN DE PRIMA PURA\n")
cat("   ├─ Mínimo:", format(round(min(base_prima$prima_pura_glm), 2), big.mark=","), "\n")
cat("   ├─ P25:", format(round(quantile(base_prima$prima_pura_glm, 0.25), 2), big.mark=","), "\n")
cat("   ├─ Mediana:", format(round(median(base_prima$prima_pura_glm), 2), big.mark=","), "\n")
cat("   ├─ Media:", format(round(mean(base_prima$prima_pura_glm), 2), big.mark=","), "\n")
cat("   ├─ P75:", format(round(quantile(base_prima$prima_pura_glm, 0.75), 2), big.mark=","), "\n")
cat("   └─ Máximo:", format(round(max(base_prima$prima_pura_glm), 2), big.mark=","), "\n")

cat("\n3. FORTALEZAS DEL MODELO GLM\n")
cat("   ├─ Interpretabilidad clara de coeficientes\n")
cat("   ├─ Separación de componentes: frecuencia y severidad\n")
cat("   ├─ Manejo apropiado de distribuciones sesgadas\n")
cat("   ├─ Correlación razonable:", round(corr_prima, 4), "\n")
cat("   └─ Línea base para comparación futura\n")

cat("\n4. LIMITACIONES DEL MODELO GLM\n")
cat("   ├─ Supuestos de linealidad en escala log\n")
cat("   ├─ No captura interacciones complejas\n")
cat("   ├─ MAPE:", round(mape_prima, 2), "% (puede mejorar)\n")
cat("   └─ Posibles outliers sin modelar\n")

cat("\n5. SEGMENTOS MEJOR EXPLICADOS\n")
seg_mejor <- precision_cobertura %>% head(3)
for(i in 1:nrow(seg_mejor)) {
  cat("   ├─", seg_mejor$cobertura[i], 
      "(RMSE:", round(seg_mejor$rmse[i], 2), 
      ", Corr:", round(seg_mejor$correlacion[i], 3), ")\n")
}

cat("\n6. SEGMENTOS PEOR EXPLICADOS\n")
seg_peor <- precision_cobertura %>% tail(3)
for(i in 1:nrow(seg_peor)) {
  cat("   ├─", seg_peor$cobertura[i], 
      "(RMSE:", round(seg_peor$rmse[i], 2), 
      ", Corr:", round(seg_peor$correlacion[i], 3), ")\n")
}

cat("\n7. POSIBLES MEJORAS FUTURAS\n")
cat("   ├─ Incorporar Árboles de Decisión para capturar no-linealidades\n")
cat("   ├─ Implementar Random Forest para interacciones\n")
cat("   ├─ Usar Gradient Boosting para optimización iterativa\n")
cat("   ├─ Incluir variables de comportamiento y siniestralidad histórica\n")
cat("   ├─ Modelar frecuencia de pequeños vs grandes siniestros por separado\n")
cat("   └─ Incorporar factores externos (económicos, demográficos)\n")

cat("\n", strrep("=", 80), "\n")

# ============================================================================
# PASO 16: GUARDAR RESULTADOS
# ============================================================================

cat("\n========== PASO 16: GUARDANDO RESULTADOS ==========\n")

# Guardar base completa con predicciones
write_xlsx(base_prima, "base_prima_pura_glm_completa.xlsx")
cat("✓ Base completa guardada: base_prima_pura_glm_completa.xlsx\n")

# Guardar resumen de segmentos
lista_segmentos <- list(
  "Entidad" = segmento_entidad %>% arrange(desc(abs(diferencia_porcentual))),
  "Cobertura" = tabla_comparativa_cobertura,
  "Tipo_Veh" = tabla_comparativa_tipo_veh,
  "Uso" = segmento_uso %>% arrange(desc(abs(diferencia_porcentual))),
  "Marca_Tipo" = segmento_marca_tipo %>% arrange(desc(abs(diferencia_porcentual))),
  "CVE_AMIS" = segmento_cve_amis %>% arrange(desc(abs(diferencia_porcentual)))
)

write_xlsx(lista_segmentos, "resumen_por_segmentos.xlsx")
cat("✓ Resumen de segmentos guardado: resumen_por_segmentos.xlsx\n")

# Guardar métricas globales
metricas_excel <- list(
  "Metricas_Globales" = metricas_globales,
  "Percentiles" = percentiles_prima,
  "Precision_Cobertura" = precision_cobertura,
  "Precision_Tipo_Veh" = precision_tipo_veh,
  "Precision_Uso" = precision_uso
)

write_xlsx(metricas_excel, "metricas_desempeño_glm.xlsx")
cat("✓ Métricas de desempeño guardadas: metricas_desempeño_glm.xlsx\n")

# Guardar segmentos problemáticos
segmentos_problema <- list(
  "Sobretarificados" = sobretarificados_entidad,
  "Subtarificados" = subtarificados_entidad,
  "Mayor_Desviacion" = mayor_desviacion
)

write_xlsx(segmentos_problema, "segmentos_problematicos.xlsx")
cat("✓ Segmentos problemáticos guardados: segmentos_problematicos.xlsx\n")

# Guardar base prima en RDS para uso posterior
saveRDS(base_prima, "base_prima_pura_glm.rds")
cat("✓ Base prima guardada en RDS: base_prima_pura_glm.rds\n")

# Crear resumen ejecutivo
resumen_ejecutivo <- data.frame(
  Concepto = c(
    "Observaciones",
    "Prima Pura GLM - Media",
    "Prima Pura GLM - Mediana",
    "Prima Pura GLM - Desv.Est.",
    "RMSE",
    "MAE",
    "MAPE (%)",
    "Correlación",
    "Suma Observada",
    "Suma Estimada",
    "Desviación (%)",
    "Segmentos Bien Tarificados",
    "Segmentos Sobretarificados",
    "Segmentos Subtarificados"
  ),
  Valor = c(
    format(nrow(base_prima), big.mark=","),
    format(round(mean(base_prima$prima_pura_glm), 2), big.mark=","),
    format(round(median(base_prima$prima_pura_glm), 2), big.mark=","),
    format(round(sd(base_prima$prima_pura_glm), 2), big.mark=","),
    format(round(rmse_prima, 2), big.mark=","),
    format(round(mae_prima, 2), big.mark=","),
    round(mape_prima, 2),
    round(corr_prima, 4),
    format(round(suma_observada, 0), big.mark=","),
    format(round(suma_estimada, 0), big.mark=","),
    round(desviacion_suma_pct, 2),
    nrow(segmento_cobertura %>% filter(abs(diferencia_porcentual) <= 10)),
    nrow(segmento_cobertura %>% filter(diferencia_porcentual > 10)),
    nrow(segmento_cobertura %>% filter(diferencia_porcentual < -10))
  )
)

write_xlsx(resumen_ejecutivo, "resumen_ejecutivo_prima_glm.xlsx")
cat("✓ Resumen ejecutivo guardado: resumen_ejecutivo_prima_glm.xlsx\n")

cat("\n✓ CÁLCULO DE PRIMA PURA COMPLETADO EXITOSAMENTE\n")
cat("\nArchivos generados:\n")
cat("1. base_prima_pura_glm_completa.xlsx - Base con todas las predicciones\n")
cat("2. resumen_por_segmentos.xlsx - Análisis por segmentos de negocio\n")
cat("3. metricas_desempeño_glm.xlsx - Métricas de bondad de ajuste\n")
cat("4. segmentos_problematicos.xlsx - Segmentos sobretarificados/subtarificados\n")
cat("5. resumen_ejecutivo_prima_glm.xlsx - Resumen de KPIs principales\n")
cat("6. base_prima_pura_glm.rds - Base en formato RDS para análisis futuro\n")

# ============================================================================
# FIN DEL SCRIPT
# ============================================================================
