################################################################################
# ANÁLISIS EXPLORATORIO DE DATOS (EDA) - TARIFICACIÓN DE SEGUROS AUTOS
################################################################################
# Objetivo: Exploración completa de la base maestra para entender el comportamiento
# de los datos, identificar patrones, relaciones y tomar decisiones metodológicas.
#
# NO se construyen modelos GLM, árboles ni Random Forest.
# El objetivo es entender los datos antes de modelar.
#
# Base maestra contiene:
# id, exposicion, frecuencia, severidad_promedio, costo_total,
# entidad, tipo_veh, marca_tipo, cve_amis, modelo, uso,
# cobertura, sa, deducible, prima_emi
#
# Librerías: sqldf, dplyr, tidyverse, ggplot2, janitor
#
# Fecha: 2026-06-05
################################################################################

# ===========================
# 0. SETUP Y LIBRERÍAS
# ===========================

library(sqldf)
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(writexl)

# Configurar opciones
options(sqldf.driver = "SQLite")
options(scipen = 999)

# Crear directorio para gráficos
if (!dir.exists("graficos")) {
  dir.create("graficos")
}

if (!dir.exists("reportes")) {
  dir.create("reportes")
}

# Función auxiliar para imprimir títulos
print_section <- function(title) {
  cat("\n")
  cat(strrep("=", 95), "\n")
  cat(title, "\n")
  cat(strrep("=", 95), "\n")
}

print_subsection <- function(subtitle) {
  cat("\n", strrep("-", 95), "\n")
  cat(subtitle, "\n")
  cat(strrep("-", 95), "\n")
}

# Inicializar lista para guardar resultados
eda_resultados <- list()

################################################################################
# SECCIÓN 1: ANÁLISIS DE LA VARIABLE FRECUENCIA
################################################################################

print_section("SECCIÓN 1: ANÁLISIS DE LA VARIABLE FRECUENCIA")

cat("\n[1.1] Estadísticas Descriptivas de FRECUENCIA\n")

# Estadísticas básicas
stats_freq <- base_modelacion$base_maestra %>%
  summarise(
    n_observaciones = n(),
    n_validas = sum(!is.na(num_siniestros)),
    media = mean(num_siniestros, na.rm = TRUE),
    mediana = median(num_siniestros, na.rm = TRUE),
    varianza = var(num_siniestros, na.rm = TRUE),
    desv_estandar = sd(num_siniestros, na.rm = TRUE),
    coef_variacion = sd(num_siniestros, na.rm = TRUE) / mean(num_siniestros, na.rm = TRUE),
    minimo = min(num_siniestros, na.rm = TRUE),
    maximo = max(num_siniestros, na.rm = TRUE),
    rango = max(num_siniestros, na.rm = TRUE) - min(num_siniestros, na.rm = TRUE)
  )

cat("Observaciones totales:     ", format(stats_freq$n_observaciones, big.mark = ","), "\n")
cat("Observaciones válidas:     ", format(stats_freq$n_validas, big.mark = ","), "\n")
cat("Media:                     ", round(stats_freq$media, 4), "\n")
cat("Mediana:                   ", stats_freq$mediana, "\n")
cat("Varianza:                  ", round(stats_freq$varianza, 4), "\n")
cat("Desviación Estándar:       ", round(stats_freq$desv_estandar, 4), "\n")
cat("Coef. de Variación:        ", round(stats_freq$coef_variacion, 4), "\n")
cat("Mínimo:                    ", stats_freq$minimo, "\n")
cat("Máximo:                    ", stats_freq$maximo, "\n")
cat("Rango:                     ", stats_freq$rango, "\n")

eda_resultados$stats_frecuencia <- stats_freq

cat("\n[1.2] Porcentajes de Frecuencia\n")

pct_freq <- base_modelacion$base_maestra %>%
  summarise(
    n_frecuencia_0 = sum(num_siniestros == 0, na.rm = TRUE),
    pct_frecuencia_0 = round(100 * sum(num_siniestros == 0, na.rm = TRUE) / n(), 2),
    n_frecuencia_1 = sum(num_siniestros == 1, na.rm = TRUE),
    pct_frecuencia_1 = round(100 * sum(num_siniestros == 1, na.rm = TRUE) / n(), 2),
    n_frecuencia_mayor_0 = sum(num_siniestros > 0, na.rm = TRUE),
    pct_frecuencia_mayor_0 = round(100 * sum(num_siniestros > 0, na.rm = TRUE) / n(), 2),
    n_frecuencia_mayor_1 = sum(num_siniestros > 1, na.rm = TRUE),
    pct_frecuencia_mayor_1 = round(100 * sum(num_siniestros > 1, na.rm = TRUE) / n(), 2)
  )

cat("Frecuencia = 0:            ", format(pct_freq$n_frecuencia_0, big.mark = ","),
    " (", pct_freq$pct_frecuencia_0, "%)\n")
cat("Frecuencia = 1:            ", format(pct_freq$n_frecuencia_1, big.mark = ","),
    " (", pct_freq$pct_frecuencia_1, "%)\n")
cat("Frecuencia > 0:            ", format(pct_freq$n_frecuencia_mayor_0, big.mark = ","),
    " (", pct_freq$pct_frecuencia_mayor_0, "%)\n")
cat("Frecuencia > 1:            ", format(pct_freq$n_frecuencia_mayor_1, big.mark = ","),
    " (", pct_freq$pct_frecuencia_mayor_1, "%)\n")

eda_resultados$pct_frecuencia <- pct_freq

cat("\n[1.3] Tabla de Frecuencias\n")

tabla_freq <- base_modelacion$base_maestra %>%
  group_by(num_siniestros) %>%
  summarise(
    cantidad = n(),
    pct = round(100 * n() / nrow(base_modelacion$base_maestra), 2),
    pct_acumulado = cumsum(round(100 * n() / nrow(base_modelacion$base_maestra), 2)),
    .groups = 'drop'
  ) %>%
  arrange(num_siniestros) %>%
  head(20)

print(tabla_freq %>% as.data.frame())

cat("\nNota: Mostrando primeros 20 valores\n")

eda_resultados$tabla_frecuencias <- tabla_freq

cat("\n[1.4] Percentiles de Frecuencia\n")

percentiles_freq <- base_modelacion$base_maestra %>%
  summarise(
    p1 = quantile(num_siniestros, 0.01, na.rm = TRUE),
    p5 = quantile(num_siniestros, 0.05, na.rm = TRUE),
    p10 = quantile(num_siniestros, 0.10, na.rm = TRUE),
    p25 = quantile(num_siniestros, 0.25, na.rm = TRUE),
    p50 = quantile(num_siniestros, 0.50, na.rm = TRUE),
    p75 = quantile(num_siniestros, 0.75, na.rm = TRUE),
    p90 = quantile(num_siniestros, 0.90, na.rm = TRUE),
    p95 = quantile(num_siniestros, 0.95, na.rm = TRUE),
    p99 = quantile(num_siniestros, 0.99, na.rm = TRUE)
  )

print(percentiles_freq %>% as.data.frame())

eda_resultados$percentiles_frecuencia <- percentiles_freq

cat("\n[1.5] Histograma de Frecuencia\n")

p1 <- ggplot(base_modelacion$base_maestra, aes(x = num_siniestros)) +
  geom_histogram(bins = 30, fill = "#2E86AB", alpha = 0.7, color = "black") +
  labs(
    title = "Distribución de Frecuencia de Siniestros",
    x = "Número de Siniestros",
    y = "Cantidad de Pólizas",
    subtitle = paste("Media:", round(stats_freq$media, 3), 
                     "| Mediana:", stats_freq$mediana,
                     "| SD:", round(stats_freq$desv_estandar, 3))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave("graficos/01_histograma_frecuencia.png", p1, width = 10, height = 6, dpi = 300)
cat("✓ Gráfico guardado: graficos/01_histograma_frecuencia.png\n")

################################################################################
# SECCIÓN 2: ANÁLISIS DE LA VARIABLE SEVERIDAD_PROMEDIO
################################################################################

print_section("SECCIÓN 2: ANÁLISIS DE LA VARIABLE SEVERIDAD_PROMEDIO")

cat("\n[2.1] Estadísticas Descriptivas de SEVERIDAD PROMEDIO\n")

# Solo considerar severidades > 0 (pólizas con siniestros)
base_con_siniestros <- base_modelacion$base_maestra %>%
  filter(severidad_promedio > 0)

stats_sev <- base_con_siniestros %>%
  summarise(
    n_observaciones = n(),
    n_validas = sum(!is.na(severidad_promedio)),
    media = mean(severidad_promedio, na.rm = TRUE),
    mediana = median(severidad_promedio, na.rm = TRUE),
    varianza = var(severidad_promedio, na.rm = TRUE),
    desv_estandar = sd(severidad_promedio, na.rm = TRUE),
    coef_variacion = sd(severidad_promedio, na.rm = TRUE) / mean(severidad_promedio, na.rm = TRUE),
    minimo = min(severidad_promedio, na.rm = TRUE),
    maximo = max(severidad_promedio, na.rm = TRUE),
    rango = max(severidad_promedio, na.rm = TRUE) - min(severidad_promedio, na.rm = TRUE),
    skewness = mean((severidad_promedio - mean(severidad_promedio, na.rm = TRUE))^3, na.rm = TRUE) / 
               (sd(severidad_promedio, na.rm = TRUE)^3)
  )

cat("Observaciones totales:     ", format(stats_sev$n_observaciones, big.mark = ","), "\n")
cat("  (Pólizas con siniestros)\n")
cat("Media:                     $", format(round(stats_sev$media, 2), big.mark = ","), "\n")
cat("Mediana:                   $", format(round(stats_sev$mediana, 2), big.mark = ","), "\n")
cat("Desviación Estándar:       $", format(round(stats_sev$desv_estandar, 2), big.mark = ","), "\n")
cat("Coef. de Variación:        ", round(stats_sev$coef_variacion, 4), "\n")
cat("Mínimo:                    $", format(round(stats_sev$minimo, 2), big.mark = ","), "\n")
cat("Máximo:                    $", format(round(stats_sev$maximo, 2), big.mark = ","), "\n")
cat("Rango:                     $", format(round(stats_sev$rango, 2), big.mark = ","), "\n")
cat("Skewness (asimetría):      ", round(stats_sev$skewness, 4), "\n")

eda_resultados$stats_severidad <- stats_sev

cat("\n[2.2] Percentiles de SEVERIDAD PROMEDIO\n")

percentiles_sev <- base_con_siniestros %>%
  summarise(
    p1 = quantile(severidad_promedio, 0.01, na.rm = TRUE),
    p5 = quantile(severidad_promedio, 0.05, na.rm = TRUE),
    p10 = quantile(severidad_promedio, 0.10, na.rm = TRUE),
    p25 = quantile(severidad_promedio, 0.25, na.rm = TRUE),
    p50 = quantile(severidad_promedio, 0.50, na.rm = TRUE),
    p75 = quantile(severidad_promedio, 0.75, na.rm = TRUE),
    p90 = quantile(severidad_promedio, 0.90, na.rm = TRUE),
    p95 = quantile(severidad_promedio, 0.95, na.rm = TRUE),
    p99 = quantile(severidad_promedio, 0.99, na.rm = TRUE)
  )

cat("P1:   $", format(round(percentiles_sev$p1, 2), big.mark = ","), "\n")
cat("P5:   $", format(round(percentiles_sev$p5, 2), big.mark = ","), "\n")
cat("P25:  $", format(round(percentiles_sev$p25, 2), big.mark = ","), "\n")
cat("P50:  $", format(round(percentiles_sev$p50, 2), big.mark = ","), "\n")
cat("P75:  $", format(round(percentiles_sev$p75, 2), big.mark = ","), "\n")
cat("P95:  $", format(round(percentiles_sev$p95, 2), big.mark = ","), "\n")
cat("P99:  $", format(round(percentiles_sev$p99, 2), big.mark = ","), "\n")

eda_resultados$percentiles_severidad <- percentiles_sev

cat("\n[2.3] Identificar Valores Extremos (Outliers)\n")

Q1 <- percentiles_sev$p25
Q3 <- percentiles_sev$p75
IQR <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

outliers <- base_con_siniestros %>%
  filter(severidad_promedio < limite_inferior | severidad_promedio > limite_superior)

cat("Límite inferior (Q1 - 1.5*IQR): $", format(round(limite_inferior, 2), big.mark = ","), "\n")
cat("Límite superior (Q3 + 1.5*IQR): $", format(round(limite_superior, 2), big.mark = ","), "\n")
cat("Cantidad de outliers:            ", nrow(outliers), "\n")
cat("Porcentaje de outliers:          ", 
    round(100 * nrow(outliers) / nrow(base_con_siniestros), 2), "%\n")

eda_resultados$outliers_severidad <- outliers

cat("\n[2.4] Histograma de Severidad Promedio\n")

p2 <- ggplot(base_con_siniestros, aes(x = severidad_promedio)) +
  geom_histogram(bins = 50, fill = "#A23B72", alpha = 0.7, color = "black") +
  labs(
    title = "Distribución de Severidad Promedio",
    x = "Severidad Promedio ($)",
    y = "Cantidad de Pólizas",
    subtitle = paste("Media: $", format(round(stats_sev$media, 0), big.mark = ","),
                     " | Mediana: $", format(round(stats_sev$mediana, 0), big.mark = ","))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave("graficos/02_histograma_severidad.png", p2, width = 10, height = 6, dpi = 300)
cat("✓ Gráfico guardado: graficos/02_histograma_severidad.png\n")

cat("\n[2.5] Boxplot de Severidad Promedio\n")

p3 <- ggplot(base_con_siniestros, aes(y = severidad_promedio)) +
  geom_boxplot(fill = "#A23B72", alpha = 0.7, color = "black") +
  geom_point(data = outliers, aes(x = 0.5, y = severidad_promedio), 
             color = "red", size = 2, alpha = 0.5) +
  labs(
    title = "Boxplot de Severidad Promedio",
    subtitle = paste("Puntos rojos = outliers"),
    y = "Severidad Promedio ($)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("graficos/03_boxplot_severidad.png", p3, width = 10, height = 6, dpi = 300)
cat("✓ Gráfico guardado: graficos/03_boxplot_severidad.png\n")

################################################################################
# SECCIÓN 3: ANÁLISIS DE COSTO_TOTAL
################################################################################

print_section("SECCIÓN 3: ANÁLISIS DE COSTO_TOTAL")

cat("\n[3.1] Estadísticas Descriptivas de COSTO_TOTAL\n")

base_con_costo <- base_modelacion$base_maestra %>%
  filter(costo_total > 0)

stats_costo <- base_con_costo %>%
  summarise(
    n_observaciones = n(),
    media = mean(costo_total, na.rm = TRUE),
    mediana = median(costo_total, na.rm = TRUE),
    desv_estandar = sd(costo_total, na.rm = TRUE),
    minimo = min(costo_total, na.rm = TRUE),
    maximo = max(costo_total, na.rm = TRUE),
    total = sum(costo_total, na.rm = TRUE),
    skewness = mean((costo_total - mean(costo_total, na.rm = TRUE))^3, na.rm = TRUE) / 
               (sd(costo_total, na.rm = TRUE)^3)
  )

cat("Observaciones totales:     ", format(stats_costo$n_observaciones, big.mark = ","), "\n")
cat("Media:                     $", format(round(stats_costo$media, 2), big.mark = ","), "\n")
cat("Mediana:                   $", format(round(stats_costo$mediana, 2), big.mark = ","), "\n")
cat("Desviación Estándar:       $", format(round(stats_costo$desv_estandar, 2), big.mark = ","), "\n")
cat("Mínimo:                    $", format(round(stats_costo$minimo, 2), big.mark = ","), "\n")
cat("Máximo:                    $", format(round(stats_costo$maximo, 2), big.mark = ","), "\n")
cat("Total:                     $", format(round(stats_costo$total, 2), big.mark = ","), "\n")
cat("Skewness:                  ", round(stats_costo$skewness, 4), "\n")

eda_resultados$stats_costo <- stats_costo

cat("\n[3.2] Histograma de Costo Total\n")

p4 <- ggplot(base_con_costo, aes(x = costo_total)) +
  geom_histogram(bins = 50, fill = "#F18F01", alpha = 0.7, color = "black") +
  labs(
    title = "Distribución de Costo Total",
    x = "Costo Total ($)",
    y = "Cantidad de Pólizas"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave("graficos/04_histograma_costo_total.png", p4, width = 10, height = 6, dpi = 300)
cat("✓ Gráfico guardado: graficos/04_histograma_costo_total.png\n")

cat("\n[3.3] Costo Total por Cobertura\n")

costo_por_cobertura <- base_modelacion$base_maestra %>%
  group_by(cobertura) %>%
  summarise(
    cantidad_polizas = n(),
    costo_total = round(sum(costo_total, na.rm = TRUE), 2),
    costo_promedio = round(mean(costo_total, na.rm = TRUE), 2),
    costo_mediana = round(median(costo_total, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(costo_total))

print(costo_por_cobertura %>% as.data.frame())

eda_resultados$costo_por_cobertura <- costo_por_cobertura

################################################################################
# SECCIÓN 4: ANÁLISIS DE VARIABLES EXPLICATIVAS CATEGÓRICAS
################################################################################

print_section("SECCIÓN 4: ANÁLISIS DE VARIABLES EXPLICATIVAS CATEGÓRICAS")

# Variables categóricas a analizar
vars_categoricas <- c("entidad", "tipo_veh", "uso", "cobertura", "marca_tipo", "cve_amis")

for (var in vars_categoricas) {
  cat("\n[4.1]", toupper(var), "\n")
  
  # Crear tabla dinámica
  resultado <- base_modelacion$base_maestra %>%
    group_by(!!sym(var)) %>%
    summarise(
      frecuencia_absoluta = n(),
      frecuencia_relativa = round(n() / nrow(base_modelacion$base_maestra), 4),
      porcentaje = round(100 * n() / nrow(base_modelacion$base_maestra), 2),
      .groups = 'drop'
    ) %>%
    arrange(desc(frecuencia_absoluta)) %>%
    head(20)
  
  print(resultado %>% as.data.frame())
  
  assign(paste0("dist_", var), resultado)
  eda_resultados[[paste0("dist_", var)]] <- resultado
}

################################################################################
# SECCIÓN 5: FRECUENCIA PROMEDIO POR VARIABLES EXPLICATIVAS
################################################################################

print_section("SECCIÓN 5: RELACIÓN ENTRE VARIABLES EXPLICATIVAS Y FRECUENCIA")

cat("\n[5.1] Frecuencia Promedio por ENTIDAD\n")

freq_por_entidad <- base_modelacion$base_maestra %>%
  group_by(entidad) %>%
  summarise(
    cantidad_polizas = n(),
    frecuencia_media = round(mean(num_siniestros, na.rm = TRUE), 4),
    frecuencia_mediana = median(num_siniestros, na.rm = TRUE),
    frecuencia_sd = round(sd(num_siniestros, na.rm = TRUE), 4),
    pct_con_siniestro = round(100 * sum(num_siniestros > 0) / n(), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(frecuencia_media))

print(freq_por_entidad %>% as.data.frame() %>% head(15))

eda_resultados$freq_por_entidad <- freq_por_entidad

cat("\n[5.2] Frecuencia Promedio por TIPO_VEH\n")

freq_por_tipo_veh <- base_modelacion$base_maestra %>%
  group_by(tipo_veh) %>%
  summarise(
    cantidad_polizas = n(),
    frecuencia_media = round(mean(num_siniestros, na.rm = TRUE), 4),
    frecuencia_mediana = median(num_siniestros, na.rm = TRUE),
    frecuencia_sd = round(sd(num_siniestros, na.rm = TRUE), 4),
    pct_con_siniestro = round(100 * sum(num_siniestros > 0) / n(), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(frecuencia_media))

print(freq_por_tipo_veh %>% as.data.frame())

eda_resultados$freq_por_tipo_veh <- freq_por_tipo_veh

cat("\n[5.3] Frecuencia Promedio por USO\n")

freq_por_uso <- base_modelacion$base_maestra %>%
  group_by(uso) %>%
  summarise(
    cantidad_polizas = n(),
    frecuencia_media = round(mean(num_siniestros, na.rm = TRUE), 4),
    frecuencia_mediana = median(num_siniestros, na.rm = TRUE),
    frecuencia_sd = round(sd(num_siniestros, na.rm = TRUE), 4),
    pct_con_siniestro = round(100 * sum(num_siniestros > 0) / n(), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(frecuencia_media))

print(freq_por_uso %>% as.data.frame())

eda_resultados$freq_por_uso <- freq_por_uso

cat("\n[5.4] Frecuencia Promedio por COBERTURA\n")

freq_por_cobertura <- base_modelacion$base_maestra %>%
  group_by(cobertura) %>%
  summarise(
    cantidad_polizas = n(),
    frecuencia_media = round(mean(num_siniestros, na.rm = TRUE), 4),
    frecuencia_mediana = median(num_siniestros, na.rm = TRUE),
    frecuencia_sd = round(sd(num_siniestros, na.rm = TRUE), 4),
    pct_con_siniestro = round(100 * sum(num_siniestros > 0) / n(), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(frecuencia_media))

print(freq_por_cobertura %>% as.data.frame())

eda_resultados$freq_por_cobertura <- freq_por_cobertura

cat("\n[5.5] Frecuencia Promedio por MARCA_TIPO\n")

freq_por_marca <- base_modelacion$base_maestra %>%
  group_by(marca_tipo) %>%
  summarise(
    cantidad_polizas = n(),
    frecuencia_media = round(mean(num_siniestros, na.rm = TRUE), 4),
    frecuencia_mediana = median(num_siniestros, na.rm = TRUE),
    frecuencia_sd = round(sd(num_siniestros, na.rm = TRUE), 4),
    pct_con_siniestro = round(100 * sum(num_siniestros > 0) / n(), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(frecuencia_media)) %>%
  head(15)

print(freq_por_marca %>% as.data.frame())

eda_resultados$freq_por_marca <- freq_por_marca

cat("\n[5.6] Frecuencia Promedio por CVE_AMIS (top 15)\n")

freq_por_amis <- base_modelacion$base_maestra %>%
  group_by(cve_amis) %>%
  summarise(
    cantidad_polizas = n(),
    frecuencia_media = round(mean(num_siniestros, na.rm = TRUE), 4),
    frecuencia_mediana = median(num_siniestros, na.rm = TRUE),
    frecuencia_sd = round(sd(num_siniestros, na.rm = TRUE), 4),
    pct_con_siniestro = round(100 * sum(num_siniestros > 0) / n(), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(frecuencia_media)) %>%
  head(15)

print(freq_por_amis %>% as.data.frame())

eda_resultados$freq_por_amis <- freq_por_amis

################################################################################
# SECCIÓN 6: SEVERIDAD PROMEDIO POR VARIABLES EXPLICATIVAS
################################################################################

print_section("SECCIÓN 6: RELACIÓN ENTRE VARIABLES EXPLICATIVAS Y SEVERIDAD")

cat("\n[6.1] Severidad Promedio por ENTIDAD\n")

sev_por_entidad <- base_modelacion$base_maestra %>%
  filter(severidad_promedio > 0) %>%
  group_by(entidad) %>%
  summarise(
    cantidad_polizas = n(),
    severidad_media = round(mean(severidad_promedio, na.rm = TRUE), 2),
    severidad_mediana = round(median(severidad_promedio, na.rm = TRUE), 2),
    severidad_sd = round(sd(severidad_promedio, na.rm = TRUE), 2),
    severidad_min = round(min(severidad_promedio, na.rm = TRUE), 2),
    severidad_max = round(max(severidad_promedio, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(severidad_media))

print(sev_por_entidad %>% as.data.frame() %>% head(15))

eda_resultados$sev_por_entidad <- sev_por_entidad

cat("\n[6.2] Severidad Promedio por TIPO_VEH\n")

sev_por_tipo_veh <- base_modelacion$base_maestra %>%
  filter(severidad_promedio > 0) %>%
  group_by(tipo_veh) %>%
  summarise(
    cantidad_polizas = n(),
    severidad_media = round(mean(severidad_promedio, na.rm = TRUE), 2),
    severidad_mediana = round(median(severidad_promedio, na.rm = TRUE), 2),
    severidad_sd = round(sd(severidad_promedio, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(severidad_media))

print(sev_por_tipo_veh %>% as.data.frame())

eda_resultados$sev_por_tipo_veh <- sev_por_tipo_veh

cat("\n[6.3] Severidad Promedio por USO\n")

sev_por_uso <- base_modelacion$base_maestra %>%
  filter(severidad_promedio > 0) %>%
  group_by(uso) %>%
  summarise(
    cantidad_polizas = n(),
    severidad_media = round(mean(severidad_promedio, na.rm = TRUE), 2),
    severidad_mediana = round(median(severidad_promedio, na.rm = TRUE), 2),
    severidad_sd = round(sd(severidad_promedio, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(severidad_media))

print(sev_por_uso %>% as.data.frame())

eda_resultados$sev_por_uso <- sev_por_uso

cat("\n[6.4] Severidad Promedio por COBERTURA\n")

sev_por_cobertura <- base_modelacion$base_maestra %>%
  filter(severidad_promedio > 0) %>%
  group_by(cobertura) %>%
  summarise(
    cantidad_polizas = n(),
    severidad_media = round(mean(severidad_promedio, na.rm = TRUE), 2),
    severidad_mediana = round(median(severidad_promedio, na.rm = TRUE), 2),
    severidad_sd = round(sd(severidad_promedio, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(severidad_media))

print(sev_por_cobertura %>% as.data.frame())

eda_resultados$sev_por_cobertura <- sev_por_cobertura

cat("\n[6.5] Severidad Promedio por MARCA_TIPO (top 15)\n")

sev_por_marca <- base_modelacion$base_maestra %>%
  filter(severidad_promedio > 0) %>%
  group_by(marca_tipo) %>%
  summarise(
    cantidad_polizas = n(),
    severidad_media = round(mean(severidad_promedio, na.rm = TRUE), 2),
    severidad_mediana = round(median(severidad_promedio, na.rm = TRUE), 2),
    severidad_sd = round(sd(severidad_promedio, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(severidad_media)) %>%
  head(15)

print(sev_por_marca %>% as.data.frame())

eda_resultados$sev_por_marca <- sev_por_marca

################################################################################
# SECCIÓN 7: ANÁLISIS DE VARIABLES NUMÉRICAS
################################################################################

print_section("SECCIÓN 7: ANÁLISIS DE VARIABLES NUMÉRICAS")

cat("\n[7.1] Estadísticas de EXPOSICIÓN\n")

stats_exp <- base_modelacion$base_maestra %>%
  summarise(
    n = n(),
    media = round(mean(exposicion_anos, na.rm = TRUE), 4),
    mediana = round(median(exposicion_anos, na.rm = TRUE), 4),
    sd = round(sd(exposicion_anos, na.rm = TRUE), 4),
    min = round(min(exposicion_anos, na.rm = TRUE), 4),
    p25 = round(quantile(exposicion_anos, 0.25, na.rm = TRUE), 4),
    p75 = round(quantile(exposicion_anos, 0.75, na.rm = TRUE), 4),
    max = round(max(exposicion_anos, na.rm = TRUE), 4)
  )

print(stats_exp %>% as.data.frame())

eda_resultados$stats_exposicion <- stats_exp

cat("\n[7.2] Estadísticas de SA (Suma Asegurada)\n")

stats_sa <- base_modelacion$base_maestra %>%
  filter(!is.na(sa)) %>%
  summarise(
    n = n(),
    media = round(mean(sa, na.rm = TRUE), 2),
    mediana = round(median(sa, na.rm = TRUE), 2),
    sd = round(sd(sa, na.rm = TRUE), 2),
    min = round(min(sa, na.rm = TRUE), 2),
    p25 = round(quantile(sa, 0.25, na.rm = TRUE), 2),
    p75 = round(quantile(sa, 0.75, na.rm = TRUE), 2),
    max = round(max(sa, na.rm = TRUE), 2)
  )

print(stats_sa %>% as.data.frame())

eda_resultados$stats_sa <- stats_sa

cat("\n[7.3] Estadísticas de DEDUCIBLE\n")

stats_ded <- base_modelacion$base_maestra %>%
  filter(!is.na(deducible)) %>%
  summarise(
    n = n(),
    media = round(mean(deducible, na.rm = TRUE), 2),
    mediana = round(median(deducible, na.rm = TRUE), 2),
    sd = round(sd(deducible, na.rm = TRUE), 2),
    min = round(min(deducible, na.rm = TRUE), 2),
    p25 = round(quantile(deducible, 0.25, na.rm = TRUE), 2),
    p75 = round(quantile(deducible, 0.75, na.rm = TRUE), 2),
    max = round(max(deducible, na.rm = TRUE), 2)
  )

print(stats_ded %>% as.data.frame())

eda_resultados$stats_deducible <- stats_ded

cat("\n[7.4] Estadísticas de PRIMA_EMI\n")

stats_prima <- base_modelacion$base_maestra %>%
  filter(!is.na(prima_emi)) %>%
  summarise(
    n = n(),
    media = round(mean(prima_emi, na.rm = TRUE), 2),
    mediana = round(median(prima_emi, na.rm = TRUE), 2),
    sd = round(sd(prima_emi, na.rm = TRUE), 2),
    min = round(min(prima_emi, na.rm = TRUE), 2),
    p25 = round(quantile(prima_emi, 0.25, na.rm = TRUE), 2),
    p75 = round(quantile(prima_emi, 0.75, na.rm = TRUE), 2),
    max = round(max(prima_emi, na.rm = TRUE), 2)
  )

print(stats_prima %>% as.data.frame())

eda_resultados$stats_prima <- stats_prima

cat("\n[7.5] Histogramas de Variables Numéricas\n")

# Exposición
p5 <- ggplot(base_modelacion$base_maestra, aes(x = exposicion_anos)) +
  geom_histogram(bins = 30, fill = "#06A77D", alpha = 0.7, color = "black") +
  labs(title = "Distribución de Exposición", x = "Años", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

ggsave("graficos/05_histograma_exposicion.png", p5, width = 10, height = 6, dpi = 300)

# SA
p6 <- ggplot(base_modelacion$base_maestra %>% filter(!is.na(sa)), aes(x = sa)) +
  geom_histogram(bins = 30, fill = "#C6538C", alpha = 0.7, color = "black") +
  labs(title = "Distribución de Suma Asegurada", x = "SA ($)", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

ggsave("graficos/06_histograma_sa.png", p6, width = 10, height = 6, dpi = 300)

# Deducible
p7 <- ggplot(base_modelacion$base_maestra %>% filter(!is.na(deducible)), aes(x = deducible)) +
  geom_histogram(bins = 30, fill = "#003B6F", alpha = 0.7, color = "black") +
  labs(title = "Distribución de Deducible", x = "Deducible ($)", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

ggsave("graficos/07_histograma_deducible.png", p7, width = 10, height = 6, dpi = 300)

# Prima
p8 <- ggplot(base_modelacion$base_maestra %>% filter(!is.na(prima_emi)), aes(x = prima_emi)) +
  geom_histogram(bins = 30, fill = "#EC8F5E", alpha = 0.7, color = "black") +
  labs(title = "Distribución de Prima Emitida", x = "Prima ($)", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

ggsave("graficos/08_histograma_prima.png", p8, width = 10, height = 6, dpi = 300)

cat("✓ Gráficos guardados\n")

################################################################################
# SECCIÓN 8: ANÁLISIS DE CORRELACIONES
################################################################################

print_section("SECCIÓN 8: ANÁLISIS DE CORRELACIONES")

cat("\n[8.1] Matriz de Correlaciones entre Variables Numéricas\n")

# Seleccionar variables numéricas
vars_numericas <- base_modelacion$base_maestra %>%
  select(exposicion_anos, num_siniestros, severidad_promedio, 
         costo_total, sa, deducible, prima_emi, modelo) %>%
  rename(
    "Exposición" = exposicion_anos,
    "Frecuencia" = num_siniestros,
    "Severidad" = severidad_promedio,
    "Costo Total" = costo_total,
    "SA" = sa,
    "Deducible" = deducible,
    "Prima" = prima_emi,
    "Modelo" = modelo
  )

# Calcular correlaciones (solo para pares válidos)
correlacion_matrix <- cor(vars_numericas, use = "complete.obs")

print(round(correlacion_matrix, 3))

eda_resultados$correlacion_matrix <- correlacion_matrix

# Gráfico de correlaciones
p9 <- ggplot(
  data.frame(
    var1 = rep(colnames(correlacion_matrix), ncol(correlacion_matrix)),
    var2 = rep(colnames(correlacion_matrix), each = nrow(correlacion_matrix)),
    corr = as.vector(correlacion_matrix)
  ),
  aes(x = var1, y = var2, fill = corr)
) +
  geom_tile() +
  geom_text(aes(label = round(corr, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = "#003B6F", mid = "white", high = "#C6538C", limits = c(-1, 1)) +
  labs(title = "Matriz de Correlaciones", fill = "Correlación") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 12)
  )

ggsave("graficos/09_matriz_correlaciones.png", p9, width = 10, height = 8, dpi = 300)

cat("\n✓ Gráfico de correlaciones guardado\n")

################################################################################
# SECCIÓN 9: EVALUACIÓN PRELIMINAR PARA MODELACIÓN
################################################################################

print_section("SECCIÓN 9: EVALUACIÓN PRELIMINAR PARA MODELACIÓN")

cat("\n[9.1] Análisis de Sobredispersión en Frecuencia\n")

varianza_freq <- stats_freq$varianza
media_freq <- stats_freq$media
indice_dispersion <- varianza_freq / media_freq

cat("Media de Frecuencia:       ", round(media_freq, 4), "\n")
cat("Varianza de Frecuencia:    ", round(varianza_freq, 4), "\n")
cat("Índice de Dispersión:      ", round(indice_dispersion, 4), "\n")

if (indice_dispersion > 1.5) {
  cat("\n⚠️  RESULTADO: SOBREDISPERSIÓN MODERADA A ALTA\n")
  cat("   Recomendación: Usar Negative Binomial en lugar de Poisson\n")
} else if (indice_dispersion > 1.1) {
  cat("\n⚠️  RESULTADO: LIGERA SOBREDISPERSIÓN\n")
  cat("   Recomendación: Considerar Negative Binomial o Poisson con ajuste\n")
} else if (indice_dispersion < 0.8) {
  cat("\n⚠️  RESULTADO: SUBDISPERSIÓN\n")
  cat("   Recomendación: Poisson puede ser apropiado\n")
} else {
  cat("\n✓ RESULTADO: Datos consistentes con distribución Poisson\n")
}

eda_resultados$indice_dispersion_frecuencia <- indice_dispersion

cat("\n[9.2] Evaluación de Distribución para Severidad\n")

# Test de normalidad en log(severidad)
log_severidad <- log(base_con_siniestros$severidad_promedio + 1)
shapiro_test <- shapiro.test(log_severidad[1:min(5000, length(log_severidad))])

cat("Shapiro-Wilk test en log(Severidad):\n")
cat("  p-value: ", round(shapiro_test$p.value, 4), "\n")

if (shapiro_test$p.value > 0.05) {
  cat("  ✓ Log(Severidad) parece normal\n")
  cat("  Recomendación: Usar GLM Gamma (apropiado para datos positivos sesgados)\n")
} else {
  cat("  Log(Severidad) no es normal\n")
  cat("  Recomendación: Gamma o Lognormal\n")
}

# Evaluar asimetría
if (abs(stats_sev$skewness) > 2) {
  cat("  ⚠️  Alta asimetría (", round(stats_sev$skewness, 2), ")\n")
  cat("  Recomendación: Gamma con link log es apropiado\n")
}

eda_resultados$shapiro_test_severidad <- shapiro_test

cat("\n[9.3] Variables Más Prometedoras para FRECUENCIA\n")

# Calcular correlación con frecuencia usando ANOVA
cat("\nAnálisis por categoría (ANOVA implícito):\n")

vars_prometedoras_freq <- data.frame(
  variable = c("tipo_veh", "uso", "cobertura", "entidad", "marca_tipo"),
  coef_variacion = c(
    sd(freq_por_tipo_veh$frecuencia_media) / mean(freq_por_tipo_veh$frecuencia_media),
    sd(freq_por_uso$frecuencia_media) / mean(freq_por_uso$frecuencia_media),
    sd(freq_por_cobertura$frecuencia_media) / mean(freq_por_cobertura$frecuencia_media),
    sd(freq_por_entidad$frecuencia_media) / mean(freq_por_entidad$frecuencia_media),
    sd(freq_por_marca$frecuencia_media) / mean(freq_por_marca$frecuencia_media)
  )
) %>%
  arrange(desc(coef_variacion))

print(vars_prometedoras_freq %>% as.data.frame())

cat("\nInterpretación:\n")
cat("Variables con mayor variación entre categorías tienen mayor poder explicativo\n")

eda_resultados$vars_prometedoras_frecuencia <- vars_prometedoras_freq

cat("\n[9.4] Variables Más Prometedoras para SEVERIDAD\n")

vars_prometedoras_sev <- data.frame(
  variable = c("tipo_veh", "uso", "cobertura", "entidad", "marca_tipo"),
  coef_variacion = c(
    sd(sev_por_tipo_veh$severidad_media, na.rm = TRUE) / mean(sev_por_tipo_veh$severidad_media, na.rm = TRUE),
    sd(sev_por_uso$severidad_media, na.rm = TRUE) / mean(sev_por_uso$severidad_media, na.rm = TRUE),
    sd(sev_por_cobertura$severidad_media, na.rm = TRUE) / mean(sev_por_cobertura$severidad_media, na.rm = TRUE),
    sd(sev_por_entidad$severidad_media, na.rm = TRUE) / mean(sev_por_entidad$severidad_media, na.rm = TRUE),
    sd(sev_por_marca$severidad_media, na.rm = TRUE) / mean(sev_por_marca$severidad_media, na.rm = TRUE)
  )
) %>%
  arrange(desc(coef_variacion))

print(vars_prometedoras_sev %>% as.data.frame())

eda_resultados$vars_prometedoras_severidad <- vars_prometedoras_sev

################################################################################
# SECCIÓN 10: REPORTE FINAL
################################################################################

print_section("SECCIÓN 10: REPORTE FINAL Y CONCLUSIONES")

cat("\n╔", strrep("═", 93), "╗\n", sep = "")
cat("║", strrep(" ", 25), "ANÁLISIS EXPLORATORIO - CONCLUSIONES", strrep(" ", 30), "║\n")
cat("╚", strrep("═", 93), "╝\n", sep = "")

cat("\n[10.1] PRINCIPALES HALLAZGOS\n")

cat("\n1. FRECUENCIA DE SINIESTROS:\n")
cat("   • Media: ", round(stats_freq$media, 3), "siniestros/póliza\n")
cat("   • Mediana: ", stats_freq$mediana, "\n")
cat("   • Pólizas sin siniestros: ", pct_freq$pct_frecuencia_0, "%\n")
cat("   • Distribución: ", ifelse(indice_dispersion > 1.1, "Sobredispersa", "Poisson-like"), "\n")

cat("\n2. SEVERIDAD PROMEDIO (en pólizas con siniestros):\n")
cat("   • Media: $", format(round(stats_sev$media, 0), big.mark = ","), "\n")
cat("   • Mediana: $", format(round(stats_sev$mediana, 0), big.mark = ","), "\n")
cat("   • Rango: $", format(round(stats_sev$minimo, 0), big.mark = ","),
    " - $", format(round(stats_sev$maximo, 0), big.mark = ","), "\n")
cat("   • Outliers: ", nrow(outliers), " (", 
    round(100 * nrow(outliers) / nrow(base_con_siniestros), 2), "%)\n")
cat("   • Asimetría: ", round(stats_sev$skewness, 2), " (muy sesgada a la derecha)\n")

cat("\n3. COSTO TOTAL:\n")
cat("   • Pólizas con costo: ", format(nrow(base_con_costo), big.mark = ","), "\n")
cat("   • Total costo: $", format(round(stats_costo$total, 0), big.mark = ","), "\n")
cat("   • Costo promedio: $", format(round(stats_costo$media, 0), big.mark = ","), "\n")

cat("\n4. VARIABLES CON MAYOR PODER EXPLICATIVO:\n")
cat("\n   Para FRECUENCIA:\n")
for (i in 1:nrow(vars_prometedoras_freq)) {
  cat("   ", i, ".", vars_prometedoras_freq$variable[i], 
      " (CV:", round(vars_prometedoras_freq$coef_variacion[i], 4), ")\n")
}

cat("\n   Para SEVERIDAD:\n")
for (i in 1:nrow(vars_prometedoras_sev)) {
  cat("   ", i, ".", vars_prometedoras_sev$variable[i], 
      " (CV:", round(vars_prometedoras_sev$coef_variacion[i], 4), ")\n")
}

cat("\n[10.2] PROBLEMAS DE CALIDAD DETECTADOS\n")

problemas <- 0

if (pct_freq$pct_frecuencia_0 > 80) {
  problemas <- problemas + 1
  cat("\n⚠️  PROBLEMA #", problemas, ": Exceso de ceros en frecuencia\n")
  cat("   Causa probable: Muchas pólizas sin siniestros\n")
  cat("   Solución: Considerar Zero-Inflated Poisson o modelo dos partes\n")
}

if (nrow(outliers) > 0.05 * nrow(base_con_siniestros)) {
  problemas <- problemas + 1
  cat("\n⚠️  PROBLEMA #", problemas, ": Muchos outliers en severidad\n")
  cat("   Solución: Revisar datos, aplicar transformaciones o usar robust methods\n")
}

if (indice_dispersion > 2) {
  problemas <- problemas + 1
  cat("\n⚠️  PROBLEMA #", problemas, ": Alta sobredispersión en frecuencia\n")
  cat("   Solución: Usar Negative Binomial en GLM\n")
}

if (problemas == 0) {
  cat("\n✓ No se detectaron problemas mayores de calidad\n")
}

cat("\n[10.3] RECOMENDACIÓN PRELIMINAR DE DISTRIBUCIONES\n")

cat("\n1. MODELO DE FRECUENCIA:\n")
if (indice_dispersion > 1.5) {
  cat("   ✓ Recomendado: Negative Binomial GLM\n")
} else if (indice_dispersion > 1.1) {
  cat("   ✓ Recomendado: Negative Binomial GLM (o Poisson)\n")
} else {
  cat("   ✓ Recomendado: Poisson GLM\n")
}
cat("   ✓ Link function: log\n")
cat("   ✓ Offset: log(exposicion_anos)\n")

cat("\n2. MODELO DE SEVERIDAD:\n")
cat("   ✓ Recomendado: Gamma GLM\n")
cat("   ✓ Link function: log\n")
cat("   ✓ Alternativa: Lognormal (si datos muy sesgados)\n")

cat("\n[10.4] PRÓXIMOS PASOS\n")

cat("\n   1. Crear variables dummy para variables categóricas\n")
cat("   2. Escalar variables numéricas si es necesario\n")
cat("   3. Construir modelo GLM Frecuencia (Negative Binomial)\n")
cat("   4. Construir modelo GLM Severidad (Gamma)\n")
cat("   5. Calcular Prima Pura = Frecuencia × Severidad\n")
cat("   6. Ajustar prima final según criterios comerciales\n")

cat("\n")
cat("╔", strrep("═", 93), "╗\n", sep = "")
cat("║", format(paste("EDA completado:", Sys.time()), width = 93), "║\n")
cat("╚", strrep("═", 93), "╝\n", sep = "")

################################################################################
# SECCIÓN 11: EXPORTAR RESULTADOS
################################################################################

print_section("SECCIÓN 11: EXPORTAR RESULTADOS")

cat("\n[11.1] Guardar tablas de análisis en Excel\n")

# Crear lista para exportar
export_list <- list(
  "Estadísticas_Frecuencia" = as.data.frame(stats_freq),
  "Porcentajes_Frecuencia" = as.data.frame(pct_freq),
  "Estadísticas_Severidad" = as.data.frame(stats_sev),
  "Estadísticas_Costo" = as.data.frame(stats_costo),
  "Freq_por_Tipo_Veh" = freq_por_tipo_veh,
  "Freq_por_Uso" = freq_por_uso,
  "Freq_por_Cobertura" = freq_por_cobertura,
  "Sev_por_Tipo_Veh" = sev_por_tipo_veh,
  "Sev_por_Uso" = sev_por_uso,
  "Sev_por_Cobertura" = sev_por_cobertura,
  "Vars_Prometedoras_Freq" = vars_prometedoras_freq,
  "Vars_Prometedoras_Sev" = vars_prometedoras_sev
)

write_xlsx(export_list, path = "reportes/eda_resultados_analisis.xlsx")

cat("✓ Análisis exportado: reportes/eda_resultados_analisis.xlsx\n")

cat("\n[11.2] Ubicación de gráficos\n")

graficos <- list.files("graficos", pattern = "*.png", full.names = FALSE)
cat("Total de gráficos generados:", length(graficos), "\n")
for (g in graficos) {
  cat("  •", g, "\n")
}

cat("\n[11.3] Objeto 'eda_resultados' disponible en memoria\n")
cat("Elementos disponibles:", length(eda_resultados), "\n")
for (i in 1:min(15, length(eda_resultados))) {
  cat("  •", names(eda_resultados)[i], "\n")
}

cat("\n✓ EDA completado. Base lista para modelación.\n")

cat("\n" %+paste% strrep("=", 95) %+paste% "\n")
