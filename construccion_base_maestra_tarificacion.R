################################################################################
# CONSTRUCCIÓN DE BASE MAESTRA PARA TARIFICACIÓN DE SEGUROS AUTOS
################################################################################
# Objetivo: Construir una base de datos limpia a nivel póliza, agregando
# exposición, frecuencia y severidad de siniestros para modelación actuarial.
#
# Pasos:
# 1. Construir tabla de exposición
# 2. Construir tabla de frecuencia
# 3. Construir tabla de severidad
# 4. Agregar emisiones (si es necesario)
# 5. Construir base maestra uniendo todas las tablas
# 6. Validar integridad y generar reporte
#
# Librerías: sqldf, dplyr, tidyverse, lubridate, janitor, writexl
#
# Fecha: 2026-06-05
################################################################################

# ===========================
# 0. SETUP Y LIBRERÍAS
# ===========================

library(sqldf)
library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(writexl)

# Configurar opciones
options(sqldf.driver = "SQLite")
options(scipen = 999)  # Evitar notación científica

# Crear directorio para guardar reportes
if (!dir.exists("reportes")) {
  dir.create("reportes")
}

# Función auxiliar para imprimir títulos
print_section <- function(title) {
  cat("\n")
  cat(strrep("=", 90), "\n")
  cat(title, "\n")
  cat(strrep("=", 90), "\n")
}

print_subsection <- function(subtitle) {
  cat("\n", strrep("-", 90), "\n")
  cat(subtitle, "\n")
  cat(strrep("-", 90), "\n")
}

# Función para mostrar resumen de tabla
show_summary <- function(data, name) {
  cat("\n✓", name, "\n")
  cat("  - Registros:", nrow(data), "\n")
  cat("  - Columnas:", ncol(data), "\n")
  cat("  - IDs únicos:", n_distinct(data$id), "\n")
}

# Inicializar lista para guardar resultados
base_modelacion <- list()

################################################################################
# SECCIÓN 1: CONSTRUCCIÓN DE TABLA DE EXPOSICIÓN
################################################################################

print_section("SECCIÓN 1: CONSTRUCCIÓN DE TABLA DE EXPOSICIÓN")

cat("\n[1.1] Cálculo de exposición por póliza\n")
cat("Reglas:\n")
cat("  • Si NO está cancelada: usar fecha_fin_vig\n")
cat("  • Si está cancelada: usar fec_can\n")
cat("  • Exposición = días_vigentes / 365\n")

tabla_exposicion <- sqldf("
  SELECT 
    id,
    fecha_ini_vig,
    fecha_fin_vig,
    fec_can,
    CASE 
      WHEN fec_can IS NULL OR fec_can = 'NA' THEN fecha_fin_vig
      ELSE fec_can
    END as fecha_cierre,
    CAST((
      CASE 
        WHEN fec_can IS NULL OR fec_can = 'NA' THEN fecha_fin_vig
        ELSE fec_can
      END - fecha_ini_vig
    ) AS REAL) as dias_vigencia,
    ROUND(
      CAST((
        CASE 
          WHEN fec_can IS NULL OR fec_can = 'NA' THEN fecha_fin_vig
          ELSE fec_can
        END - fecha_ini_vig
      ) AS REAL) / 365.0, 4
    ) as exposicion_anos
  FROM generales
  WHERE id IS NOT NULL
") %>%
  as.data.frame()

# Validar que no haya exposiciones negativas o cero
tabla_exposicion <- tabla_exposicion %>%
  filter(dias_vigencia > 0 & exposicion_anos > 0) %>%
  arrange(id)

cat("\n[1.2] Resumen de Exposición\n")
show_summary(tabla_exposicion, "tabla_exposicion")

# Validar unicidad
ids_duplicados_exp <- tabla_exposicion %>%
  group_by(id) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)

if (nrow(ids_duplicados_exp) > 0) {
  cat("\n⚠️  ADVERTENCIA: Hay IDs duplicados en tabla_exposicion\n")
  cat("  Cantidad:", nrow(ids_duplicados_exp), "\n")
  print(head(ids_duplicados_exp, 10))
} else {
  cat("\n✓ Cada ID tiene un único registro en tabla_exposicion\n")
}

# Estadísticas de exposición
estadisticas_exp <- tabla_exposicion %>%
  summarise(
    min_exp = min(exposicion_anos),
    max_exp = max(exposicion_anos),
    media_exp = mean(exposicion_anos),
    mediana_exp = median(exposicion_anos)
  )

cat("\n[1.3] Estadísticas de Exposición\n")
print(estadisticas_exp %>% as.data.frame())

base_modelacion$tabla_exposicion <- tabla_exposicion

################################################################################
# SECCIÓN 2: CONSTRUCCIÓN DE TABLA DE FRECUENCIA
################################################################################

print_section("SECCIÓN 2: CONSTRUCCI��N DE TABLA DE FRECUENCIA")

cat("\n[2.1] Cálculo de frecuencia por póliza\n")
cat("Definición: número de siniestros por id\n")

tabla_frecuencia <- sqldf("
  SELECT 
    id,
    COUNT(*) as num_siniestros,
    COUNT(DISTINCT sin) as siniestros_distintos,
    COUNT(DISTINCT cobertura) as coberturas_con_siniestro
  FROM siniestros
  WHERE id IS NOT NULL
  GROUP BY id
") %>%
  as.data.frame() %>%
  arrange(id)

cat("\n[2.2] Resumen de Frecuencia\n")
show_summary(tabla_frecuencia, "tabla_frecuencia")

# Validar unicidad
ids_duplicados_freq <- tabla_frecuencia %>%
  group_by(id) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)

if (nrow(ids_duplicados_freq) > 0) {
  cat("\n⚠️  ADVERTENCIA: Hay IDs duplicados en tabla_frecuencia\n")
} else {
  cat("\n✓ Cada ID tiene un único registro en tabla_frecuencia\n")
}

# Estadísticas de frecuencia
estadisticas_freq <- tabla_frecuencia %>%
  summarise(
    min_sin = min(num_siniestros),
    max_sin = max(num_siniestros),
    media_sin = mean(num_siniestros),
    mediana_sin = median(num_siniestros),
    porc_1_sin = round(100 * sum(num_siniestros == 1) / n(), 2)
  )

cat("\n[2.3] Estadísticas de Frecuencia\n")
cat("Min siniestros:       ", estadisticas_freq$min_sin, "\n")
cat("Max siniestros:       ", estadisticas_freq$max_sin, "\n")
cat("Media siniestros:     ", round(estadisticas_freq$media_sin, 2), "\n")
cat("Mediana siniestros:   ", estadisticas_freq$mediana_sin, "\n")
cat("% con 1 siniestro:    ", estadisticas_freq$porc_1_sin, "%\n")

base_modelacion$tabla_frecuencia <- tabla_frecuencia

################################################################################
# SECCIÓN 3: CONSTRUCCIÓN DE TABLA DE SEVERIDAD
################################################################################

print_section("SECCIÓN 3: CONSTRUCCIÓN DE TABLA DE SEVERIDAD")

cat("\n[3.1] Cálculo de severidad por póliza\n")
cat("Definiciones:\n")
cat("  • costo_total = suma de monto_ocu por id\n")
cat("  • severidad_promedio = costo_total / num_siniestros\n")

tabla_severidad <- sqldf("
  SELECT 
    id,
    COUNT(*) as num_siniestros,
    ROUND(SUM(monto_ocu), 2) as costo_total,
    ROUND(SUM(monto_ocu) / COUNT(*), 2) as severidad_promedio,
    ROUND(MIN(monto_ocu), 2) as monto_minimo,
    ROUND(MAX(monto_ocu), 2) as monto_maximo
  FROM siniestros
  WHERE id IS NOT NULL AND monto_ocu IS NOT NULL
  GROUP BY id
") %>%
  as.data.frame() %>%
  arrange(id)

cat("\n[3.2] Resumen de Severidad\n")
show_summary(tabla_severidad, "tabla_severidad")

# Validar unicidad
ids_duplicados_sev <- tabla_severidad %>%
  group_by(id) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)

if (nrow(ids_duplicados_sev) > 0) {
  cat("\n⚠️  ADVERTENCIA: Hay IDs duplicados en tabla_severidad\n")
} else {
  cat("\n✓ Cada ID tiene un único registro en tabla_severidad\n")
}

# Estadísticas de severidad
estadisticas_sev <- tabla_severidad %>%
  summarise(
    min_sev = min(severidad_promedio),
    max_sev = max(severidad_promedio),
    media_sev = mean(severidad_promedio),
    mediana_sev = median(severidad_promedio),
    min_costo = min(costo_total),
    max_costo = max(costo_total),
    media_costo = mean(costo_total),
    total_costo = sum(costo_total)
  )

cat("\n[3.3] Estadísticas de Severidad\n")
cat("Severidad Promedio:\n")
cat("  Min:     $", format(estadisticas_sev$min_sev, big.mark = ","), "\n")
cat("  Max:     $", format(estadisticas_sev$max_sev, big.mark = ","), "\n")
cat("  Media:   $", format(round(estadisticas_sev$media_sev, 2), big.mark = ","), "\n")
cat("  Mediana: $", format(estadisticas_sev$mediana_sev, big.mark = ","), "\n")
cat("\nCosto Total de Siniestros:\n")
cat("  Min:     $", format(estadisticas_sev$min_costo, big.mark = ","), "\n")
cat("  Max:     $", format(estadisticas_sev$max_costo, big.mark = ","), "\n")
cat("  Media:   $", format(round(estadisticas_sev$media_costo, 2), big.mark = ","), "\n")
cat("  Total:   $", format(round(estadisticas_sev$total_costo, 2), big.mark = ","), "\n")

base_modelacion$tabla_severidad <- tabla_severidad

################################################################################
# SECCIÓN 4: PREPARACIÓN DE TABLA EMISIONES
################################################################################

print_section("SECCIÓN 4: PREPARACIÓN DE TABLA EMISIONES")

cat("\n[4.1] Análisis de granularidad en EMISIONES\n")

# Verificar si hay múltiples registros por id
granularidad_emi <- sqldf("
  SELECT 
    COUNT(*) as total_registros,
    COUNT(DISTINCT id) as ids_unicos,
    COUNT(*) - COUNT(DISTINCT id) as registros_duplicados
  FROM emisiones
")

cat("Total registros:    ", granularidad_emi$total_registros, "\n")
cat("IDs únicos:         ", granularidad_emi$ids_unicos, "\n")
cat("Registros duplicados:", granularidad_emi$registros_duplicados, "\n")

if (granularidad_emi$registros_duplicados > 0) {
  cat("\n⚠️  Hay múltiples registros por ID en EMISIONES\n")
  cat("Acción: Agregar por id (tomar valores modales/máximos)\n")
  
  tabla_emisiones <- sqldf("
    SELECT 
      id,
      GROUP_CONCAT(DISTINCT cobertura, ', ') as coberturas,
      ROUND(SUM(prima_emi), 2) as prima_emi_total,
      ROUND(SUM(sa), 2) as sa_total,
      ROUND(AVG(deducible), 2) as deducible_promedio,
      MAX(entidad) as entidad,
      MAX(tipo_veh) as tipo_veh,
      MAX(marca_tipo) as marca_tipo,
      MAX(modelo) as modelo,
      MAX(uso) as uso,
      MAX(fec_emi) as fec_emi_ultima
    FROM emisiones
    WHERE id IS NOT NULL
    GROUP BY id
  ") %>%
    as.data.frame() %>%
    arrange(id)
  
  cat("\nNota: Para múltiples coberturas se concatenan.\n")
  cat("      Prima se suma, deducible se promedia.\n")
} else {
  cat("\n✓ Cada ID tiene un único registro en EMISIONES\n")
  
  tabla_emisiones <- sqldf("
    SELECT 
      id,
      cobertura,
      prima_emi,
      sa,
      deducible,
      entidad,
      tipo_veh,
      marca_tipo,
      modelo,
      uso,
      fec_emi
    FROM emisiones
    WHERE id IS NOT NULL
  ") %>%
    as.data.frame() %>%
    arrange(id)
}

cat("\n[4.2] Resumen de EMISIONES\n")
show_summary(tabla_emisiones, "tabla_emisiones")

base_modelacion$tabla_emisiones <- tabla_emisiones

################################################################################
# SECCIÓN 5: PREPARACIÓN DE TABLA GENERALES
################################################################################

print_section("SECCIÓN 5: PREPARACIÓN DE TABLA GENERALES")

cat("\n[5.1] Análisis de granularidad en GENERALES\n")

# Verificar duplicados
granularidad_gen <- sqldf("
  SELECT 
    COUNT(*) as total_registros,
    COUNT(DISTINCT id) as ids_unicos,
    COUNT(*) - COUNT(DISTINCT id) as registros_duplicados
  FROM generales
")

cat("Total registros:    ", granularidad_gen$total_registros, "\n")
cat("IDs únicos:         ", granularidad_gen$ids_unicos, "\n")
cat("Registros duplicados:", granularidad_gen$registros_duplicados, "\n")

if (granularidad_gen$registros_duplicados > 0) {
  cat("\n⚠️  Hay múltiples registros por ID en GENERALES\n")
  cat("Acción: Tomar el registro más reciente por ID\n")
  
  tabla_generales <- sqldf("
    SELECT 
      id,
      fecha_ini_vig,
      fecha_fin_vig,
      estatus,
      fec_can,
      entidad,
      tipo_veh,
      marca_tipo,
      cve_amis,
      modelo,
      uso,
      moneda
    FROM generales
    WHERE id IN (
      SELECT id FROM (
        SELECT id, fecha_fin_vig,
          ROW_NUMBER() OVER (PARTITION BY id ORDER BY fecha_fin_vig DESC) as rn
        FROM generales
      ) WHERE rn = 1
    )
  ") %>%
    as.data.frame() %>%
    arrange(id)
} else {
  cat("\n✓ Cada ID tiene un único registro en GENERALES\n")
  
  tabla_generales <- sqldf("
    SELECT 
      id,
      fecha_ini_vig,
      fecha_fin_vig,
      estatus,
      fec_can,
      entidad,
      tipo_veh,
      marca_tipo,
      cve_amis,
      modelo,
      uso,
      moneda
    FROM generales
    WHERE id IS NOT NULL
  ") %>%
    as.data.frame() %>%
    arrange(id)
}

cat("\n[5.2] Resumen de GENERALES\n")
show_summary(tabla_generales, "tabla_generales")

base_modelacion$tabla_generales <- tabla_generales

################################################################################
# SECCIÓN 6: CONSTRUCCIÓN DE BASE MAESTRA
################################################################################

print_section("SECCIÓN 6: CONSTRUCCIÓN DE BASE MAESTRA")

cat("\n[6.1] Uniendo tablas de manera progresiva\n\n")

# Paso 1: Base de GENERALES
cat("PASO 1: Tabla GENERALES como base\n")
base_maestra <- tabla_generales
cat("  Registros:", nrow(base_maestra), "| IDs únicos:", n_distinct(base_maestra$id), "\n")

# Paso 2: JOIN con EXPOSICIÓN
cat("\nPASO 2: LEFT JOIN con EXPOSICIÓN\n")
registros_antes <- nrow(base_maestra)
base_maestra <- base_maestra %>%
  left_join(tabla_exposicion %>% select(id, dias_vigencia, exposicion_anos),
            by = "id")
registros_despues <- nrow(base_maestra)
cat("  Registros antes: ", registros_antes, "\n")
cat("  Registros después:", registros_despues, "\n")
if (registros_antes == registros_despues) {
  cat("  ✓ Sin duplicaciones\n")
} else {
  cat("  ⚠️  Posible problema de duplicación\n")
}

# Paso 3: JOIN con EMISIONES
cat("\nPASO 3: LEFT JOIN con EMISIONES\n")
registros_antes <- nrow(base_maestra)
base_maestra <- base_maestra %>%
  left_join(tabla_emisiones %>% select(id, cobertura, prima_emi, sa, deducible, fec_emi),
            by = "id")
registros_despues <- nrow(base_maestra)
cat("  Registros antes: ", registros_antes, "\n")
cat("  Registros después:", registros_despues, "\n")
if (registros_antes == registros_despues) {
  cat("  ✓ Sin duplicaciones\n")
} else {
  cat("  ⚠️  Hay duplicaciones - Revisar tabla EMISIONES\n")
}

# Paso 4: JOIN con FRECUENCIA
cat("\nPASO 4: LEFT JOIN con FRECUENCIA\n")
registros_antes <- nrow(base_maestra)
base_maestra <- base_maestra %>%
  left_join(tabla_frecuencia %>% select(id, num_siniestros),
            by = "id")
registros_despues <- nrow(base_maestra)
cat("  Registros antes: ", registros_antes, "\n")
cat("  Registros después:", registros_despues, "\n")
if (registros_antes == registros_despues) {
  cat("  ✓ Sin duplicaciones\n")
} else {
  cat("  ⚠️  Posible problema de duplicación\n")
}

# Paso 5: JOIN con SEVERIDAD
cat("\nPASO 5: LEFT JOIN con SEVERIDAD\n")
registros_antes <- nrow(base_maestra)
base_maestra <- base_maestra %>%
  left_join(tabla_severidad %>% select(id, severidad_promedio, costo_total),
            by = "id")
registros_despues <- nrow(base_maestra)
cat("  Registros antes: ", registros_antes, "\n")
cat("  Registros después:", registros_despues, "\n")
if (registros_antes == registros_despues) {
  cat("  ✓ Sin duplicaciones\n")
} else {
  cat("  ⚠️  Posible problema de duplicación\n")
}

# Reemplazar NA en frecuencia y severidad con 0
cat("\n[6.2] Imputación de valores faltantes\n")
cat("Reemplazando NA con 0 en:\n")
cat("  • num_siniestros → 0 (pólizas sin siniestros)\n")
cat("  • severidad_promedio → 0\n")
cat("  • costo_total → 0\n")

base_maestra <- base_maestra %>%
  mutate(
    num_siniestros = ifelse(is.na(num_siniestros), 0, num_siniestros),
    severidad_promedio = ifelse(is.na(severidad_promedio), 0, severidad_promedio),
    costo_total = ifelse(is.na(costo_total), 0, costo_total),
    exposicion_anos = ifelse(is.na(exposicion_anos), 0, exposicion_anos),
    dias_vigencia = ifelse(is.na(dias_vigencia), 0, dias_vigencia)
  )

cat("\n[6.3] Seleccionar columnas finales\n")

base_maestra <- base_maestra %>%
  select(
    id,
    exposicion_anos,
    num_siniestros,
    severidad_promedio,
    costo_total,
    entidad,
    tipo_veh,
    marca_tipo,
    cve_amis,
    modelo,
    uso,
    cobertura,
    sa,
    deducible,
    prima_emi
  ) %>%
  arrange(id)

cat("✓ Base maestra construida\n")

base_modelacion$base_maestra <- base_maestra

################################################################################
# SECCIÓN 7: VALIDACIÓN DE LA BASE MAESTRA
################################################################################

print_section("SECCIÓN 7: VALIDACIÓN DE LA BASE MAESTRA")

cat("\n[7.1] Validación de Integridad\n")

# Verificar unicidad de IDs
ids_duplicados_base <- base_maestra %>%
  group_by(id) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)

if (nrow(ids_duplicados_base) > 0) {
  cat("\n⚠️  PROBLEMA: Hay IDs duplicados en la base maestra\n")
  cat("  Cantidad:", nrow(ids_duplicados_base), "\n")
  print(head(ids_duplicados_base, 10))
} else {
  cat("\n✓ Cada ID tiene un único registro\n")
}

# Verificar valores faltantes
cat("\n[7.2] Resumen de Valores Faltantes\n")

missing_summary <- base_maestra %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "cantidad_na") %>%
  mutate(pct_na = round(100 * cantidad_na / nrow(base_maestra), 2)) %>%
  filter(cantidad_na > 0) %>%
  arrange(desc(cantidad_na))

if (nrow(missing_summary) > 0) {
  print(missing_summary %>% as.data.frame())
} else {
  cat("\n✓ No hay valores faltantes\n")
}

# Verificar valores negativos o inválidos
cat("\n[7.3] Validación de Valores\n")

# Exposición negativa
exp_negativa <- base_maestra %>%
  filter(exposicion_anos < 0) %>%
  nrow()

if (exp_negativa > 0) {
  cat("\n⚠️  PROBLEMA: Hay", exp_negativa, "registros con exposición negativa\n")
} else {
  cat("\n✓ Todas las exposiciones son positivas\n")
}

# Prima negativa
prima_negativa <- base_maestra %>%
  filter(!is.na(prima_emi) & prima_emi < 0) %>%
  nrow()

if (prima_negativa > 0) {
  cat("⚠️  PROBLEMA: Hay", prima_negativa, "registros con prima negativa\n")
} else {
  cat("✓ Todas las primas son positivas\n")
}

# Severidad negativa
sev_negativa <- base_maestra %>%
  filter(!is.na(severidad_promedio) & severidad_promedio < 0) %>%
  nrow()

if (sev_negativa > 0) {
  cat("⚠️  PROBLEMA: Hay", sev_negativa, "registros con severidad negativa\n")
} else {
  cat("✓ Todas las severidades son positivas\n")
}

################################################################################
# SECCIÓN 8: ESTADÍSTICAS DESCRIPTIVAS
################################################################################

print_section("SECCIÓN 8: ESTADÍSTICAS DESCRIPTIVAS DE LA BASE MAESTRA")

cat("\n[8.1] Exposición (en años)\n")
stats_exp <- base_maestra %>%
  summarise(
    n = n(),
    min = min(exposicion_anos, na.rm = TRUE),
    q25 = quantile(exposicion_anos, 0.25, na.rm = TRUE),
    mediana = median(exposicion_anos, na.rm = TRUE),
    media = mean(exposicion_anos, na.rm = TRUE),
    q75 = quantile(exposicion_anos, 0.75, na.rm = TRUE),
    max = max(exposicion_anos, na.rm = TRUE),
    sd = sd(exposicion_anos, na.rm = TRUE)
  ) %>%
  round(4)

print(stats_exp %>% as.data.frame())

cat("\n[8.2] Frecuencia (número de siniestros)\n")
stats_freq <- base_maestra %>%
  summarise(
    n = n(),
    min = min(num_siniestros, na.rm = TRUE),
    q25 = quantile(num_siniestros, 0.25, na.rm = TRUE),
    mediana = median(num_siniestros, na.rm = TRUE),
    media = mean(num_siniestros, na.rm = TRUE),
    q75 = quantile(num_siniestros, 0.75, na.rm = TRUE),
    max = max(num_siniestros, na.rm = TRUE)
  ) %>%
  round(4)

print(stats_freq %>% as.data.frame())

cat("\n[8.3] Severidad Promedio (monto por siniestro, en $)\n")
stats_sev <- base_maestra %>%
  filter(severidad_promedio > 0) %>%
  summarise(
    n = n(),
    min = min(severidad_promedio, na.rm = TRUE),
    q25 = quantile(severidad_promedio, 0.25, na.rm = TRUE),
    mediana = median(severidad_promedio, na.rm = TRUE),
    media = mean(severidad_promedio, na.rm = TRUE),
    q75 = quantile(severidad_promedio, 0.75, na.rm = TRUE),
    max = max(severidad_promedio, na.rm = TRUE),
    sd = sd(severidad_promedio, na.rm = TRUE)
  ) %>%
  round(2)

print(stats_sev %>% as.data.frame())

cat("\n[8.4] Costo Total (suma de siniestros por póliza, en $)\n")
stats_costo <- base_maestra %>%
  filter(costo_total > 0) %>%
  summarise(
    n = n(),
    min = min(costo_total, na.rm = TRUE),
    q25 = quantile(costo_total, 0.25, na.rm = TRUE),
    mediana = median(costo_total, na.rm = TRUE),
    media = mean(costo_total, na.rm = TRUE),
    q75 = quantile(costo_total, 0.75, na.rm = TRUE),
    max = max(costo_total, na.rm = TRUE),
    sd = sd(costo_total, na.rm = TRUE),
    total = sum(costo_total, na.rm = TRUE)
  ) %>%
  round(2)

print(stats_costo %>% as.data.frame())

cat("\n[8.5] Prima Emitida (en $)\n")
stats_prima <- base_maestra %>%
  filter(!is.na(prima_emi)) %>%
  summarise(
    n = n(),
    min = min(prima_emi, na.rm = TRUE),
    q25 = quantile(prima_emi, 0.25, na.rm = TRUE),
    mediana = median(prima_emi, na.rm = TRUE),
    media = mean(prima_emi, na.rm = TRUE),
    q75 = quantile(prima_emi, 0.75, na.rm = TRUE),
    max = max(prima_emi, na.rm = TRUE),
    sd = sd(prima_emi, na.rm = TRUE),
    total = sum(prima_emi, na.rm = TRUE)
  ) %>%
  round(2)

print(stats_prima %>% as.data.frame())

base_modelacion$estadisticas <- list(
  exposicion = stats_exp,
  frecuencia = stats_freq,
  severidad = stats_sev,
  costo_total = stats_costo,
  prima = stats_prima
)

################################################################################
# SECCIÓN 9: DISTRIBUCIÓN DE SINIESTRALIDAD
################################################################################

print_section("SECCIÓN 9: DISTRIBUCIÓN DE SINIESTRALIDAD")

cat("\n[9.1] Distribución de Frecuencia (número de siniestros)\n")

dist_freq <- base_maestra %>%
  group_by(num_siniestros) %>%
  summarise(
    cantidad_polizas = n(),
    pct = round(100 * n() / nrow(base_maestra), 2),
    .groups = 'drop'
  ) %>%
  arrange(num_siniestros) %>%
  head(15)

print(dist_freq %>% as.data.frame())

cat("\n[9.2] Distribución de Pólizas sin Siniestros vs con Siniestros\n")

polizas_sin_sin <- base_maestra %>%
  filter(num_siniestros == 0) %>%
  nrow()

polizas_con_sin <- base_maestra %>%
  filter(num_siniestros > 0) %>%
  nrow()

pct_sin_sin <- round(100 * polizas_sin_sin / nrow(base_maestra), 2)
pct_con_sin <- round(100 * polizas_con_sin / nrow(base_maestra), 2)

cat("\nPólizas SIN siniestros:  ", format(polizas_sin_sin, big.mark = ","),
    "(", pct_sin_sin, "%)\n")
cat("Pólizas CON siniestros:  ", format(polizas_con_sin, big.mark = ","),
    "(", pct_con_sin, "%)\n")

################################################################################
# SECCIÓN 10: REPORTE FINAL
################################################################################

print_section("SECCIÓN 10: REPORTE FINAL")

cat("\n╔", strrep("═", 88), "╗\n", sep = "")
cat("║", strrep(" ", 25), "BASE MAESTRA - RESUMEN EJECUTIVO", strrep(" ", 30), "║\n")
cat("╚", strrep("═", 88), "╝\n", sep = "")

cat("\n[10.1] DIMENSIONES DE LA BASE MAESTRA\n")
cat("  • Número final de observaciones: ", format(nrow(base_maestra), big.mark = ","), "\n")
cat("  • Número de columnas:             ", ncol(base_maestra), "\n")
cat("  • IDs únicos:                     ", format(n_distinct(base_maestra$id), big.mark = ","), "\n")

cat("\n[10.2] COBERTURA DE DATOS\n")
cat("  • Pólizas sin siniestros:        ", format(polizas_sin_sin, big.mark = ","),
    " (", pct_sin_sin, "%)\n")
cat("  • Pólizas con siniestros:        ", format(polizas_con_sin, big.mark = ","),
    " (", pct_con_sin, "%)\n")

cat("\n[10.3] COMPOSICIÓN DE VARIABLES\n")

vars_ok <- base_maestra %>%
  select(everything()) %>%
  summarise(across(everything(), ~sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "no_na") %>%
  mutate(pct_completitud = round(100 * no_na / nrow(base_maestra), 1)) %>%
  arrange(pct_completitud)

print(vars_ok %>% as.data.frame())

cat("\n[10.4] PROBLEMAS DETECTADOS\n")

problemas_detectados <- 0

if (nrow(ids_duplicados_base) > 0) {
  problemas_detectados <- problemas_detectados + 1
  cat("\n  ⚠️  PROBLEMA #", problemas_detectados, ": IDs duplicados\n")
}

if (exp_negativa > 0) {
  problemas_detectados <- problemas_detectados + 1
  cat("\n  ⚠️  PROBLEMA #", problemas_detectados, ": Exposición negativa\n")
}

if (prima_negativa > 0) {
  problemas_detectados <- problemas_detectados + 1
  cat("\n  ⚠️  PROBLEMA #", problemas_detectados, ": Prima negativa\n")
}

if (sev_negativa > 0) {
  problemas_detectados <- problemas_detectados + 1
  cat("\n  ⚠️  PROBLEMA #", problemas_detectados, ": Severidad negativa\n")
}

if (nrow(missing_summary) > 0) {
  problemas_detectados <- problemas_detectados + 1
  cat("\n  ⚠️  PROBLEMA #", problemas_detectados, ": Valores faltantes\n")
}

if (problemas_detectados == 0) {
  cat("\n  ✓ No se detectaron problemas de calidad\n")
}

cat("\n[10.5] RECOMENDACIONES\n")

cat("\n  ✓ LISTO PARA MODELACIÓN\n")
cat("    La base maestra está lista para:\n")
cat("    • Construcción de modelos GLM\n")
cat("    • Árboles de decisión (CART, Random Forest)\n")
cat("    • Análisis de frecuencia y severidad\n")
cat("    • Cálculo de prima pura\n")

cat("\n  PRÓXIMOS PASOS:\n")
cat("    1. Crear variables dummy para categorías\n")
cat("    2. Escalar variables continuas\n")
cat("    3. Detectar y tratar outliers\n")
cat("    4. Separar en conjuntos train/test\n")
cat("    5. Construir modelos actuariales\n")

cat("\n")
cat("╔", strrep("═", 88), "╗\n", sep = "")
cat("║", format(paste("Generado:", Sys.time()), width = 88), "║\n")
cat("╚", strrep("═", 88), "╝\n", sep = "")

################################################################################
# SECCIÓN 11: EXPORTAR RESULTADOS
################################################################################

print_section("SECCIÓN 11: EXPORTAR RESULTADOS")

cat("\n[11.1] Guardar base maestra en Excel\n")

# Crear lista con múltiples hojas
export_list <- list(
  "base_maestra" = base_maestra,
  "tabla_exposicion" = tabla_exposicion,
  "tabla_frecuencia" = tabla_frecuencia,
  "tabla_severidad" = tabla_severidad,
  "estadisticas" = bind_rows(
    list(
      variable = "Exposición",
      n = nrow(stats_exp),
      media = stats_exp$media,
      min = stats_exp$min,
      max = stats_exp$max
    )
  )
)

# Guardar base maestra principal
write_xlsx(base_maestra, 
           path = "reportes/base_maestra_tarificacion.xlsx")

cat("✓ Base maestra exportada: reportes/base_maestra_tarificacion.xlsx\n")

# Guardar tablas auxiliares
write_xlsx(tabla_exposicion, 
           path = "reportes/tabla_exposicion.xlsx")
cat("✓ Tabla exposición exportada: reportes/tabla_exposicion.xlsx\n")

write_xlsx(tabla_frecuencia, 
           path = "reportes/tabla_frecuencia.xlsx")
cat("✓ Tabla frecuencia exportada: reportes/tabla_frecuencia.xlsx\n")

write_xlsx(tabla_severidad, 
           path = "reportes/tabla_severidad.xlsx")
cat("✓ Tabla severidad exportada: reportes/tabla_severidad.xlsx\n")

cat("\n[11.2] Guardar resumen de estadísticas\n")

# Crear resumen de estadísticas
resumen_stats <- data.frame(
  Metrica = c(
    "Exposición (años) - Media",
    "Exposición (años) - Min",
    "Exposición (años) - Max",
    "Frecuencia - Media",
    "Frecuencia - Max",
    "Severidad - Media",
    "Severidad - Max",
    "Costo Total - Total",
    "Prima Emitida - Total",
    "Pólizas sin Siniestros",
    "Pólizas con Siniestros"
  ),
  Valor = c(
    round(stats_exp$media, 4),
    round(stats_exp$min, 4),
    round(stats_exp$max, 4),
    round(stats_freq$media, 2),
    stats_freq$max,
    round(stats_sev$media, 2),
    round(stats_sev$max, 2),
    round(stats_costo$total, 2),
    round(stats_prima$total, 2),
    polizas_sin_sin,
    polizas_con_sin
  )
)

write_xlsx(resumen_stats, 
           path = "reportes/resumen_estadisticas.xlsx")
cat("✓ Resumen de estadísticas exportado: reportes/resumen_estadisticas.xlsx\n")

cat("\n[11.3] Información de la sesión\n")
cat("✓ Objeto 'base_modelacion' contiene todos los resultados\n")
cat("  Use: base_modelacion$[nombre] para acceder\n")
cat("  Elementos disponibles:\n")
for (i in seq_along(base_modelacion)) {
  cat("  •", names(base_modelacion)[i], "\n")
}

cat("\n" %+paste% strrep("=", 90) %+paste% "\n")
