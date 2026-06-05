################################################################################
# AUDITORÍA COMPLETA DE BASES DE DATOS - TARIFICACIÓN DE SEGUROS AUTOS
################################################################################
# Objetivo: Explorar y entender la estructura de los datos antes de construir
# modelos actuariales. Se utiliza sqldf para todas las consultas.
#
# Tablas disponibles:
# 1. generales - Información general de pólizas
# 2. siniestros - Información de siniestros reportados
# 3. emisiones - Información de emisiones y coberturas
# 4. archivo_auxiliar - Información auxiliar por cobertura/entidad/tipo_veh
#
# Librerías autorizadas: sqldf, readxl, writexl, dplyr, tidyverse, lubridate,
#                        janitor, ggplot2, caret, MASS, rpart, rpart.plot,
#                        ranger, Metrics
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

# Configurar opciones de sqldf para mejor desempeño
options(sqldf.driver = "SQLite")

# Crear directorio para guardar reportes (si no existe)
if (!dir.exists("reportes")) {
  dir.create("reportes")
}

# Función auxiliar para imprimir títulos
print_section <- function(title) {
  cat("\n")
  cat(strrep("=", 85), "\n")
  cat(title, "\n")
  cat(strrep("=", 85), "\n")
}

print_subsection <- function(subtitle) {
  cat("\n", strrep("-", 85), "\n")
  cat(subtitle, "\n")
  cat(strrep("-", 85), "\n")
}

# Función para mostrar resultados
show_result <- function(data, max_rows = 10, title = NULL) {
  if (!is.null(title)) cat("\n", title, "\n")
  if (nrow(data) > max_rows) {
    cat("(Mostrando primeros", max_rows, "de", nrow(data), "registros)\n")
    print(head(data, max_rows) %>% as.data.frame())
    cat("...\n\n")
  } else {
    print(data %>% as.data.frame())
    cat("\n")
  }
}

# Inicializar lista para guardar resultados
auditoria_resultados <- list()

################################################################################
# SECCIÓN 1: ANÁLISIS GENERAL DE CADA TABLA
################################################################################

print_section("SECCIÓN 1: ANÁLISIS GENERAL Y ESTADÍSTICAS BÁSICAS")

# ===========================
# 1.1 TABLA GENERALES
# ===========================

print_subsection("1.1 TABLA GENERALES")

# Estadísticas básicas
cat("\n[1.1.1] Estadísticas Básicas\n")

resumen_generales <- sqldf("
  SELECT 
    (SELECT COUNT(*) FROM generales) as total_registros,
    (SELECT COUNT(DISTINCT id) FROM generales) as ids_unicos,
    (SELECT COUNT(*) FROM generales) - 
    (SELECT COUNT(DISTINCT id) FROM generales) as ids_duplicados,
    ROUND(100.0 * (
      (SELECT COUNT(*) FROM generales) - 
      (SELECT COUNT(DISTINCT id) FROM generales)
    ) / (SELECT COUNT(*) FROM generales), 2) as pct_duplicacion
")

cat("Total de registros:       ", resumen_generales$total_registros, "\n")
cat("IDs únicos:               ", resumen_generales$ids_unicos, "\n")
cat("IDs duplicados:           ", resumen_generales$ids_duplicados, "\n")
cat("Porcentaje duplicación:   ", resumen_generales$pct_duplicacion, "%\n")

auditoria_resultados$generales_resumen <- resumen_generales

# Valores faltantes
cat("\n[1.1.2] Valores Faltantes por Columna\n")

columnas_generales <- c("id", "fecha_ini_vig", "fecha_fin_vig", "estatus", 
                        "fec_can", "entidad", "tipo_veh", "marca_tipo", 
                        "cve_amis", "modelo", "uso", "moneda")

missing_generales <- sqldf("
  SELECT 
    'id' as columna,
    SUM(CASE WHEN id IS NULL THEN 1 ELSE 0 END) as cantidad_nulos
  FROM generales
  UNION ALL
  SELECT 'fecha_ini_vig',
    SUM(CASE WHEN fecha_ini_vig IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'fecha_fin_vig',
    SUM(CASE WHEN fecha_fin_vig IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'estatus',
    SUM(CASE WHEN estatus IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'fec_can',
    SUM(CASE WHEN fec_can IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'entidad',
    SUM(CASE WHEN entidad IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'tipo_veh',
    SUM(CASE WHEN tipo_veh IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'marca_tipo',
    SUM(CASE WHEN marca_tipo IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'cve_amis',
    SUM(CASE WHEN cve_amis IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'modelo',
    SUM(CASE WHEN modelo IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'uso',
    SUM(CASE WHEN uso IS NULL THEN 1 ELSE 0 END)
  FROM generales
  UNION ALL
  SELECT 'moneda',
    SUM(CASE WHEN moneda IS NULL THEN 1 ELSE 0 END)
  FROM generales
")

missing_generales <- missing_generales %>%
  mutate(pct_nulos = round(100.0 * cantidad_nulos / resumen_generales$total_registros, 2))

show_result(missing_generales, max_rows = 20)
auditoria_resultados$generales_missing <- missing_generales

# ===========================
# 1.2 TABLA SINIESTROS
# ===========================

print_subsection("1.2 TABLA SINIESTROS")

cat("\n[1.2.1] Estadísticas Básicas\n")

resumen_siniestros <- sqldf("
  SELECT 
    (SELECT COUNT(*) FROM siniestros) as total_registros,
    (SELECT COUNT(DISTINCT id) FROM siniestros) as ids_unicos,
    (SELECT COUNT(*) FROM siniestros) - 
    (SELECT COUNT(DISTINCT id) FROM siniestros) as ids_duplicados,
    ROUND(100.0 * (
      (SELECT COUNT(*) FROM siniestros) - 
      (SELECT COUNT(DISTINCT id) FROM siniestros)
    ) / (SELECT COUNT(*) FROM siniestros), 2) as pct_duplicacion
")

cat("Total de registros:       ", resumen_siniestros$total_registros, "\n")
cat("IDs únicos:               ", resumen_siniestros$ids_unicos, "\n")
cat("IDs duplicados:           ", resumen_siniestros$ids_duplicados, "\n")
cat("Porcentaje duplicación:   ", resumen_siniestros$pct_duplicacion, "%\n")

auditoria_resultados$siniestros_resumen <- resumen_siniestros

cat("\n[1.2.2] Valores Faltantes por Columna\n")

missing_siniestros <- sqldf("
  SELECT 
    'id' as columna,
    SUM(CASE WHEN id IS NULL THEN 1 ELSE 0 END) as cantidad_nulos
  FROM siniestros
  UNION ALL
  SELECT 'sin',
    SUM(CASE WHEN sin IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
  UNION ALL
  SELECT 'cobertura',
    SUM(CASE WHEN cobertura IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
  UNION ALL
  SELECT 'fec_ocu',
    SUM(CASE WHEN fec_ocu IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
  UNION ALL
  SELECT 'causa',
    SUM(CASE WHEN causa IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
  UNION ALL
  SELECT 'ent_mun',
    SUM(CASE WHEN ent_mun IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
  UNION ALL
  SELECT 'tipo_perd',
    SUM(CASE WHEN tipo_perd IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
  UNION ALL
  SELECT 'monto_de',
    SUM(CASE WHEN monto_de IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
  UNION ALL
  SELECT 'monto_ocu',
    SUM(CASE WHEN monto_ocu IS NULL THEN 1 ELSE 0 END)
  FROM siniestros
")

missing_siniestros <- missing_siniestros %>%
  mutate(pct_nulos = round(100.0 * cantidad_nulos / resumen_siniestros$total_registros, 2))

show_result(missing_siniestros, max_rows = 20)
auditoria_resultados$siniestros_missing <- missing_siniestros

# ===========================
# 1.3 TABLA EMISIONES
# ===========================

print_subsection("1.3 TABLA EMISIONES")

cat("\n[1.3.1] Estadísticas Básicas\n")

resumen_emisiones <- sqldf("
  SELECT 
    (SELECT COUNT(*) FROM emisiones) as total_registros,
    (SELECT COUNT(DISTINCT id) FROM emisiones) as ids_unicos,
    (SELECT COUNT(*) FROM emisiones) - 
    (SELECT COUNT(DISTINCT id) FROM emisiones) as ids_duplicados,
    ROUND(100.0 * (
      (SELECT COUNT(*) FROM emisiones) - 
      (SELECT COUNT(DISTINCT id) FROM emisiones)
    ) / (SELECT COUNT(*) FROM emisiones), 2) as pct_duplicacion
")

cat("Total de registros:       ", resumen_emisiones$total_registros, "\n")
cat("IDs únicos:               ", resumen_emisiones$ids_unicos, "\n")
cat("IDs duplicados:           ", resumen_emisiones$ids_duplicados, "\n")
cat("Porcentaje duplicación:   ", resumen_emisiones$pct_duplicacion, "%\n")

auditoria_resultados$emisiones_resumen <- resumen_emisiones

cat("\n[1.3.2] Valores Faltantes por Columna\n")

missing_emisiones <- sqldf("
  SELECT 
    'id' as columna,
    SUM(CASE WHEN id IS NULL THEN 1 ELSE 0 END) as cantidad_nulos
  FROM emisiones
  UNION ALL
  SELECT 'cobertura',
    SUM(CASE WHEN cobertura IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'fec_emi',
    SUM(CASE WHEN fec_emi IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'sa',
    SUM(CASE WHEN sa IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'deducible',
    SUM(CASE WHEN deducible IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'entidad',
    SUM(CASE WHEN entidad IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'tipo_veh',
    SUM(CASE WHEN tipo_veh IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'marca_tipo',
    SUM(CASE WHEN marca_tipo IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'id_vehiculo',
    SUM(CASE WHEN id_vehiculo IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'modelo',
    SUM(CASE WHEN modelo IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'uso',
    SUM(CASE WHEN uso IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
  UNION ALL
  SELECT 'prima_emi',
    SUM(CASE WHEN prima_emi IS NULL THEN 1 ELSE 0 END)
  FROM emisiones
")

missing_emisiones <- missing_emisiones %>%
  mutate(pct_nulos = round(100.0 * cantidad_nulos / resumen_emisiones$total_registros, 2))

show_result(missing_emisiones, max_rows = 20)
auditoria_resultados$emisiones_missing <- missing_emisiones

# ===========================
# 1.4 TABLA ARCHIVO_AUXILIAR
# ===========================

print_subsection("1.4 TABLA ARCHIVO_AUXILIAR")

cat("\n[1.4.1] Estadísticas Básicas\n")

resumen_auxiliar <- sqldf("
  SELECT 
    (SELECT COUNT(*) FROM archivo_auxiliar) as total_registros,
    (SELECT COUNT(DISTINCT cobertura) FROM archivo_auxiliar) as coberturas_unicas,
    (SELECT COUNT(DISTINCT entidad) FROM archivo_auxiliar) as entidades_unicas,
    (SELECT COUNT(DISTINCT tipo_veh) FROM archivo_auxiliar) as tipos_veh_unicos
")

cat("Total de registros:       ", resumen_auxiliar$total_registros, "\n")
cat("Coberturas únicas:        ", resumen_auxiliar$coberturas_unicas, "\n")
cat("Entidades únicas:         ", resumen_auxiliar$entidades_unicas, "\n")
cat("Tipos de vehículo únicos: ", resumen_auxiliar$tipos_veh_unicos, "\n")

auditoria_resultados$auxiliar_resumen <- resumen_auxiliar

cat("\n[1.4.2] Valores Faltantes por Columna\n")

missing_auxiliar <- sqldf("
  SELECT 
    'cobertura' as columna,
    SUM(CASE WHEN cobertura IS NULL THEN 1 ELSE 0 END) as cantidad_nulos
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'entidad',
    SUM(CASE WHEN entidad IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'tipo_veh',
    SUM(CASE WHEN tipo_veh IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'comp1',
    SUM(CASE WHEN comp1 IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'comp2',
    SUM(CASE WHEN comp2 IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'comp3',
    SUM(CASE WHEN comp3 IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'pos1',
    SUM(CASE WHEN pos1 IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'pos2',
    SUM(CASE WHEN pos2 IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'pos3',
    SUM(CASE WHEN pos3 IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'DA',
    SUM(CASE WHEN DA IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
  UNION ALL
  SELECT 'DR',
    SUM(CASE WHEN DR IS NULL THEN 1 ELSE 0 END)
  FROM archivo_auxiliar
")

missing_auxiliar <- missing_auxiliar %>%
  mutate(pct_nulos = round(100.0 * cantidad_nulos / resumen_auxiliar$total_registros, 2))

show_result(missing_auxiliar, max_rows = 20)
auditoria_resultados$auxiliar_missing <- missing_auxiliar

################################################################################
# SECCIÓN 2: ANÁLISIS DE GRANULARIDAD Y UNICIDAD
################################################################################

print_section("SECCIÓN 2: ANÁLISIS DE GRANULARIDAD Y UNICIDAD")

# ===========================
# 2.1 GRANULARIDAD DE GENERALES
# ===========================

print_subsection("2.1 Análisis de Granularidad: GENERALES")

cat("\n[2.1.1] ¿Es 'id' la clave primaria en GENERALES?\n")

es_clave_primaria_generales <- ifelse(
  resumen_generales$total_registros == resumen_generales$ids_unicos,
  "SÍ - cada id aparece una sola vez",
  "NO - el id se repite múltiples veces"
)

cat("Conclusión:", es_clave_primaria_generales, "\n")
cat("Granularidad: NIVEL PÓLIZA (si id es único)\n")

cat("\n[2.1.2] Top 20 IDs con más repeticiones en GENERALES\n")

top_dup_gen <- sqldf("
  SELECT 
    id,
    COUNT(*) as frecuencia,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM generales), 2) as pct_total
  FROM generales
  GROUP BY id
  HAVING COUNT(*) > 1
  ORDER BY frecuencia DESC
  LIMIT 20
")

if (nrow(top_dup_gen) > 0) {
  show_result(top_dup_gen, max_rows = 20,
              title = paste("Total de IDs duplicados:", nrow(top_dup_gen)))
  auditoria_resultados$generales_top_dup <- top_dup_gen
} else {
  cat("✓ No hay IDs duplicados en GENERALES\n")
  auditoria_resultados$generales_top_dup <- NULL
}

# ===========================
# 2.2 GRANULARIDAD DE SINIESTROS
# ===========================

print_subsection("2.2 Análisis de Granularidad: SINIESTROS")

cat("\n[2.2.1] ¿Cuál es la granularidad de SINIESTROS?\n")

granularidad_siniestros <- sqldf("
  SELECT 
    'Total registros' as metrica,
    (SELECT COUNT(*) FROM siniestros) as cantidad
  UNION ALL
  SELECT 'IDs únicos',
    (SELECT COUNT(DISTINCT id) FROM siniestros)
  UNION ALL
  SELECT 'Siniestros únicos (sin)',
    (SELECT COUNT(DISTINCT sin) FROM siniestros)
  UNION ALL
  SELECT 'Combinaciones id-cobertura',
    (SELECT COUNT(DISTINCT id || '-' || cobertura) FROM siniestros)
")

show_result(granularidad_siniestros)

cat("[2.2.2] Análisis: ", "\n")
cat("  • La tabla está a NIVEL SINIESTRO (cada fila es un siniestro único)\n")
cat("  • Pueden haber múltiples siniestros por id y cobertura\n")

cat("\n[2.2.3] Top 20 IDs con más siniestros\n")

top_sin <- sqldf("
  SELECT 
    id,
    COUNT(*) as num_siniestros,
    COUNT(DISTINCT sin) as siniestros_distintos,
    COUNT(DISTINCT cobertura) as coberturas_afectadas,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM siniestros), 2) as pct_total
  FROM siniestros
  GROUP BY id
  ORDER BY num_siniestros DESC
  LIMIT 20
")

show_result(top_sin, max_rows = 20)
auditoria_resultados$siniestros_top_sin <- top_sin

# ===========================
# 2.3 GRANULARIDAD DE EMISIONES
# ===========================

print_subsection("2.3 Análisis de Granularidad: EMISIONES")

cat("\n[2.3.1] ¿Cuál es la granularidad de EMISIONES?\n")

granularidad_emisiones <- sqldf("
  SELECT 
    'Total registros' as metrica,
    (SELECT COUNT(*) FROM emisiones) as cantidad
  UNION ALL
  SELECT 'IDs únicos',
    (SELECT COUNT(DISTINCT id) FROM emisiones)
  UNION ALL
  SELECT 'Combinaciones id-cobertura únicas',
    (SELECT COUNT(DISTINCT id || '-' || cobertura) FROM emisiones)
")

show_result(granularidad_emisiones)

cat("[2.3.2] Análisis: \n")
if (resumen_emisiones$ids_duplicados > 0) {
  cat("  • La tabla está a NIVEL PÓLIZA-COBERTURA\n")
  cat("  • Pueden haber múltiples registros por id (múltiples coberturas)\n")
} else {
  cat("  • Cada id tiene exactamente una cobertura\n")
}

cat("\n[2.3.3] Top 20 IDs con más coberturas\n")

top_cob <- sqldf("
  SELECT 
    id,
    COUNT(*) as num_registros,
    COUNT(DISTINCT cobertura) as num_coberturas_distintas,
    GROUP_CONCAT(DISTINCT cobertura, ', ') as coberturas,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM emisiones), 2) as pct_total
  FROM emisiones
  GROUP BY id
  ORDER BY num_registros DESC
  LIMIT 20
")

show_result(top_cob, max_rows = 20)
auditoria_resultados$emisiones_top_cob <- top_cob

################################################################################
# SECCIÓN 3: ANÁLISIS DE VARIABLES CLAVE
################################################################################

print_section("SECCIÓN 3: TABLAS DE FRECUENCIA - VARIABLES CLAVE")

# ===========================
# 3.1 COBERTURA
# ===========================

print_subsection("3.1 Variable: COBERTURA")

cat("\n[3.1.1] Distribución en EMISIONES\n")

dist_cob_emi <- sqldf("
  SELECT 
    cobertura,
    COUNT(*) as frecuencia,
    COUNT(DISTINCT id) as ids_con_cobertura,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM emisiones), 2) as pct
  FROM emisiones
  WHERE cobertura IS NOT NULL
  GROUP BY cobertura
  ORDER BY frecuencia DESC
")

show_result(dist_cob_emi)
auditoria_resultados$cobertura_emisiones <- dist_cob_emi

cat("[3.1.2] Distribución en SINIESTROS\n")

dist_cob_sin <- sqldf("
  SELECT 
    cobertura,
    COUNT(*) as num_siniestros,
    COUNT(DISTINCT id) as ids_con_siniestro,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM siniestros), 2) as pct,
    ROUND(AVG(monto_ocu), 2) as monto_promedio
  FROM siniestros
  WHERE cobertura IS NOT NULL
  GROUP BY cobertura
  ORDER BY num_siniestros DESC
")

show_result(dist_cob_sin)
auditoria_resultados$cobertura_siniestros <- dist_cob_sin

# ===========================
# 3.2 ENTIDAD
# ===========================

print_subsection("3.2 Variable: ENTIDAD")

cat("\n[3.2.1] Distribución en GENERALES\n")

dist_ent_gen <- sqldf("
  SELECT 
    entidad,
    COUNT(*) as frecuencia,
    COUNT(DISTINCT id) as ids_unicos,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM generales), 2) as pct
  FROM generales
  WHERE entidad IS NOT NULL
  GROUP BY entidad
  ORDER BY frecuencia DESC
  LIMIT 20
")

show_result(dist_ent_gen, max_rows = 15)
auditoria_resultados$entidad_generales <- dist_ent_gen

cat("[3.2.2] Distribución en EMISIONES\n")

dist_ent_emi <- sqldf("
  SELECT 
    entidad,
    COUNT(*) as frecuencia,
    COUNT(DISTINCT id) as ids_unicos,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM emisiones), 2) as pct
  FROM emisiones
  WHERE entidad IS NOT NULL
  GROUP BY entidad
  ORDER BY frecuencia DESC
  LIMIT 20
")

show_result(dist_ent_emi, max_rows = 15)
auditoria_resultados$entidad_emisiones <- dist_ent_emi

# ===========================
# 3.3 TIPO_VEH
# ===========================

print_subsection("3.3 Variable: TIPO_VEH (Tipo de Vehículo)")

cat("\n[3.3.1] Distribución en GENERALES\n")

dist_tv_gen <- sqldf("
  SELECT 
    tipo_veh,
    COUNT(*) as frecuencia,
    COUNT(DISTINCT id) as ids_unicos,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM generales), 2) as pct
  FROM generales
  WHERE tipo_veh IS NOT NULL
  GROUP BY tipo_veh
  ORDER BY frecuencia DESC
")

show_result(dist_tv_gen)
auditoria_resultados$tipo_veh_generales <- dist_tv_gen

cat("[3.3.2] Distribución en EMISIONES\n")

dist_tv_emi <- sqldf("
  SELECT 
    tipo_veh,
    COUNT(*) as frecuencia,
    COUNT(DISTINCT id) as ids_unicos,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM emisiones), 2) as pct
  FROM emisiones
  WHERE tipo_veh IS NOT NULL
  GROUP BY tipo_veh
  ORDER BY frecuencia DESC
")

show_result(dist_tv_emi)
auditoria_resultados$tipo_veh_emisiones <- dist_tv_emi

# ===========================
# 3.4 USO
# ===========================

print_subsection("3.4 Variable: USO")

cat("\n[3.4.1] Distribución en GENERALES\n")

dist_uso_gen <- sqldf("
  SELECT 
    uso,
    COUNT(*) as frecuencia,
    COUNT(DISTINCT id) as ids_unicos,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM generales), 2) as pct
  FROM generales
  WHERE uso IS NOT NULL
  GROUP BY uso
  ORDER BY frecuencia DESC
")

show_result(dist_uso_gen)
auditoria_resultados$uso_generales <- dist_uso_gen

cat("[3.4.2] Distribución en EMISIONES\n")

dist_uso_emi <- sqldf("
  SELECT 
    uso,
    COUNT(*) as frecuencia,
    COUNT(DISTINCT id) as ids_unicos,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM emisiones), 2) as pct
  FROM emisiones
  WHERE uso IS NOT NULL
  GROUP BY uso
  ORDER BY frecuencia DESC
")

show_result(dist_uso_emi)
auditoria_resultados$uso_emisiones <- dist_uso_emi

# ===========================
# 3.5 TIPO_PERD (Tipo de Pérdida)
# ===========================

print_subsection("3.5 Variable: TIPO_PERD (Tipo de Pérdida)")

cat("\n[3.5.1] Distribución en SINIESTROS\n")

dist_tp_sin <- sqldf("
  SELECT 
    tipo_perd,
    COUNT(*) as num_siniestros,
    COUNT(DISTINCT id) as ids_afectados,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM siniestros), 2) as pct,
    ROUND(AVG(monto_ocu), 2) as monto_promedio,
    ROUND(SUM(monto_ocu), 2) as monto_total
  FROM siniestros
  WHERE tipo_perd IS NOT NULL
  GROUP BY tipo_perd
  ORDER BY num_siniestros DESC
")

show_result(dist_tp_sin)
auditoria_resultados$tipo_perd_siniestros <- dist_tp_sin

################################################################################
# SECCIÓN 4: ANÁLISIS DE CONSISTENCIA ENTRE TABLAS
################################################################################

print_section("SECCIÓN 4: ANÁLISIS DE CONSISTENCIA ENTRE TABLAS")

# ===========================
# 4.1 GENERALES vs EMISIONES
# ===========================

print_subsection("4.1 Consistencia: GENERALES vs EMISIONES")

cat("\n[4.1.1] IDs en GENERALES pero NO en EMISIONES\n")

gen_no_emi_ids <- sqldf("
  SELECT COUNT(DISTINCT g.id) as cantidad_ids
  FROM (SELECT DISTINCT id FROM generales) g
  LEFT JOIN (SELECT DISTINCT id FROM emisiones) e ON g.id = e.id
  WHERE e.id IS NULL
")

gen_no_emi_regs <- sqldf("
  SELECT COUNT(*) as cantidad_registros
  FROM generales g
  LEFT JOIN (SELECT DISTINCT id FROM emisiones) e ON g.id = e.id
  WHERE e.id IS NULL
")

cat("IDs sin emisión:      ", gen_no_emi_ids$cantidad_ids, "\n")
cat("Registros afectados:  ", gen_no_emi_regs$cantidad_registros, "\n")
cat("Porcentaje:           ",
    round(100 * gen_no_emi_regs$cantidad_registros / resumen_generales$total_registros, 2), "%\n")

auditoria_resultados$gen_no_emi <- list(
  ids = gen_no_emi_ids$cantidad_ids,
  regs = gen_no_emi_regs$cantidad_registros
)

cat("\n[4.1.2] IDs en EMISIONES pero NO en GENERALES\n")

emi_no_gen_ids <- sqldf("
  SELECT COUNT(DISTINCT e.id) as cantidad_ids
  FROM (SELECT DISTINCT id FROM emisiones) e
  LEFT JOIN (SELECT DISTINCT id FROM generales) g ON e.id = g.id
  WHERE g.id IS NULL
")

emi_no_gen_regs <- sqldf("
  SELECT COUNT(*) as cantidad_registros
  FROM emisiones e
  LEFT JOIN (SELECT DISTINCT id FROM generales) g ON e.id = g.id
  WHERE g.id IS NULL
")

cat("IDs sin datos generales:  ", emi_no_gen_ids$cantidad_ids, "\n")
cat("Registros afectados:      ", emi_no_gen_regs$cantidad_registros, "\n")
cat("Porcentaje:               ",
    round(100 * emi_no_gen_regs$cantidad_registros / resumen_emisiones$total_registros, 2), "%\n")

auditoria_resultados$emi_no_gen <- list(
  ids = emi_no_gen_ids$cantidad_ids,
  regs = emi_no_gen_regs$cantidad_registros
)

# ===========================
# 4.2 GENERALES vs SINIESTROS
# ===========================

print_subsection("4.2 Consistencia: GENERALES vs SINIESTROS")

cat("\n[4.2.1] IDs en GENERALES pero NO en SINIESTROS (esperado - sin siniestros)\n")

gen_no_sin_ids <- sqldf("
  SELECT COUNT(DISTINCT g.id) as cantidad_ids
  FROM (SELECT DISTINCT id FROM generales) g
  LEFT JOIN (SELECT DISTINCT id FROM siniestros) s ON g.id = s.id
  WHERE s.id IS NULL
")

gen_no_sin_regs <- sqldf("
  SELECT COUNT(*) as cantidad_registros
  FROM generales g
  LEFT JOIN (SELECT DISTINCT id FROM siniestros) s ON g.id = s.id
  WHERE s.id IS NULL
")

cat("IDs sin siniestros:   ", gen_no_sin_ids$cantidad_ids, "\n")
cat("Registros afectados:  ", gen_no_sin_regs$cantidad_registros, "\n")
cat("Porcentaje:           ",
    round(100 * gen_no_sin_regs$cantidad_registros / resumen_generales$total_registros, 2), "%\n")

auditoria_resultados$gen_no_sin <- list(
  ids = gen_no_sin_ids$cantidad_ids,
  regs = gen_no_sin_regs$cantidad_registros
)

cat("\n[4.2.2] IDs en SINIESTROS pero NO en GENERALES (PROBLEMA)\n")

sin_no_gen_ids <- sqldf("
  SELECT COUNT(DISTINCT s.id) as cantidad_ids
  FROM (SELECT DISTINCT id FROM siniestros) s
  LEFT JOIN (SELECT DISTINCT id FROM generales) g ON s.id = g.id
  WHERE g.id IS NULL
")

sin_no_gen_regs <- sqldf("
  SELECT COUNT(*) as cantidad_registros
  FROM siniestros s
  LEFT JOIN (SELECT DISTINCT id FROM generales) g ON s.id = g.id
  WHERE g.id IS NULL
")

cat("IDs sin datos generales:  ", sin_no_gen_ids$cantidad_ids, "\n")
cat("Registros afectados:      ", sin_no_gen_regs$cantidad_registros, "\n")
cat("Porcentaje:               ",
    round(100 * sin_no_gen_regs$cantidad_registros / resumen_siniestros$total_registros, 2), "%\n")

if (sin_no_gen_ids$cantidad_ids > 0) {
  cat("⚠️  PROBLEMA: Hay siniestros sin póliza correspondiente en GENERALES\n")
}

auditoria_resultados$sin_no_gen <- list(
  ids = sin_no_gen_ids$cantidad_ids,
  regs = sin_no_gen_regs$cantidad_registros
)

# ===========================
# 4.3 EMISIONES vs SINIESTROS
# ===========================

print_subsection("4.3 Consistencia: EMISIONES vs SINIESTROS")

cat("\n[4.3.1] IDs en EMISIONES pero NO en SINIESTROS (sin siniestros - NORMAL)\n")

emi_no_sin_ids <- sqldf("
  SELECT COUNT(DISTINCT e.id) as cantidad_ids
  FROM (SELECT DISTINCT id FROM emisiones) e
  LEFT JOIN (SELECT DISTINCT id FROM siniestros) s ON e.id = s.id
  WHERE s.id IS NULL
")

emi_no_sin_regs <- sqldf("
  SELECT COUNT(*) as cantidad_registros
  FROM emisiones e
  LEFT JOIN (SELECT DISTINCT id FROM siniestros) s ON e.id = s.id
  WHERE s.id IS NULL
")

cat("IDs sin siniestros:   ", emi_no_sin_ids$cantidad_ids, "\n")
cat("Registros afectados:  ", emi_no_sin_regs$cantidad_registros, "\n")
cat("Porcentaje:           ",
    round(100 * emi_no_sin_regs$cantidad_registros / resumen_emisiones$total_registros, 2), "%\n")

auditoria_resultados$emi_no_sin <- list(
  ids = emi_no_sin_ids$cantidad_ids,
  regs = emi_no_sin_regs$cantidad_registros
)

cat("\n[4.3.2] IDs en SINIESTROS pero NO en EMISIONES (PROBLEMA)\n")

sin_no_emi_ids <- sqldf("
  SELECT COUNT(DISTINCT s.id) as cantidad_ids
  FROM (SELECT DISTINCT id FROM siniestros) s
  LEFT JOIN (SELECT DISTINCT id FROM emisiones) e ON s.id = e.id
  WHERE e.id IS NULL
")

sin_no_emi_regs <- sqldf("
  SELECT COUNT(*) as cantidad_registros
  FROM siniestros s
  LEFT JOIN (SELECT DISTINCT id FROM emisiones) e ON s.id = e.id
  WHERE e.id IS NULL
")

cat("IDs sin datos de emisión:  ", sin_no_emi_ids$cantidad_ids, "\n")
cat("Registros afectados:       ", sin_no_emi_regs$cantidad_registros, "\n")
cat("Porcentaje:                ",
    round(100 * sin_no_emi_regs$cantidad_registros / resumen_siniestros$total_registros, 2), "%\n")

if (sin_no_emi_ids$cantidad_ids > 0) {
  cat("⚠️  PROBLEMA: Hay siniestros sin emisión correspondiente\n")
}

auditoria_resultados$sin_no_emi <- list(
  ids = sin_no_emi_ids$cantidad_ids,
  regs = sin_no_emi_regs$cantidad_registros
)

################################################################################
# SECCIÓN 5: ANÁLISIS DE RELACIONES
################################################################################

print_section("SECCIÓN 5: ANÁLISIS DE RELACIONES ENTRE VARIABLES")

# ===========================
# 5.1 RELACIONES EN EMISIONES
# ===========================

print_subsection("5.1 Relaciones en EMISIONES")

cat("\n[5.1.1] Promedio de coberturas por póliza\n")

prom_cob <- sqldf("
  SELECT 
    COUNT(*) / COUNT(DISTINCT id) as coberturas_promedio_por_id,
    MIN(coberturas_por_id) as min_coberturas,
    MAX(coberturas_por_id) as max_coberturas
  FROM (
    SELECT id, COUNT(DISTINCT cobertura) as coberturas_por_id
    FROM emisiones
    GROUP BY id
  ) t
")

cat("Promedio:  ", round(prom_cob$coberturas_promedio_por_id, 2), "coberturas/póliza\n")
cat("Mínimo:    ", prom_cob$min_coberturas, "\n")
cat("Máximo:    ", prom_cob$max_coberturas, "\n")

auditoria_resultados$cob_promedio <- prom_cob

cat("\n[5.1.2] Distribución del número de coberturas por póliza\n")

dist_cob_por_id <- sqldf("
  SELECT 
    num_coberturas,
    COUNT(*) as cantidad_pólizas,
    ROUND(100.0 * COUNT(*) / (SELECT COUNT(DISTINCT id) FROM emisiones), 2) as pct
  FROM (
    SELECT id, COUNT(DISTINCT cobertura) as num_coberturas
    FROM emisiones
    GROUP BY id
  ) t
  GROUP BY num_coberturas
  ORDER BY num_coberturas
")

show_result(dist_cob_por_id)
auditoria_resultados$dist_cob_por_id <- dist_cob_por_id

# ===========================
# 5.2 RELACIONES EN SINIESTROS
# ===========================

print_subsection("5.2 Relaciones en SINIESTROS")

cat("\n[5.2.1] Promedio de siniestros por póliza\n")

prom_sin <- sqldf("
  SELECT 
    COUNT(*) / COUNT(DISTINCT id) as siniestros_promedio_por_id,
    MIN(siniestros_por_id) as min_siniestros,
    MAX(siniestros_por_id) as max_siniestros
  FROM (
    SELECT id, COUNT(*) as siniestros_por_id
    FROM siniestros
    GROUP BY id
  ) t
")

cat("Promedio:  ", round(prom_sin$siniestros_promedio_por_id, 2), "siniestros/póliza\n")
cat("Mínimo:    ", prom_sin$min_siniestros, "\n")
cat("Máximo:    ", prom_sin$max_siniestros, "\n")

auditoria_resultados$sin_promedio <- prom_sin

cat("\n[5.2.2] Distribución del número de siniestros por póliza\n")

dist_sin_por_id <- sqldf("
  SELECT 
    num_siniestros,
    COUNT(*) as cantidad_pólizas,
    ROUND(100.0 * COUNT(*) / 
        (SELECT COUNT(DISTINCT id) FROM siniestros), 2) as pct
  FROM (
    SELECT id, COUNT(*) as num_siniestros
    FROM siniestros
    GROUP BY id
  ) t
  GROUP BY num_siniestros
  ORDER BY num_siniestros
  LIMIT 20
")

show_result(dist_sin_por_id, max_rows = 20)
auditoria_resultados$dist_sin_por_id <- dist_sin_por_id

cat("\n[5.2.3] Distribución de monto de siniestros\n")

dist_monto <- sqldf("
  SELECT 
    'Monto mínimo' as metrica,
    ROUND(MIN(monto_ocu), 2) as valor
  FROM siniestros
  WHERE monto_ocu IS NOT NULL AND monto_ocu > 0
  UNION ALL
  SELECT 'Monto máximo',
    ROUND(MAX(monto_ocu), 2)
  FROM siniestros
  WHERE monto_ocu IS NOT NULL
  UNION ALL
  SELECT 'Monto promedio',
    ROUND(AVG(monto_ocu), 2)
  FROM siniestros
  WHERE monto_ocu IS NOT NULL
  UNION ALL
  SELECT 'Monto total',
    ROUND(SUM(monto_ocu), 2)
  FROM siniestros
  WHERE monto_ocu IS NOT NULL
")

show_result(dist_monto)
auditoria_resultados$dist_monto <- dist_monto

################################################################################
# SECCIÓN 6: REPORTE FINAL Y RECOMENDACIONES
################################################################################

print_section("SECCIÓN 6: REPORTE FINAL Y RECOMENDACIONES")

cat("\n")
cat("╔", strrep("═", 83), "╗\n", sep = "")
cat("║", strrep(" ", 25), "RESUMEN EJECUTIVO", strrep(" ", 41), "║\n")
cat("╚", strrep("═", 83), "╝\n", sep = "")

cat("\n[6.1] VOLUMEN Y COBERTURA DE DATOS\n")
cat("  ┌─ GENERALES\n")
cat("  │  • Registros:       ", format(resumen_generales$total_registros, big.mark = ","), "\n")
cat("  │  • IDs únicos:      ", format(resumen_generales$ids_unicos, big.mark = ","), "\n")
cat("  │  • IDs duplicados:  ", format(resumen_generales$ids_duplicados, big.mark = ","),
    "(", resumen_generales$pct_duplicacion, "%)\n")
cat("  │\n")
cat("  ├─ EMISIONES\n")
cat("  │  • Registros:       ", format(resumen_emisiones$total_registros, big.mark = ","), "\n")
cat("  │  • IDs únicos:      ", format(resumen_emisiones$ids_unicos, big.mark = ","), "\n")
cat("  │  • IDs duplicados:  ", format(resumen_emisiones$ids_duplicados, big.mark = ","),
    "(", resumen_emisiones$pct_duplicacion, "%)\n")
cat("  │\n")
cat("  ├─ SINIESTROS\n")
cat("  │  • Registros:       ", format(resumen_siniestros$total_registros, big.mark = ","), "\n")
cat("  │  • IDs únicos:      ", format(resumen_siniestros$ids_unicos, big.mark = ","), "\n")
cat("  │  • IDs duplicados:  ", format(resumen_siniestros$ids_duplicados, big.mark = ","),
    "(", resumen_siniestros$pct_duplicacion, "%)\n")
cat("  │\n")
cat("  └─ ARCHIVO_AUXILIAR\n")
cat("     • Registros:       ", format(resumen_auxiliar$total_registros, big.mark = ","), "\n")

cat("\n[6.2] GRANULARIDAD DE CADA TABLA\n")
cat("  • GENERALES:         Nivel PÓLIZA (id es clave)\n")
cat("  • EMISIONES:         Nivel PÓLIZA-COBERTURA (múltiples coberturas por id)\n")
cat("  • SINIESTROS:        Nivel SINIESTRO (múltiples siniestros por id-cobertura)\n")
cat("  • ARCHIVO_AUXILIAR:  Tabla de referencia (cobertura × entidad × tipo_veh)\n")

cat("\n[6.3] PROBLEMAS IDENTIFICADOS\n")

problema_count <- 0

if (resumen_generales$ids_duplicados > 0) {
  problema_count <- problema_count + 1
  cat("\n  ⚠️  PROBLEMA #", problema_count, ": IDs DUPLICADOS EN GENERALES\n")
  cat("      • Cantidad: ", resumen_generales$ids_duplicados, 
      "(", resumen_generales$pct_duplicacion, "%)\n")
  cat("      • Causa probable: Cambios de estado de póliza en el tiempo\n")
  cat("      • Impacto: Riesgo de contar pólizas múltiples veces\n")
}

if (gen_no_emi$ids > 0) {
  problema_count <- problema_count + 1
  cat("\n  ⚠️  PROBLEMA #", problema_count, ": GENERALES SIN EMISIÓN\n")
  cat("      • Cantidad de IDs: ", gen_no_emi$ids, "\n")
  cat("      • Registros: ", gen_no_emi$regs, "\n")
  cat("      • Causa probable: Pólizas canceladas antes de emisión\n")
}

if (sin_no_gen$ids > 0) {
  problema_count <- problema_count + 1
  cat("\n  ⚠️  PROBLEMA #", problema_count, ": SINIESTROS SIN DATOS GENERALES\n")
  cat("      • Cantidad de IDs: ", sin_no_gen$ids, "\n")
  cat("      • Registros: ", sin_no_gen$regs, "\n")
  cat("      • Causa probable: Integridad referencial faltante\n")
}

if (sin_no_emi$ids > 0) {
  problema_count <- problema_count + 1
  cat("\n  ⚠️  PROBLEMA #", problema_count, ": SINIESTROS SIN EMISIÓN\n")
  cat("      • Cantidad de IDs: ", sin_no_emi$ids, "\n")
  cat("      • Registros: ", sin_no_emi$regs, "\n")
  cat("      • Causa probable: Pólizas sin cobertura para el tipo de siniestro\n")
}

if (problema_count == 0) {
  cat("\n  ✓ No se detectaron problemas mayores de integridad referencial\n")
}

cat("\n[6.4] RIESGOS DE JOINS DIRECTOS\n")

cat("\n  1. GENERALES ← JOIN → EMISIONES\n")
if (resumen_generales$ids_duplicados > 0) {
  cat("     ✗ RIESGO ALTO: IDs duplicados causarán explosión cartesiana\n")
} else {
  cat("     ✓ SEGURO: Relación 1:N manejable\n")
}

cat("\n  2. EMISIONES ← JOIN → SINIESTROS (por id-cobertura)\n")
cat("     ⚠️  MODERADO: Validar cobertura antes del join\n")
if (sin_no_emi$ids > 0) {
  cat("     ✗ PROBLEMA: Algunos siniestros no tienen emisión\n")
}

cat("\n  3. GENERALES ← JOIN → SINIESTROS (directo por id)\n")
if (sin_no_gen$ids > 0) {
  cat("     ✗ RIESGO ALTO: Siniestros huérfanos serán perdidos en INNER JOIN\n")
  cat("     ✓ SOLUCIÓN: Usar LEFT JOIN (siniestros al izquierda)\n")
} else {
  cat("     ✓ SEGURO: Todos los siniestros tienen póliza\n")
}

cat("\n[6.5] RECOMENDACIONES PARA CONSTRUIR LA BASE MAESTRA\n")

cat("\n  PASO 1: VALIDACIÓN Y LIMPIEZA\n")
cat("    a) Resolver duplicados en GENERALES\n")
cat("       → Analizar por qué se repiten IDs\n")
cat("       → Decidir: usar DISTINCT, filtrar por fecha, o agregar criterio\n")
cat("    b) Investigar pólizas sin emisión\n")
cat("       → ¿Son canceladas antes de emitir?\n")
cat("       → Decidir si excluir o marcar como 'pendiente'\n")
cat("    c) Validar integridad de siniestros\n")
cat("       → Investigar siniestros sin póliza/emisión\n")
cat("       → Decidir: excluir, investigar o marcar como 'huérfano'\n")

cat("\n  PASO 2: CONSTRUIR BASE A NIVEL PÓLIZA-COBERTURA\n")
cat("    SQL: SELECT e.id, e.cobertura, g.*, e.prima_emi\n")
cat("         FROM emisiones e\n")
cat("         LEFT JOIN generales g ON e.id = g.id\n")
cat("         LEFT JOIN archivo_auxiliar a ON\n")
cat("           e.cobertura = a.cobertura AND\n")
cat("           e.entidad = a.entidad AND\n")
cat("           e.tipo_veh = a.tipo_veh\n")

cat("\n  PASO 3: AGREGAR SINIESTRALIDAD\n")
cat("    SQL: Crear tabla temporal de siniestros agregados:\n")
cat("         SELECT id, cobertura,\n")
cat("                COUNT(*) as num_siniestros,\n")
cat("                SUM(monto_ocu) as monto_total_siniestros\n")
cat("         FROM siniestros\n")
cat("         GROUP BY id, cobertura\n")
cat("    Luego: LEFT JOIN base_maestra con tabla de siniestros\n")

cat("\n  PASO 4: CREAR VARIABLES DE EXPOSICIÓN\n")
cat("    • Días de vigencia: fecha_fin_vig - fecha_ini_vig\n")
cat("    • Años de exposición: días / 365.25\n")
cat("    • Indicador de cancelación: CASE WHEN fec_can IS NOT NULL THEN 1 ELSE 0\n")

cat("\n  PASO 5: CALCULAR MÉTRICAS ACTUARIALES\n")
cat("    • Frecuencia = num_siniestros / años_exposición\n")
cat("    • Severidad = monto_total_siniestros / num_siniestros (si > 0)\n")
cat("    • Pérdida Esperada = Frecuencia × Severidad\n")
cat("    • Ratio Siniestralidad = Pérdida Esperada / prima_emi\n")

cat("\n[6.6] ESTRUCTURA FINAL RECOMENDADA\n")

cat("\n  BASE MAESTRA (nivel póliza-cobertura):\n")
cat("  ┌─────────────────────────────────────────────────────┐\n")
cat("  │ id, cobertura                    [CLAVES]           │\n")
cat("  │ fecha_ini_vig, fecha_fin_vig     [FECHAS]           │\n")
cat("  │ entidad, tipo_veh, uso           [VARIABLES RIESGO]  │\n")
cat("  │ prima_emi, sa, deducible         [VARIABLES PRIMA]   │\n")
cat("  │ num_siniestros, monto_siniestros [VARIABLES RESP]    │\n")
cat("  │ dias_vigencia, frecuencia        [VARIABLES CALC]    │\n")
cat("  └─────────────────────────────────────────────────────┘\n")

cat("\n[6.7] CHECKLIST ANTES DE MODELAR\n")

checklist <- data.frame(
  Tarea = c(
    "Resolver duplicados en GENERALES",
    "Investigar pólizas sin emisión",
    "Validar integridad de siniestros",
    "Crear base maestra póliza-cobertura",
    "Calcular variables de exposición",
    "Validar distribución de frecuencias",
    "Detectar y tratar outliers",
    "Crear conjuntos train/test",
    "Documentar transformaciones"
  ),
  Prioridad = c("ALTA", "ALTA", "ALTA", "CRÍTICA", "CRÍTICA",
                "MEDIA", "MEDIA", "MEDIA", "BAJA")
)

print(checklist)

cat("\n")
cat("╔", strrep("═", 83), "╗\n", sep = "")
cat("║", strrep(" ", 20), "FIN DE LA AUDITORÍA", strrep(" ", 45), "║\n")
cat("║", format(paste("Generado:", Sys.time()), width = 83), "║\n")
cat("╚", strrep("═", 83), "╝\n", sep = "")

# Guardar resultados en lista para uso posterior
cat("\n✓ Resultados guardados en objeto 'auditoria_resultados'\n")
cat("  Use: auditoria_resultados$[nombre_tabla] para acceder a resultados específicos\n")

################################################################################
# SECCIÓN 7: EXPORTAR RESUMEN A ARCHIVO
################################################################################

# Crear resumen en texto para guardar
resumen_txt <- paste0(
  "RESUMEN EJECUTIVO DE AUDITORÍA\n",
  "Generado: ", Sys.time(), "\n\n",
  
  "VOLUMEN DE DATOS:\n",
  "  GENERALES: ", format(resumen_generales$total_registros, big.mark = ","), " registros, ",
  format(resumen_generales$ids_unicos, big.mark = ","), " IDs únicos\n",
  "  EMISIONES: ", format(resumen_emisiones$total_registros, big.mark = ","), " registros, ",
  format(resumen_emisiones$ids_unicos, big.mark = ","), " IDs únicos\n",
  "  SINIESTROS: ", format(resumen_siniestros$total_registros, big.mark = ","), " registros, ",
  format(resumen_siniestros$ids_unicos, big.mark = ","), " IDs únicos\n\n",
  
  "GRANULARIDAD:\n",
  "  GENERALES: Nivel PÓLIZA\n",
  "  EMISIONES: Nivel PÓLIZA-COBERTURA\n",
  "  SINIESTROS: Nivel SINIESTRO\n\n",
  
  "PROBLEMAS DETECTADOS: ", problema_count, "\n"
)

writeLines(resumen_txt, "reportes/resumen_auditoria.txt")
cat("\n✓ Resumen exportado a: reportes/resumen_auditoria.txt\n")

cat("\n" %+paste% strrep("=", 85) %+paste% "\n")
