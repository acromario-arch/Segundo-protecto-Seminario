# ============================================================================
# SCRIPT COMPLETO DE TARIFICACIÓN DE SEGUROS DE AUTOS
# ============================================================================
# Objetivo: Un script funcional que genera tarificación completa
# Metodologías: GLM, Árbol de Decisión, Random Forest
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
# PARTE 1: CONSTRUCCIÓN DE LA BASE MAESTRA
# ============================================================================

cat("\n===== PARTE 1: CONSTRUCCIÓN DE LA BASE MAESTRA =====\n")

# Cargar datos
cat("\n1. Cargando datos...\n")
generales <- read.csv("generales_tot_fin.csv", stringsAsFactors = FALSE)
siniestros <- read.csv("siniestros_tot_fin.csv", stringsAsFactors = FALSE)
emisiones <- read_excel("emisiones_final.xlsx")

cat("✓ Generales:", nrow(generales), "registros\n")
cat("✓ Siniestros:", nrow(siniestros), "registros\n")
cat("✓ Emisiones:", nrow(emisiones), "registros\n")

# ---- Limpieza básica de datos ----
cat("\n2. Limpieza de datos...\n")

# Generales: eliminar registros con id duplicado o faltante
generales <- generales %>%
  filter(!is.na(id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  clean_names()

# Siniestros: eliminar registros con id faltante
siniestros <- siniestros %>%
  filter(!is.na(id)) %>%
  clean_names()

# Emisiones: limpiar
emisiones <- emisiones %>%
  clean_names()

cat("✓ Datos limpios\n")

# ---- Conversión de fechas ----
cat("\n3. Convirtiendo fechas...\n")

# Función para convertir fechas en formato YYYYMMDD a Date
convertir_fecha_yyyymmdd <- function(x) {
  # Si está vacío o es NA, devolver NA
  if(is.na(x) || x == "" || is.null(x)) {
    return(NA)
  }
  
  # Convertir a string si no lo es
  x <- as.character(x)
  
  # Si el string no tiene 8 caracteres, asumir que es una fecha normal
  if(nchar(x) != 8) {
    return(as.Date(x))
  }
  
  # Convertir YYYYMMDD a Date
  fecha <- as.Date(x, format="%Y%m%d")
  return(fecha)
}

# Aplicar la conversión a las fechas en generales
generales <- generales %>%
  mutate(
    fecha_ini_vig = sapply(fecha_ini_vig, convertir_fecha_yyyymmdd),
    fecha_fin_vig = sapply(fecha_fin_vig, convertir_fecha_yyyymmdd),
    fec_can = sapply(fec_can, convertir_fecha_yyyymmdd)
  )

# Convertir fechas en siniestros
siniestros <- siniestros %>%
  mutate(
    fec_ocu = sapply(fec_ocu, convertir_fecha_yyyymmdd)
  )

# Convertir fechas en emisiones
emisiones <- emisiones %>%
  mutate(
    fec_emi = sapply(fec_emi, convertir_fecha_yyyymmdd)
  )

cat("✓ Fechas convertidas\n")

# Verificar conversión de fechas
cat("\nEjemplos de fechas convertidas:\n")
cat("Fecha inicial vigencia (primeros 5):", format(head(generales$fecha_ini_vig, 5)), "\n")
cat("Fecha final vigencia (primeros 5):", format(head(generales$fecha_fin_vig, 5)), "\n")

# ---- Construcción de exposición ----
cat("\n4. Construyendo exposición...\n")

generales <- generales %>%
  mutate(
    # Calcular días vigentes
    dias_vigentes = case_when(
      # Si hay cancelación, contar hasta fecha de cancelación
      !is.na(fec_can) ~ as.numeric(fec_can - fecha_ini_vig),
      # Si no hay cancelación, contar hasta fin de vigencia
      TRUE ~ as.numeric(fecha_fin_vig - fecha_ini_vig)
    ),
    # Calcular exposición (en años)
    exposicion = dias_vigentes / 365
  ) %>%
  select(-dias_vigentes)

cat("✓ Exposición calculada (promedio:", 
    round(mean(generales$exposicion, na.rm=TRUE), 3), "años)\n")

# ---- Construcción de frecuencia y severidad ----
cat("\n5. Construyendo frecuencia y severidad...\n")

# Frecuencia: número de siniestros por id
frecuencia_base <- siniestros %>%
  group_by(id) %>%
  summarise(
    frecuencia = n(),
    costo_total = sum(monto_ocu, na.rm=TRUE),
    .groups='drop'
  ) %>%
  mutate(
    # Severidad = costo total / número de siniestros
    severidad = costo_total / frecuencia
  )

# Ids sin siniestros
ids_sin_siniestros <- tibble(
  id = setdiff(generales$id, frecuencia_base$id),
  frecuencia = 0,
  costo_total = 0,
  severidad = 0
)

# Combinar
frecuencia_final <- bind_rows(frecuencia_base, ids_sin_siniestros)

cat("✓ Frecuencia construida\n")
cat("  Registros con siniestros:", sum(frecuencia_final$frecuencia > 0), "\n")
cat("  Registros sin siniestros:", sum(frecuencia_final$frecuencia == 0), "\n")

# ---- Agregar información de emisiones ----
cat("\n6. Agregando información de emisiones...\n")

# Agrupar emisiones por id (tomar la primera emisión)
emisiones_agg <- emisiones %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  select(id, cobertura, sa, deducible, prima_emi)

cat("✓ Emisiones agregadas\n")

# ---- Construcción de base maestra final ----
cat("\n7. Construyendo base maestra final...\n")

base_maestra <- generales %>%
  select(id, exposicion, entidad, tipo_veh, marca_tipo, 
         cve_amis, modelo, uso) %>%
  left_join(frecuencia_final, by="id") %>%
  left_join(emisiones_agg, by="id") %>%
  filter(!is.na(cobertura)) %>%
  mutate(
    # Reemplazar NA en severidad con 0 si no hay siniestros
    severidad = ifelse(frecuencia == 0, 0, severidad),
    # Crear variables factor para modelos
    cobertura = factor(cobertura),
    entidad = factor(entidad),
    tipo_veh = factor(tipo_veh),
    marca_tipo = factor(marca_tipo),
    uso = factor(uso)
  )

cat("✓ Base maestra construida\n")
cat("  Total de registros:", nrow(base_maestra), "\n")
cat("  Variables:", ncol(base_maestra), "\n")

# Resumen de la base
cat("\n--- RESUMEN DE LA BASE MAESTRA ---\n")
cat("Exposición: media =", round(mean(base_maestra$exposicion, na.rm=TRUE), 3),
    ", rango = [", 
    round(min(base_maestra$exposicion, na.rm=TRUE), 3), ",",
    round(max(base_maestra$exposicion, na.rm=TRUE), 3), "]\n")
cat("Frecuencia: media =", round(mean(base_maestra$frecuencia, na.rm=TRUE), 3),
    ", máx =", max(base_maestra$frecuencia, na.rm=TRUE), "\n")
cat("Severidad: media =", round(mean(base_maestra$severidad, na.rm=TRUE), 2),
    ", máx =", round(max(base_maestra$severidad, na.rm=TRUE), 2), "\n")

# ============================================================================
# PARTE 2: ANÁLISIS EXPLORATORIO
# ============================================================================

cat("\n===== PARTE 2: ANÁLISIS EXPLORATORIO =====\n")

# ---- Distribución de frecuencia ----
cat("\n1. Análisis de frecuencia...\n")

freq_stats <- base_maestra %>%
  summarise(
    media = mean(frecuencia, na.rm=TRUE),
    varianza = var(frecuencia, na.rm=TRUE),
    desv_std = sd(frecuencia, na.rm=TRUE),
    min = min(frecuencia, na.rm=TRUE),
    max = max(frecuencia, na.rm=TRUE)
  )

cat("Media:", round(freq_stats$media, 4), "\n")
cat("Varianza:", round(freq_stats$varianza, 4), "\n")
cat("Desv. Estándar:", round(freq_stats$desv_std, 4), "\n")
cat("Sobredispersión (varianza/media):", 
    round(freq_stats$varianza/freq_stats$media, 4), "\n")

# Tabla de frecuencias
tabla_freq <- base_maestra %>%
  group_by(frecuencia) %>%
  summarise(
    cantidad = n(),
    porcentaje = round(n()/nrow(base_maestra)*100, 2),
    .groups='drop'
  ) %>%
  arrange(frecuencia) %>%
  head(10)

cat("\n--- Tabla de frecuencias (primeros 10) ---\n")
print(tabla_freq)

# Gráfico de frecuencia
g_freq <- ggplot(base_maestra, aes(x=frecuencia)) +
  geom_histogram(bins=30, fill="steelblue", alpha=0.7) +
  labs(title="Distribución de Frecuencia de Siniestros",
       x="Número de Siniestros", y="Cantidad") +
  theme_minimal()

print(g_freq)

# ---- Distribución de severidad ----
cat("\n2. Análisis de severidad...\n")

# Eliminar ceros para análisis
severidad_datos <- base_maestra %>%
  filter(severidad > 0) %>%
  pull(severidad)

sev_stats <- tibble(
  media = mean(severidad_datos, na.rm=TRUE),
  mediana = median(severidad_datos, na.rm=TRUE),
  p25 = quantile(severidad_datos, 0.25, na.rm=TRUE),
  p75 = quantile(severidad_datos, 0.75, na.rm=TRUE),
  p95 = quantile(severidad_datos, 0.95, na.rm=TRUE),
  desv_std = sd(severidad_datos, na.rm=TRUE)
)

cat("Media:", round(sev_stats$media, 2), "\n")
cat("Mediana:", round(sev_stats$mediana, 2), "\n")
cat("P25:", round(sev_stats$p25, 2), "\n")
cat("P75:", round(sev_stats$p75, 2), "\n")
cat("P95:", round(sev_stats$p95, 2), "\n")

# Gráfico de severidad
g_sev <- ggplot(base_maestra %>% filter(severidad > 0), aes(x=severidad)) +
  geom_histogram(bins=30, fill="darkgreen", alpha=0.7) +
  labs(title="Distribución de Severidad (Costo por Siniestro)",
       x="Severidad ($)", y="Cantidad") +
  theme_minimal() +
  scale_x_log10()

print(g_sev)

# Box plot de severidad
g_box_sev <- ggplot(base_maestra %>% filter(severidad > 0), 
                     aes(y=severidad)) +
  geom_boxplot(fill="darkgreen", alpha=0.7) +
  labs(title="Box Plot de Severidad",
       y="Severidad ($)") +
  theme_minimal() +
  scale_y_log10()

print(g_box_sev)

# ---- Análisis por cobertura ----
cat("\n3. Análisis por cobertura...\n")

analisis_cobertura <- base_maestra %>%
  group_by(cobertura) %>%
  summarise(
    cantidad = n(),
    freq_promedio = round(mean(frecuencia, na.rm=TRUE), 4),
    sev_promedio = round(mean(severidad[severidad > 0], na.rm=TRUE), 2),
    costo_total_promedio = round(mean(costo_total, na.rm=TRUE), 2),
    prima_promedio = round(mean(prima_emi, na.rm=TRUE), 2),
    .groups='drop'
  ) %>%
  arrange(desc(cantidad))

cat("\n--- Análisis por cobertura ---\n")
print(analisis_cobertura)

# Gráfico por cobertura
g_cob_freq <- analisis_cobertura %>%
  ggplot(aes(x=reorder(cobertura, -freq_promedio), y=freq_promedio)) +
  geom_bar(stat="identity", fill="steelblue", alpha=0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Frecuencia Promedio por Cobertura",
       x="Cobertura", y="Frecuencia Promedio")

print(g_cob_freq)

# ============================================================================
# PARTE 3: GLM FRECUENCIA
# ============================================================================

cat("\n===== PARTE 3: GLM FRECUENCIA =====\n")

# Preparar datos para modelo
datos_frecuencia <- base_maestra %>%
  filter(!is.na(exposicion) & exposicion > 0,
         !is.na(cobertura),
         !is.na(entidad),
         !is.na(tipo_veh),
         !is.na(uso),
         !is.na(marca_tipo),
         !is.na(sa),
         !is.na(deducible))

cat("\n1. Ajustando modelo Poisson...\n")

# Modelo Poisson
modelo_poisson <- glm(
  frecuencia ~ cobertura + entidad + tipo_veh + uso + marca_tipo + sa + deducible,
  offset = log(exposicion),
  family = poisson(link="log"),
  data = datos_frecuencia
)

cat("✓ Modelo Poisson ajustado\n")

# Calcular desvianza y sobredispersión
residuos_pois <- residuals(modelo_poisson, type="pearson")
sobredispersion <- sum(residuos_pois^2) / (nrow(datos_frecuencia) - 
                                            length(coef(modelo_poisson)))

cat("Sobredispersión:", round(sobredispersion, 4), "\n")

# Si existe sobredispersión importante (>1.5), ajustar modelo NB
modelo_frecuencia <- modelo_poisson

if(sobredispersion > 1.5) {
  cat("\n2. Sobredispersión importante detectada. Ajustando modelo Negative Binomial...\n")
  
  modelo_frecuencia <- glm.nb(
    frecuencia ~ cobertura + entidad + tipo_veh + uso + marca_tipo + sa + deducible,
    offset = log(exposicion),
    data = datos_frecuencia
  )
  
  cat("✓ Modelo Negative Binomial ajustado\n")
} else {
  cat("\n2. Sobredispersión moderada. Utilizando modelo Poisson.\n")
}

# Predicciones de frecuencia
cat("\n3. Generando predicciones...\n")

base_maestra <- base_maestra %>%
  mutate(
    frecuencia_estimada_glm = predict(modelo_frecuencia, 
                                       newdata=base_maestra, 
                                       type="response")
  )

cat("✓ Predicciones de frecuencia generadas\n")

# ============================================================================
# PARTE 4: GLM SEVERIDAD
# ============================================================================

cat("\n===== PARTE 4: GLM SEVERIDAD =====\n")

# Preparar datos solo con frecuencia > 0
datos_severidad <- base_maestra %>%
  filter(frecuencia > 0,
         severidad > 0,
         !is.na(cobertura),
         !is.na(entidad),
         !is.na(tipo_veh),
         !is.na(uso),
         !is.na(marca_tipo),
         !is.na(sa),
         !is.na(deducible))

cat("\n1. Datos para severidad:", nrow(datos_severidad), "registros\n")

# Modelo Gamma con enlace log
cat("\n2. Ajustando modelo Gamma...\n")

modelo_severidad <- glm(
  severidad ~ cobertura + entidad + tipo_veh + uso + marca_tipo + sa + deducible,
  family = Gamma(link="log"),
  data = datos_severidad
)

cat("✓ Modelo Gamma ajustado\n")

# Predicciones de severidad
cat("\n3. Generando predicciones...\n")

base_maestra <- base_maestra %>%
  mutate(
    severidad_estimada_glm = predict(modelo_severidad,
                                      newdata=base_maestra,
                                      type="response")
  )

# Asegurar que severidad estimada sea positiva para registros sin siniestros
base_maestra <- base_maestra %>%
  mutate(
    severidad_estimada_glm = ifelse(is.na(severidad_estimada_glm), 0, 
                                     severidad_estimada_glm)
  )

cat("✓ Predicciones de severidad generadas\n")

# ============================================================================
# PARTE 5: PRIMA PURA GLM
# ============================================================================

cat("\n===== PARTE 5: PRIMA PURA GLM =====\n")

cat("\n1. Calculando prima pura GLM...\n")

base_maestra <- base_maestra %>%
  mutate(
    prima_pura_glm = frecuencia_estimada_glm * severidad_estimada_glm
  )

cat("✓ Prima pura GLM calculada\n")
cat("Media:", round(mean(base_maestra$prima_pura_glm, na.rm=TRUE), 2), "\n")
cat("Mediana:", round(median(base_maestra$prima_pura_glm, na.rm=TRUE), 2), "\n")

# ============================================================================
# PARTE 6: ÁRBOL DE DECISIÓN
# ============================================================================

cat("\n===== PARTE 6: ÁRBOL DE DECISIÓN =====\n")

# Preparar datos
datos_arbol <- base_maestra %>%
  filter(!is.na(cobertura),
         !is.na(entidad),
         !is.na(tipo_veh),
         !is.na(uso),
         !is.na(marca_tipo),
         !is.na(sa),
         !is.na(deducible),
         !is.na(frecuencia))

# ---- Árbol para frecuencia ----
cat("\n1. Árbol para frecuencia...\n")

arbol_frecuencia <- rpart(
  frecuencia ~ cobertura + entidad + tipo_veh + uso + marca_tipo + sa + deducible,
  data = datos_arbol,
  method = "anova",
  control = rpart.control(minsplit=20, minbucket=10, cp=0.01)
)

cat("✓ Árbol de frecuencia ajustado\n")

# Predicciones
base_maestra <- base_maestra %>%
  mutate(
    frecuencia_estimada_arbol = predict(arbol_frecuencia, 
                                         newdata=base_maestra)
  )

# ---- Árbol para severidad ----
cat("\n2. Árbol para severidad...\n")

datos_severidad_arbol <- datos_arbol %>%
  filter(frecuencia > 0, severidad > 0)

if(nrow(datos_severidad_arbol) > 20) {
  arbol_severidad <- rpart(
    severidad ~ cobertura + entidad + tipo_veh + uso + marca_tipo + sa + deducible,
    data = datos_severidad_arbol,
    method = "anova",
    control = rpart.control(minsplit=10, minbucket=5, cp=0.01)
  )
  
  cat("✓ Árbol de severidad ajustado\n")
  
  # Predicciones
  base_maestra <- base_maestra %>%
    mutate(
      severidad_estimada_arbol = predict(arbol_severidad,
                                          newdata=base_maestra)
    )
} else {
  cat("⚠ Datos insuficientes para árbol de severidad\n")
  base_maestra <- base_maestra %>%
    mutate(
      severidad_estimada_arbol = mean(datos_severidad_arbol$severidad, na.rm=TRUE)
    )
}

# ---- Prima pura árbol ----
cat("\n3. Calculando prima pura árbol...\n")

base_maestra <- base_maestra %>%
  mutate(
    prima_pura_arbol = frecuencia_estimada_arbol * severidad_estimada_arbol
  )

cat("✓ Prima pura árbol calculada\n")

# ============================================================================
# PARTE 7: RANDOM FOREST
# ============================================================================

cat("\n===== PARTE 7: RANDOM FOREST =====\n")

# Preparar datos
datos_rf <- base_maestra %>%
  filter(!is.na(cobertura),
         !is.na(entidad),
         !is.na(tipo_veh),
         !is.na(uso),
         !is.na(marca_tipo),
         !is.na(sa),
         !is.na(deducible),
         !is.na(frecuencia))

# ---- Random Forest para frecuencia ----
cat("\n1. Random Forest para frecuencia...\n")

rf_frecuencia <- ranger(
  frecuencia ~ cobertura + entidad + tipo_veh + uso + marca_tipo + sa + deducible,
  data = datos_rf,
  num.trees = 100,
  mtry = 4,
  min.node.size = 10,
  seed = 42
)

cat("✓ Random Forest de frecuencia ajustado\n")

# Predicciones
predicciones_rf_freq <- predict(rf_frecuencia, data=base_maestra)
base_maestra <- base_maestra %>%
  mutate(
    frecuencia_estimada_rf = predicciones_rf_freq$predictions
  )

# ---- Random Forest para severidad ----
cat("\n2. Random Forest para severidad...\n")

datos_severidad_rf <- datos_rf %>%
  filter(frecuencia > 0, severidad > 0)

if(nrow(datos_severidad_rf) > 20) {
  rf_severidad <- ranger(
    severidad ~ cobertura + entidad + tipo_veh + uso + marca_tipo + sa + deducible,
    data = datos_severidad_rf,
    num.trees = 100,
    mtry = 4,
    min.node.size = 10,
    seed = 42
  )
  
  cat("✓ Random Forest de severidad ajustado\n")
  
  # Predicciones
  predicciones_rf_sev <- predict(rf_severidad, data=base_maestra)
  base_maestra <- base_maestra %>%
    mutate(
      severidad_estimada_rf = predicciones_rf_sev$predictions
    )
} else {
  cat("⚠ Datos insuficientes para Random Forest de severidad\n")
  base_maestra <- base_maestra %>%
    mutate(
      severidad_estimada_rf = mean(datos_severidad_rf$severidad, na.rm=TRUE)
    )
}

# ---- Prima pura Random Forest ----
cat("\n3. Calculando prima pura Random Forest...\n")

base_maestra <- base_maestra %>%
  mutate(
    prima_pura_rf = frecuencia_estimada_rf * severidad_estimada_rf
  )

cat("✓ Prima pura Random Forest calculada\n")

# ============================================================================
# PARTE 8: RESULTADOS FINALES
# ============================================================================

cat("\n===== PARTE 8: RESULTADOS FINALES =====\n")

# Preparar base final con todas las predicciones
base_final <- base_maestra %>%
  select(
    id,
    exposicion,
    # Frecuencia
    frecuencia,
    frecuencia_estimada_glm,
    frecuencia_estimada_arbol,
    frecuencia_estimada_rf,
    # Severidad
    severidad,
    severidad_estimada_glm,
    severidad_estimada_arbol,
    severidad_estimada_rf,
    # Prima pura
    prima_pura_glm,
    prima_pura_arbol,
    prima_pura_rf,
    # Variables
    cobertura,
    entidad,
    tipo_veh,
    marca_tipo,
    cve_amis,
    modelo,
    uso,
    sa,
    deducible,
    costo_total,
    prima_emi
  )

cat("\n1. Base final construida\n")
cat("Registros:", nrow(base_final), "\n")
cat("Variables:", ncol(base_final), "\n")

# Resumen de predicciones
cat("\n2. Resumen de predicciones\n\n")

resumen_predicciones <- tibble(
  Variable = c("Frecuencia GLM", "Frecuencia Árbol", "Frecuencia RF",
               "Severidad GLM", "Severidad Árbol", "Severidad RF",
               "Prima GLM", "Prima Árbol", "Prima RF"),
  Media = c(
    round(mean(base_final$frecuencia_estimada_glm, na.rm=TRUE), 4),
    round(mean(base_final$frecuencia_estimada_arbol, na.rm=TRUE), 4),
    round(mean(base_final$frecuencia_estimada_rf, na.rm=TRUE), 4),
    round(mean(base_final$severidad_estimada_glm, na.rm=TRUE), 2),
    round(mean(base_final$severidad_estimada_arbol, na.rm=TRUE), 2),
    round(mean(base_final$severidad_estimada_rf, na.rm=TRUE), 2),
    round(mean(base_final$prima_pura_glm, na.rm=TRUE), 2),
    round(mean(base_final$prima_pura_arbol, na.rm=TRUE), 2),
    round(mean(base_final$prima_pura_rf, na.rm=TRUE), 2)
  ),
  Mediana = c(
    round(median(base_final$frecuencia_estimada_glm, na.rm=TRUE), 4),
    round(median(base_final$frecuencia_estimada_arbol, na.rm=TRUE), 4),
    round(median(base_final$frecuencia_estimada_rf, na.rm=TRUE), 4),
    round(median(base_final$severidad_estimada_glm, na.rm=TRUE), 2),
    round(median(base_final$severidad_estimada_arbol, na.rm=TRUE), 2),
    round(median(base_final$severidad_estimada_rf, na.rm=TRUE), 2),
    round(median(base_final$prima_pura_glm, na.rm=TRUE), 2),
    round(median(base_final$prima_pura_arbol, na.rm=TRUE), 2),
    round(median(base_final$prima_pura_rf, na.rm=TRUE), 2)
  ),
  Min = c(
    round(min(base_final$frecuencia_estimada_glm, na.rm=TRUE), 4),
    round(min(base_final$frecuencia_estimada_arbol, na.rm=TRUE), 4),
    round(min(base_final$frecuencia_estimada_rf, na.rm=TRUE), 4),
    round(min(base_final$severidad_estimada_glm, na.rm=TRUE), 2),
    round(min(base_final$severidad_estimada_arbol, na.rm=TRUE), 2),
    round(min(base_final$severidad_estimada_rf, na.rm=TRUE), 2),
    round(min(base_final$prima_pura_glm, na.rm=TRUE), 2),
    round(min(base_final$prima_pura_arbol, na.rm=TRUE), 2),
    round(min(base_final$prima_pura_rf, na.rm=TRUE), 2)
  ),
  Max = c(
    round(max(base_final$frecuencia_estimada_glm, na.rm=TRUE), 4),
    round(max(base_final$frecuencia_estimada_arbol, na.rm=TRUE), 4),
    round(max(base_final$frecuencia_estimada_rf, na.rm=TRUE), 4),
    round(max(base_final$severidad_estimada_glm, na.rm=TRUE), 2),
    round(max(base_final$severidad_estimada_arbol, na.rm=TRUE), 2),
    round(max(base_final$severidad_estimada_rf, na.rm=TRUE), 2),
    round(max(base_final$prima_pura_glm, na.rm=TRUE), 2),
    round(max(base_final$prima_pura_arbol, na.rm=TRUE), 2),
    round(max(base_final$prima_pura_rf, na.rm=TRUE), 2)
  )
)

print(resumen_predicciones)

# ---- Exportar a Excel ----
cat("\n3. Exportando resultados a Excel...\n")

write_xlsx(base_final, "resultados_tarificacion_completa.xlsx")

cat("✓ Archivo exportado: resultados_tarificacion_completa.xlsx\n")

# ---- Resumen final ----
cat("\n", strrep("=", 80), "\n")
cat("SCRIPT COMPLETADO EXITOSAMENTE\n")
cat(strrep("=", 80), "\n")

cat("\nArchivos generados:\n")
cat("✓ resultados_tarificacion_completa.xlsx\n")

cat("\nModelos ajustados:\n")
cat("✓ GLM Frecuencia (", ifelse(class(modelo_frecuencia)[1]=="negbin", 
    "Negative Binomial", "Poisson"), ")\n")
cat("✓ GLM Severidad (Gamma)\n")
cat("✓ Árbol de Decisión (Frecuencia y Severidad)\n")
cat("✓ Random Forest (Frecuencia y Severidad)\n")

cat("\nTotal de registros en base final:", nrow(base_final), "\n")
cat("Total de variables:", ncol(base_final), "\n")

cat("\n", strrep("=", 80), "\n")

# ============================================================================
# FIN DEL SCRIPT
# ============================================================================
