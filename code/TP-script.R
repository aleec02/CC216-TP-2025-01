sink("resultados.txt")


#####################################################
# PARTE 01: CONFIGURACIÓN INICIAL Y CARGA DE DATOS
#####################################################

## PROJECT WD
if (exists(".rs.getProjectDirectory")) {
  project_dir <- .rs.getProjectDirectory()
  print(paste("Directorio del proyecto:", project_dir))
} else {
  # Alternativa si no estamos en RStudio
  project_dir <- getwd()
  print(paste("Directorio actual (no RStudio):", project_dir))
}

## LIBRARIES
if (!require("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!require("naniar", quietly = TRUE)) install.packages("naniar")
if (!require("skimr", quietly = TRUE)) install.packages("skimr")
if (!require("knitr", quietly = TRUE)) install.packages("knitr")
if (!require("crayon", quietly = TRUE)) install.packages("crayon")
if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!require("gridExtra", quietly = TRUE)) install.packages("gridExtra")

## INITIAL SETUP 
library(tidyverse)
library(naniar)   # Para análisis de datos faltantes
library(skimr)    # Para resúmenes estadísticos
library(knitr)    # Para tablas bonitas
library(crayon)   # Para colorear la salida en consola
library(ggplot2)  # Para visualizaciones
library(gridExtra) # Para organizar múltiples gráficos

# Definir las rutas de los directorios
data_dir <- file.path(project_dir, "data")
code_dir <- file.path(project_dir, "code")

# Verificar que las rutas existen
if (!dir.exists(data_dir)) {
  stop(red("El directorio de datos no existe:", data_dir))
}
if (!dir.exists(code_dir)) {
  stop(red("El directorio de código no existe:", code_dir))
}

# Mostrar la información de directorios
cat(green("Directorio del proyecto:"), project_dir, "\n")
cat(green("Directorio de datos:"), data_dir, "\n")
cat(green("Directorio de código:"), code_dir, "\n")



# Ruta al archivo CSV
CSV_original <- file.path(data_dir, "hotel_bookings.csv")
if (!file.exists(CSV_original)) {
  stop(red("El archivo CSV no existe:", CSV_original))
}

# Cargar los datos
cat(green("Cargando datos desde:"), CSV_original, "\n")
hotel_data <- read.csv(CSV_original, header = TRUE, stringsAsFactors = FALSE)

# Eliminar duplicados
hotel_data <- unique(hotel_data)

# Inspección inicial
cat(yellow("\n--- DIMENSIONES DEL DATASET ---\n"))
print(dim(hotel_data))

cat(yellow("\n--- PRIMERAS FILAS DEL DATASET ---\n"))
print(head(hotel_data, 5))

cat(yellow("\n--- ESTRUCTURA DEL DATASET ---\n"))
str(hotel_data)

cat(yellow("\n--- RESUMEN ESTADÍSTICO BÁSICO (TODAS LAS COLUMNAS) ---\n"))
print(summary(hotel_data))

# Análisis adicional con skimr para obtener más detalles
cat(yellow("\n--- ANÁLISIS DETALLADO CON SKIMR ---\n"))
print(skim(hotel_data))


# Rutas para guardar los datasets procesados
CSV_limpio <- file.path(data_dir, "hotel_bookings_limpio.csv")
CSV_final <- file.path(data_dir, "hotel_bookings_final.csv")

cat(green("\nRutas para guardar datasets procesados:"), "\n")
cat("Dataset limpio:", CSV_limpio, "\n")
cat("Dataset final (si es necesario):", CSV_final, "\n")





#####################################################
# PARTE 02: ANÁLISIS DE DATOS FALTANTES Y ATÍPICOS
#####################################################

cat(green("\n=== RONDA 2: ANÁLISIS DE DATOS FALTANTES Y ATÍPICOS ===\n"))

#------------------------------------------
# 2.1. ANÁLISIS DE DATOS FALTANTES (NA)
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE DATOS FALTANTES ---\n"))

# Conteo de NA por columna
na_count <- colSums(is.na(hotel_data))
na_percentage <- round(na_count / nrow(hotel_data) * 100, 2)

na_summary <- data.frame(
  Variable = names(na_count),
  NA_Count = na_count,
  NA_Percentage = na_percentage
)

# Ordenar por cantidad de NA (descendente)
na_summary <- na_summary[order(-na_summary$NA_Count), ]

# Mostrar resumen de valores NA
print(na_summary)

# Visualización de NA - usando una muestra representativa
set.seed(123) # Para reproducibilidad
muestra_datos <- hotel_data %>% 
  slice_sample(n = 5000) # Tomar una muestra de 5000 registros

cat(yellow("\nVisualización de datos faltantes (muestra de 5000 registros):"))
print(vis_miss(muestra_datos))

# Análisis específico para variable 'children' (la única con NA)
cat(yellow("\nAnálisis específico para la variable 'children':"))
cat("\nDistribución de valores no-NA en 'children':\n")
print(table(hotel_data$children, useNA = "ifany"))


#------------------------------------------
# 2.2. ANÁLISIS DE VALORES ATÍPICOS (OUTLIERS)
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE VALORES ATÍPICOS ---\n"))

# Análisis estadístico de outliers usando el método IQR para variables clave
cat(yellow("\nAnálisis estadístico de outliers en variables clave:\n"))

outlier_stats <- function(data, var_name) {
  var <- data[[var_name]]
  var <- var[!is.na(var)]
  
  Q1 <- quantile(var, 0.25)
  Q3 <- quantile(var, 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- sum(var < lower_bound | var > upper_bound)
  outlier_percent <- round(outliers / length(var) * 100, 2)
  
  return(data.frame(
    Variable = var_name,
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR,
    Lower_Bound = lower_bound,
    Upper_Bound = upper_bound,
    Outlier_Count = outliers,
    Outlier_Percentage = outlier_percent
  ))
}

# Variables numéricas que pueden tener outliers
numeric_vars <- c(
  "lead_time", "stays_in_weekend_nights", "stays_in_week_nights",
  "adults", "children", "babies", "previous_cancellations",
  "previous_bookings_not_canceled", "booking_changes", 
  "days_in_waiting_list", "adr", "required_car_parking_spaces",
  "total_of_special_requests"
)

# Aplicar la función a variables numéricas clave
outlier_summary <- do.call(rbind, lapply(numeric_vars, function(var) {
  outlier_stats(hotel_data, var)
}))

print(outlier_summary)

# Valores extremos específicos para variables de interés
cat(yellow("\nValores extremos en variables clave:\n"))

mostrar_extremos <- function(data, var_name, n = 5) {
  cat("\nVariable:", var_name, "\n")
  sorted_values <- sort(data[[var_name]], decreasing = TRUE)
  cat("Top", n, "valores más altos:", head(sorted_values, n), "\n")
  
  if (min(data[[var_name]], na.rm = TRUE) < 0) {
    cat("Valores negativos:", sort(data[[var_name]][data[[var_name]] < 0]), "\n")
  }
}

# Variables de particular interés 
variables_interes <- c("lead_time", "adults", "adr", "stays_in_week_nights", "stays_in_weekend_nights")

for (var in variables_interes) {
  mostrar_extremos(hotel_data, var)
}



# Crear histogramas individuales para variables clave
cat(yellow("\nHistogramas para variables clave:"))

crear_histogramas_mejorados <- function(data, variables) {
  for (var in variables) {
    # Para variables con valores extremos, usar zoom
    if(var == "lead_time") {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal() +
        xlim(0, 400)  # Zoom para ver mejor la distribución principal
    } else if(var == "adults") {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal() +
        xlim(0, 5)  # Enfocarse en valores razonables
    } else if(var == "adr") {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal() +
        xlim(0, 500)  # Enfocarse en el rango principal
    } else {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal()
    }
    print(p)
  }
}

cat("\nCreando histogramas mejorados para variables clave...\n")
crear_histogramas_mejorados(hotel_data, variables_interes)



#------------------------------------------
# 2.3. VERIFICACIÓN DE CONSISTENCIA LÓGICA
#------------------------------------------
cat(yellow("\n--- VERIFICACIÓN DE CONSISTENCIA LÓGICA ---\n"))

# Verificar reservas sin adultos
reservas_sin_adultos <- sum(hotel_data$adults == 0)
cat("Reservas sin adultos:", reservas_sin_adultos, "\n")

# Verificar total de noches = 0
reservas_sin_noches <- sum(hotel_data$stays_in_weekend_nights == 0 & hotel_data$stays_in_week_nights == 0)
cat("Reservas sin noches (0 días de estadía):", reservas_sin_noches, "\n")

# Verificar consistencia entre estado de reserva y cancelación
inconsistencia_cancelacion <- sum(hotel_data$is_canceled == 1 & hotel_data$reservation_status == "Check-Out")
cat("Inconsistencias entre cancelación y estado (canceladas pero con check-out):", inconsistencia_cancelacion, "\n")

# Verificar valores atípicos en número de adultos
cat("\nPosibles errores en 'adults':")
adultos_tabla <- table(hotel_data$adults)
print(adultos_tabla)


#------------------------------------------
# 2.4. DEFINIR ESTRATEGIAS PARA DATOS FALTANTES Y ATÍPICOS
#------------------------------------------
cat(yellow("\n--- ESTRATEGIAS PROPUESTAS ---\n"))

cat("1. Estrategia para datos faltantes:\n")
cat("   - Para 'children': Imputar con la moda (0) ya que la mayoría de reservas no tienen niños\n")

cat("\n2. Estrategia para outliers:\n")
cat("   - lead_time: Mantener valores hasta 365 días (1 año), recortar valores superiores\n")
cat("   - adults: Valores superiores a 4 parecen errores, considerar recortar a un máximo razonable\n")
cat("   - stays_in_weekend_nights y stays_in_week_nights: Establecer límites razonables (ej. máximo 14 días)\n")
cat("   - adr: Eliminar valores negativos y recortar valores extremadamente altos (ej. > 1000)\n")





#####################################################
# PARTE 03: PREPROCESAMIENTO DE DATOS
#####################################################

cat(green("\n=== RONDA 3: PREPROCESAMIENTO DE DATOS ===\n"))

# Crear una copia para no modificar los datos originales
hotel_data_limpio <- hotel_data

#------------------------------------------
# 3.1. TRATAMIENTO DE DATOS FALTANTES
#------------------------------------------
cat(yellow("\n--- TRATAMIENTO DE DATOS FALTANTES ---\n"))

# Imputar los NA en 'children' con la moda (0)
hotel_data_limpio$children[is.na(hotel_data_limpio$children)] <- 0
cat("Valores NA en 'children' después de imputación:", sum(is.na(hotel_data_limpio$children)), "\n")

#------------------------------------------
# 3.2. TRATAMIENTO DE VALORES ATÍPICOS (OUTLIERS)
#------------------------------------------
cat(yellow("\n--- TRATAMIENTO DE VALORES ATÍPICOS ---\n"))

# Función para aplicar winsorización en una variable
winsorizar <- function(x, lower_limit, upper_limit) {
  x[x < lower_limit] <- lower_limit
  x[x > upper_limit] <- upper_limit
  return(x)
}

# 3.2.1. Winsorizar lead_time (tiempo de anticipación)
cat("\nTratamiento de 'lead_time':")
cat("\n  Antes - Max:", max(hotel_data_limpio$lead_time), "Min:", min(hotel_data_limpio$lead_time))
hotel_data_limpio$lead_time <- winsorizar(hotel_data_limpio$lead_time, 0, 365)
cat("\n  Después - Max:", max(hotel_data_limpio$lead_time), "Min:", min(hotel_data_limpio$lead_time), "\n")

# 3.2.2. Corregir adults (adultos)
cat("\nTratamiento de 'adults':")
cat("\n  Antes - Max:", max(hotel_data_limpio$adults), "Min:", min(hotel_data_limpio$adults))
# Reemplazar valores 0 con 1 (no tiene sentido una reserva sin adultos)
hotel_data_limpio$adults[hotel_data_limpio$adults == 0] <- 1
# Winsorizar a un máximo de 4 adultos por habitación
hotel_data_limpio$adults <- winsorizar(hotel_data_limpio$adults, 1, 4)
cat("\n  Después - Max:", max(hotel_data_limpio$adults), "Min:", min(hotel_data_limpio$adults), "\n")

# 3.2.3. Winsorizar stays_in_weekend_nights y stays_in_week_nights
cat("\nTratamiento de 'stays_in_weekend_nights':")
cat("\n  Antes - Max:", max(hotel_data_limpio$stays_in_weekend_nights))
hotel_data_limpio$stays_in_weekend_nights <- winsorizar(hotel_data_limpio$stays_in_weekend_nights, 0, 14)
cat("\n  Después - Max:", max(hotel_data_limpio$stays_in_weekend_nights), "\n")

cat("\nTratamiento de 'stays_in_week_nights':")
cat("\n  Antes - Max:", max(hotel_data_limpio$stays_in_week_nights))
hotel_data_limpio$stays_in_week_nights <- winsorizar(hotel_data_limpio$stays_in_week_nights, 0, 14)
cat("\n  Después - Max:", max(hotel_data_limpio$stays_in_week_nights), "\n")

# 3.2.4. Tratar adr (tarifa diaria promedio)
cat("\nTratamiento de 'adr':")
cat("\n  Antes - Max:", max(hotel_data_limpio$adr), "Min:", min(hotel_data_limpio$adr))
# Reemplazar valores negativos con 0
hotel_data_limpio$adr[hotel_data_limpio$adr < 0] <- 0
# Winsorizar valores extremadamente altos
hotel_data_limpio$adr <- winsorizar(hotel_data_limpio$adr, 0, 1000)
cat("\n  Después - Max:", max(hotel_data_limpio$adr), "Min:", min(hotel_data_limpio$adr), "\n")

# 3.2.5. Winsorizar otros valores numéricos
cat("\nTratamiento de 'children':")
hotel_data_limpio$children <- winsorizar(hotel_data_limpio$children, 0, 3)
cat("\n  Después - Max:", max(hotel_data_limpio$children), "\n")

cat("\nTratamiento de 'babies':")
hotel_data_limpio$babies <- winsorizar(hotel_data_limpio$babies, 0, 2)
cat("\n  Después - Max:", max(hotel_data_limpio$babies), "\n")

#------------------------------------------
# 3.3. TRATAMIENTO DE INCONSISTENCIAS LÓGICAS
#------------------------------------------
cat(yellow("\n--- TRATAMIENTO DE INCONSISTENCIAS LÓGICAS ---\n"))

# Identificar reservas sin noches (estancia de 0 días)
reservas_sin_noches <- hotel_data_limpio$stays_in_weekend_nights == 0 & 
  hotel_data_limpio$stays_in_week_nights == 0

cat("Reservas con estancia de 0 días:", sum(reservas_sin_noches), "\n")

# Como no tiene sentido una reserva sin estadía, establecemos al menos 1 noche
hotel_data_limpio$stays_in_week_nights[reservas_sin_noches] <- 1
cat("Reservas con estancia de 0 días después de corrección:", 
    sum(hotel_data_limpio$stays_in_weekend_nights == 0 & 
          hotel_data_limpio$stays_in_week_nights == 0), "\n")

#------------------------------------------
# 4. VERIFICACIÓN DE LIMPIEZA
#------------------------------------------
cat(yellow("\n--- VERIFICACIÓN DE LIMPIEZA ---\n"))

# Verificar NA después de limpieza
na_count_limpio <- colSums(is.na(hotel_data_limpio))
cat("\nCantidad de NA después de limpieza:", sum(na_count_limpio), "\n")

# Verificar outliers después de limpieza
outlier_summary_limpio <- do.call(rbind, lapply(numeric_vars, function(var) {
  outlier_stats(hotel_data_limpio, var)
}))

cat("\nResumen de outliers después de limpieza:\n")
print(outlier_summary_limpio)





#------------------------------------------
# 4.5 GUARDAR GRÁFICAS COMO JPG EN DATA FOLDER
#------------------------------------------
cat(yellow("\n--- GUARDANDO GRÁFICAS EN FORMATO JPG ---\n"))

# Crear subfolder para las gráficas si no existe
graphics_dir <- file.path(data_dir, "graficas")
if (!dir.exists(graphics_dir)) {
  dir.create(graphics_dir)
  cat("Creado directorio para gráficas:", graphics_dir, "\n")
} else {
  cat("Usando directorio existente para gráficas:", graphics_dir, "\n")
}

# Crear subfolder para gráficas de datos limpios
graphics_clean_dir <- file.path(graphics_dir, "limpios")
if (!dir.exists(graphics_clean_dir)) {
  dir.create(graphics_clean_dir)
  cat("Creado directorio para gráficas de datos limpios:", graphics_clean_dir, "\n")
} else {
  cat("Usando directorio existente para gráficas limpias:", graphics_clean_dir, "\n")
}

# Función para crear y guardar histogramas
crear_y_guardar_histogramas <- function(data, variables, directorio) {
  graficas_guardadas <- c()
  
  for (var in variables) {
    # Nombre del archivo para guardar
    filename <- file.path(directorio, paste0("histograma_", var, ".jpg"))
    
    # Crear histograma según la variable
    if(var == "lead_time") {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal() +
        xlim(0, 400)  # Zoom para ver mejor la distribución principal
    } else if(var == "adults") {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal() +
        xlim(0, 5)  # Enfocarse en valores razonables
    } else if(var == "adr") {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal() +
        xlim(0, 500)  # Enfocarse en el rango principal
    } else {
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
        theme_minimal()
    }
    
    # Guardar el gráfico como JPG
    ggsave(filename = filename, plot = p, width = 8, height = 6, dpi = 300)
    graficas_guardadas <- c(graficas_guardadas, filename)
    cat(" - Guardado:", filename, "\n")
  }
  
  return(graficas_guardadas)
}

# Guardar histogramas para variables de interés
cat("\nGuardando histogramas para datos originales...\n")
histogramas_originales <- crear_y_guardar_histogramas(
  hotel_data, 
  variables_interes, 
  graphics_dir
)

# Guardar histogramas para datos limpios
cat("\nGuardando histogramas para datos limpios...\n")
histogramas_limpios <- crear_y_guardar_histogramas(
  hotel_data_limpio, 
  variables_interes, 
  graphics_clean_dir
)

# Resumir gráficas guardadas
cat("\nTotal de archivos guardados:", 
    length(c(histogramas_originales, histogramas_limpios)), "\n")








#------------------------------------------
# PARTE 05. GUARDAR DATASET LIMPIO
#------------------------------------------
cat(yellow("\n--- GUARDANDO DATASET LIMPIO ---\n"))

# Guardar dataset limpio
write.csv(hotel_data_limpio, CSV_limpio, row.names = FALSE)
cat("Dataset limpio guardado en:", CSV_limpio, "\n")

# Mostrar comparación de dimensiones antes y después de la limpieza
cat(yellow("\n--- COMPARACIÓN DE DIMENSIONES ---\n"))
cat("Dataset original:", dim(hotel_data)[1], "filas x", dim(hotel_data)[2], "columnas\n")
cat("Dataset limpio:", dim(hotel_data_limpio)[1], "filas x", dim(hotel_data_limpio)[2], "columnas\n")

# Verificar si hubo cambios
filas_diff <- dim(hotel_data_limpio)[1] - dim(hotel_data)[1]
cols_diff <- dim(hotel_data_limpio)[2] - dim(hotel_data)[2]

if (filas_diff != 0 || cols_diff != 0) {
  cat(red("¡Alerta! Las dimensiones cambiaron durante la limpieza:\n"))
  if (filas_diff != 0) cat("  - Diferencia en filas:", filas_diff, "\n")
  if (cols_diff != 0) cat("  - Diferencia en columnas:", cols_diff, "\n")
} else {
  cat(green("✓ Las dimensiones se mantuvieron intactas durante la limpieza.\n"))
  cat("  No se eliminaron filas ni se agregaron/eliminaron columnas.\n")
  cat("  Solo se modificaron los valores para corregir outliers y datos faltantes.\n")
}





#####################################################
# PARTE 06: ANÁLISIS EDA
#####################################################

cat(green("\n=== RONDA 4: ANÁLISIS EXPLORATORIO - PREGUNTAS CLAVE (PARTE 1) ===\n"))

# Cargar el dataset limpio
hotel_data_limpio <- read.csv(CSV_limpio, header = TRUE, stringsAsFactors = FALSE)

# Crear directorio para gráficas de análisis si no existe
graphics_analysis_dir <- file.path(graphics_dir, "analisis")
if (!dir.exists(graphics_analysis_dir)) {
  dir.create(graphics_analysis_dir)
  cat("Creado directorio para gráficas de análisis:", graphics_analysis_dir, "\n")
}

#------------------------------------------
# EDA 01: ¿CUÁNTAS RESERVAS SE REALIZAN POR TIPO DE HOTEL? ¿QUÉ TIPO DE HOTEL PREFIERE LA GENTE?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS POR TIPO DE HOTEL ---\n"))

# Contar reservas por tipo de hotel
reservas_por_hotel <- table(hotel_data_limpio$hotel)
reservas_por_hotel_df <- as.data.frame(reservas_por_hotel)
names(reservas_por_hotel_df) <- c("Tipo_Hotel", "Cantidad")

# Calcular porcentajes
reservas_por_hotel_df$Porcentaje <- round(
  reservas_por_hotel_df$Cantidad / sum(reservas_por_hotel_df$Cantidad) * 100, 2
)

# Mostrar resultados
cat("\nDistribución de reservas por tipo de hotel:\n")
print(reservas_por_hotel_df)

# Visualizar distribución
plot_hoteles <- ggplot(reservas_por_hotel_df, aes(x = Tipo_Hotel, y = Cantidad, fill = Tipo_Hotel)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Cantidad, "\n(", Porcentaje, "%)")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Cantidad de reservas por tipo de hotel",
       x = "Tipo de hotel",
       y = "Cantidad de reservas") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_hoteles)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "reservas_por_hotel.jpg"), 
       plot_hoteles, width = 8, height = 6, dpi = 300)


# Análisis por estado de cancelación para ver preferencia real
reservas_completadas <- hotel_data_limpio[hotel_data_limpio$is_canceled == 0, ]
reservas_completadas_por_hotel <- table(reservas_completadas$hotel)
reservas_completadas_df <- as.data.frame(reservas_completadas_por_hotel)
names(reservas_completadas_df) <- c("Tipo_Hotel", "Reservas_Completadas")

# Calcular porcentajes de reservas completadas
reservas_completadas_df$Porcentaje <- round(
  reservas_completadas_df$Reservas_Completadas / sum(reservas_completadas_df$Reservas_Completadas) * 100, 2
)

cat("\nDistribución de reservas completadas (no canceladas) por tipo de hotel:\n")
print(reservas_completadas_df)

# Visualizar reservas completadas
plot_completadas <- ggplot(reservas_completadas_df, aes(x = Tipo_Hotel, y = Reservas_Completadas, fill = Tipo_Hotel)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Reservas_Completadas, "\n(", Porcentaje, "%)")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Reservas completadas por tipo de hotel",
       subtitle = "Excluyendo reservas canceladas",
       x = "Tipo de hotel",
       y = "Cantidad de reservas completadas") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_completadas)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "reservas_completadas_por_hotel.jpg"), 
       plot_completadas, width = 8, height = 6, dpi = 300)

#------------------------------------------
# EDA 02: ¿ESTÁ AUMENTANDO LA DEMANDA CON EL TIEMPO?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE TENDENCIA DE DEMANDA ---\n"))

# Análisis por año y mes
# Convertir mes a factor ordenado
hotel_data_limpio$arrival_date_month <- factor(
  hotel_data_limpio$arrival_date_month,
  levels = c("January", "February", "March", "April", "May", "June", 
             "July", "August", "September", "October", "November", "December")
)

# Crear columna de fecha combinada (año-mes)
hotel_data_limpio$arrival_yearmonth <- paste(
  hotel_data_limpio$arrival_date_year,
  sprintf("%02d", as.numeric(factor(hotel_data_limpio$arrival_date_month, 
                                    levels = c("January", "February", "March", "April", "May", "June", 
                                               "July", "August", "September", "October", "November", "December")))),
  sep = "-"
)

# Agregación por año-mes
reservas_por_tiempo <- hotel_data_limpio %>%
  group_by(arrival_yearmonth) %>%
  summarise(
    Total_Reservas = n(),
    Reservas_Completadas = sum(is_canceled == 0)
  ) %>%
  arrange(arrival_yearmonth)



# Mostrar tendencia de reservas de forma más clara
cat(yellow("\n--- TENDENCIA DE RESERVAS A LO LARGO DEL TIEMPO ---\n"))

# Calcular tasa de cancelación para cada período
reservas_por_tiempo <- reservas_por_tiempo %>%
  mutate(
    Tasa_Cancelacion = round((Total_Reservas - Reservas_Completadas) / Total_Reservas * 100, 1),
    Periodo = paste(substr(arrival_yearmonth, 1, 4), substr(arrival_yearmonth, 6, 7), sep = "-")
  ) %>%
  select(Periodo, Total_Reservas, Reservas_Completadas, Tasa_Cancelacion)

# Mostrar tabla bonita
cat("\nDatos de reservas por período (primeros 10 meses):\n\n")
print(knitr::kable(head(reservas_por_tiempo, 10),
                   col.names = c("Período", "Total Reservas", "Reservas Completadas", "Tasa Cancelación (%)"),
                   align = c("l", "r", "r", "r"),
                   format = "simple"))

# Mostrar estadísticas de resumen
cat("\nEstadísticas de resumen:\n")
cat("- Período con mayor cantidad de reservas:", 
    reservas_por_tiempo$Periodo[which.max(reservas_por_tiempo$Total_Reservas)], 
    "con", max(reservas_por_tiempo$Total_Reservas), "reservas\n")
cat("- Período con menor cantidad de reservas:", 
    reservas_por_tiempo$Periodo[which.min(reservas_por_tiempo$Total_Reservas)], 
    "con", min(reservas_por_tiempo$Total_Reservas), "reservas\n")
cat("- Tasa de cancelación promedio:", 
    round(mean(reservas_por_tiempo$Tasa_Cancelacion), 1), "%\n")

# Mostrar tendencia principal
primero <- head(reservas_por_tiempo, 1)
ultimo <- tail(reservas_por_tiempo, 1)
cambio_porc <- round((ultimo$Total_Reservas - primero$Total_Reservas) / primero$Total_Reservas * 100, 1)

cat("\nTendencia general:", ifelse(cambio_porc > 0, "AUMENTO", "DISMINUCIÓN"), 
    "del", abs(cambio_porc), "% en reservas totales",
    "desde", primero$Periodo, "hasta", ultimo$Periodo, "\n")




# Primero, preparo los datos calculando porcentajes
reservas_por_tiempo <- hotel_data_limpio %>%
  group_by(arrival_yearmonth) %>%
  summarise(
    Total_Reservas = n(),
    Reservas_Completadas = sum(is_canceled == 0),
    Reservas_Canceladas = sum(is_canceled == 1)
  ) %>%
  mutate(
    Porcentaje_Completadas = round(Reservas_Completadas / Total_Reservas * 100, 1),
    Porcentaje_Canceladas = round(Reservas_Canceladas / Total_Reservas * 100, 1)
  ) %>%
  arrange(arrival_yearmonth)

# Reorganizo los datos para facilitar la visualización apilada
datos_para_grafico <- reservas_por_tiempo %>%
  pivot_longer(
    cols = c(Reservas_Completadas, Reservas_Canceladas),
    names_to = "Estado",
    values_to = "Cantidad"
  ) %>%
  mutate(
    Porcentaje = ifelse(
      Estado == "Reservas_Completadas", 
      Porcentaje_Completadas, 
      Porcentaje_Canceladas
    )
  )

# Creo un gráfico que muestra valores absolutos y porcentajes
plot_tendencia_porcentajes <- ggplot(datos_para_grafico, 
                                     aes(x = arrival_yearmonth, y = Cantidad, 
                                         fill = Estado, group = Estado)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 3) +
  labs(title = "Tendencia de reservas a lo largo del tiempo",
       subtitle = "Mostrando porcentajes de reservas completadas y canceladas",
       x = "Año-Mes",
       y = "Cantidad de reservas",
       fill = "Estado") +
  scale_fill_manual(values = c("Reservas_Completadas" = "#2ecc71", 
                               "Reservas_Canceladas" = "#e74c3c"),
                    labels = c("Completadas", "Canceladas")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

print(plot_tendencia_porcentajes)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "tendencia_reservas_porcentajes.jpg"), 
       plot_tendencia_porcentajes, width = 12, height = 8, dpi = 300)

# También podemos crear un gráfico de líneas que muestre el porcentaje de cancelación a lo largo del tiempo
plot_porcentaje_cancelacion <- ggplot(reservas_por_tiempo, 
                                      aes(x = arrival_yearmonth, y = Porcentaje_Canceladas, group = 1)) +
  geom_line(color = "#e74c3c", size = 1.2) +
  geom_point(color = "#e74c3c", size = 2) +
  geom_text(aes(label = paste0(Porcentaje_Canceladas, "%")), 
            vjust = -0.8, size = 3) +
  labs(title = "Tendencia del porcentaje de cancelaciones",
       subtitle = "Porcentaje de reservas canceladas por mes",
       x = "Año-Mes",
       y = "Porcentaje de cancelación (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot_porcentaje_cancelacion)

# Guardar segunda gráfica
ggsave(file.path(graphics_analysis_dir, "tendencia_porcentaje_cancelacion.jpg"), 
       plot_porcentaje_cancelacion, width = 12, height = 8, dpi = 300)





# Análisis por año con porcentajes
reservas_por_anio <- hotel_data_limpio %>%
  group_by(arrival_date_year) %>%
  summarise(
    Total_Reservas = n(),
    Reservas_Completadas = sum(is_canceled == 0),
    Reservas_Canceladas = sum(is_canceled == 1)
  ) %>%
  mutate(
    Porcentaje_Completadas = round(Reservas_Completadas / Total_Reservas * 100, 1),
    Porcentaje_Canceladas = round(Reservas_Canceladas / Total_Reservas * 100, 1)
  )

cat("\nReservas por año con porcentajes:\n")
print(reservas_por_anio)

# Preparar datos para el gráfico horizontal
datos_anio_apilado <- reservas_por_anio %>%
  pivot_longer(
    cols = c(Reservas_Completadas, Reservas_Canceladas),
    names_to = "Estado",
    values_to = "Cantidad"
  ) %>%
  mutate(
    Porcentaje = ifelse(
      Estado == "Reservas_Completadas", 
      Porcentaje_Completadas, 
      Porcentaje_Canceladas
    )
  )

# Crear el gráfico horizontal de barras apiladas
plot_anios_horizontal <- ggplot(datos_anio_apilado, 
                                aes(x = as.factor(arrival_date_year), 
                                    y = Cantidad, 
                                    fill = Estado, 
                                    group = Estado)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Cantidad, " (", Porcentaje, "%)")), 
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  coord_flip() + # Hace el gráfico horizontal
  labs(title = "Reservas por año",
       subtitle = "Mostrando cantidades y porcentajes de reservas",
       y = "Cantidad de reservas",
       x = "Año",
       fill = "Estado") +
  scale_fill_manual(values = c("Reservas_Completadas" = "#2ecc71", 
                               "Reservas_Canceladas" = "#e74c3c"),
                    labels = c("Completadas", "Canceladas")) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_anios_horizontal)

# Guardar gráfica horizontal
ggsave(file.path(graphics_analysis_dir, "reservas_por_anio_horizontal.jpg"), 
       plot_anios_horizontal, width = 8, height = 6, dpi = 300)





#------------------------------------------
# EDA 03: ¿CUÁLES SON LAS TEMPORADAS DE RESERVAS (ALTA, MEDIA, BAJA)?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE TEMPORADAS ---\n"))

# Agregación por mes
reservas_por_mes <- hotel_data_limpio %>%
  group_by(arrival_date_month) %>%
  summarise(
    Total_Reservas = n(),
    Reservas_Completadas = sum(is_canceled == 0),
    Tasa_Cancelacion = round(sum(is_canceled) / n() * 100, 2)
  ) %>%
  arrange(match(arrival_date_month, c("January", "February", "March", "April", "May", "June", 
                                      "July", "August", "September", "October", "November", "December")))

cat("\nReservas por mes:\n")
print(reservas_por_mes)

# Determinar temporadas basadas en cantidad de reservas
media_reservas <- mean(reservas_por_mes$Total_Reservas)
sd_reservas <- sd(reservas_por_mes$Total_Reservas)

reservas_por_mes$Temporada <- case_when(
  reservas_por_mes$Total_Reservas >= (media_reservas + 0.5 * sd_reservas) ~ "Alta",
  reservas_por_mes$Total_Reservas <= (media_reservas - 0.5 * sd_reservas) ~ "Baja",
  TRUE ~ "Media"
)

cat("\nClasificación de temporadas por mes:\n")
print(reservas_por_mes[, c("arrival_date_month", "Total_Reservas", "Temporada")])

# Visualizar reservas por mes
plot_meses <- ggplot(reservas_por_mes, aes(x = arrival_date_month, y = Total_Reservas, fill = Temporada)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Reservas), vjust = -0.5, color = "black") +
  labs(title = "Reservas por mes y temporada",
       x = "Mes",
       y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_meses)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "reservas_por_mes.jpg"), 
       plot_meses, width = 10, height = 6, dpi = 300)





#------------------------------------------
# EDA 04:  ¿CUÁNDO ES MENOR LA DEMANDA DE RESERVAS?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE PERÍODOS DE BAJA DEMANDA ---\n"))

# Ya identificamos los meses de baja demanda en el análisis anterior
meses_baja_demanda <- reservas_por_mes %>%
  filter(Temporada == "Baja") %>%
  arrange(Total_Reservas)

cat("\nMeses con menor demanda de reservas:\n")
print(meses_baja_demanda)

# Análisis por combinación de mes y día del mes
reservas_por_dia_mes <- hotel_data_limpio %>%
  group_by(arrival_date_month, arrival_date_day_of_month) %>%
  summarise(Total_Reservas = n()) %>%
  arrange(Total_Reservas)

cat("\nCombinaciones de mes y día con menor demanda (10 primeros):\n")
print(head(reservas_por_dia_mes, 10))

# Visualizar los 10 días con menor demanda
dias_menor_demanda <- reservas_por_dia_mes %>%
  arrange(Total_Reservas) %>%
  head(10)

dias_menor_demanda$Fecha <- paste(dias_menor_demanda$arrival_date_month, 
                                  dias_menor_demanda$arrival_date_day_of_month)

# Gráfico con el menor valor arriba
plot_menor_demanda <- ggplot(dias_menor_demanda, 
                             aes(x = reorder(Fecha, Total_Reservas), 
                                 y = Total_Reservas, 
                                 fill = Total_Reservas)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Reservas), hjust = -0.2) +
  labs(title = "10 días con menor demanda de reservas",
       subtitle = "Ordenados de menor a mayor",
       x = "Fecha (Mes-Día)",
       y = "Cantidad de reservas") +
  coord_flip() +
  scale_fill_gradient(low = "#e74c3c", high = "#f39c12") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_menor_demanda)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "dias_menor_demanda.jpg"), 
       plot_menor_demanda, width = 8, height = 6, dpi = 300)





#------------------------------------------
# EDA 05: ¿CUÁNDO ES MAYOR LA DEMANDA DE RESERVAS?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE PERÍODOS DE ALTA DEMANDA ---\n"))

# Identificar meses de alta demanda
meses_alta_demanda <- reservas_por_mes %>%
  filter(Temporada == "Alta") %>%
  arrange(desc(Total_Reservas))

cat("\nMeses con mayor demanda de reservas:\n")
print(meses_alta_demanda)

# Análisis por combinación de mes y día del mes para alta demanda
cat("\nCombinaciones de mes y día con mayor demanda (10 primeros):\n")
dias_mayor_demanda <- reservas_por_dia_mes %>%
  arrange(desc(Total_Reservas)) %>%
  head(10)

print(dias_mayor_demanda)

# Crear etiqueta de fecha
dias_mayor_demanda$Fecha <- paste(dias_mayor_demanda$arrival_date_month, 
                                  dias_mayor_demanda$arrival_date_day_of_month)

# Visualizar los 10 días con mayor demanda
plot_mayor_demanda <- ggplot(dias_mayor_demanda, 
                             aes(x = reorder(Fecha, -Total_Reservas), 
                                 y = Total_Reservas, 
                                 fill = Total_Reservas)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Reservas), hjust = -0.2) +
  labs(title = "10 días con mayor demanda de reservas",
       subtitle = "Ordenados de mayor a menor",
       x = "Fecha (Mes-Día)",
       y = "Cantidad de reservas") +
  coord_flip() +
  scale_fill_gradient(low = "#f39c12", high = "#2ecc71") +  # Invertimos colores: amarillo a verde
  theme_minimal() +
  theme(legend.position = "none")

print(plot_mayor_demanda)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "dias_mayor_demanda.jpg"), 
       plot_mayor_demanda, width = 8, height = 6, dpi = 300)

# Comparativa de meses extremos (mayor y menor demanda)
cat(yellow("\n--- COMPARATIVA DE MESES DE MAYOR Y MENOR DEMANDA ---\n"))

# Obtener el mes de mayor y menor demanda
mes_max_demanda <- reservas_por_mes %>% 
  arrange(desc(Total_Reservas)) %>% 
  slice(1)

mes_min_demanda <- reservas_por_mes %>% 
  arrange(Total_Reservas) %>% 
  slice(1)

cat("\nMes con MAYOR demanda:", mes_max_demanda$arrival_date_month, 
    "con", mes_max_demanda$Total_Reservas, "reservas\n")
cat("Mes con MENOR demanda:", mes_min_demanda$arrival_date_month, 
    "con", mes_min_demanda$Total_Reservas, "reservas\n")

# Crear dataframe para comparativa
meses_extremos <- rbind(
  mes_max_demanda %>% mutate(Tipo = "Mayor demanda"),
  mes_min_demanda %>% mutate(Tipo = "Menor demanda")
)

# Visualizar comparativa
plot_meses_extremos <- ggplot(meses_extremos, 
                              aes(x = arrival_date_month, y = Total_Reservas, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total_Reservas), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparativa entre meses de mayor y menor demanda",
       x = "Mes",
       y = "Cantidad de reservas",
       fill = "") +
  scale_fill_manual(values = c("Mayor demanda" = "#2ecc71", "Menor demanda" = "#e74c3c")) +
  theme_minimal()

print(plot_meses_extremos)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "comparativa_meses_extremos.jpg"), 
       plot_meses_extremos, width = 8, height = 6, dpi = 300)





# Verificar si necesitamos recargar el dataset limpio
if (!exists("hotel_data_limpio") || !is.data.frame(hotel_data_limpio)) {
  cat("Recargando dataset limpio...\n")
  hotel_data_limpio <- read.csv(CSV_limpio, header = TRUE, stringsAsFactors = FALSE)
}

# Asegurar que el directorio para gráficas de análisis existe
graphics_analysis_dir <- file.path(graphics_dir, "analisis")
if (!dir.exists(graphics_analysis_dir)) {
  dir.create(graphics_analysis_dir)
  cat("Creado directorio para gráficas de análisis:", graphics_analysis_dir, "\n")
}




#------------------------------------------
# EDA 06: ¿CUÁNTAS RESERVAS INCLUYEN NIÑOS Y/O BEBÉS?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE RESERVAS CON NIÑOS Y/O BEBÉS ---\n"))

# Crear variables categóricas para facilitar el análisis
hotel_data_limpio$tiene_ninos <- hotel_data_limpio$children > 0
hotel_data_limpio$tiene_bebes <- hotel_data_limpio$babies > 0
hotel_data_limpio$tiene_menores <- hotel_data_limpio$tiene_ninos | hotel_data_limpio$tiene_bebes

# Contar reservas con niños y/o bebés
reservas_con_ninos <- sum(hotel_data_limpio$tiene_ninos)
reservas_con_bebes <- sum(hotel_data_limpio$tiene_bebes)
reservas_con_menores <- sum(hotel_data_limpio$tiene_menores)
total_reservas <- nrow(hotel_data_limpio)

# Calcular porcentajes
porcentaje_ninos <- round(reservas_con_ninos / total_reservas * 100, 2)
porcentaje_bebes <- round(reservas_con_bebes / total_reservas * 100, 2)
porcentaje_menores <- round(reservas_con_menores / total_reservas * 100, 2)

# Mostrar resultados
cat("\nAnálisis de reservas con menores:\n")
cat("- Reservas con niños:", reservas_con_ninos, "(", porcentaje_ninos, "%)\n")
cat("- Reservas con bebés:", reservas_con_bebes, "(", porcentaje_bebes, "%)\n")
cat("- Reservas con niños y/o bebés:", reservas_con_menores, "(", porcentaje_menores, "%)\n")
cat("- Total de reservas:", total_reservas, "\n")

# Crear dataframe para visualización
datos_menores <- data.frame(
  Categoria = c("Con niños", "Con bebés", "Con niños y/o bebés", "Sin menores"),
  Cantidad = c(reservas_con_ninos, reservas_con_bebes, reservas_con_menores, 
               total_reservas - reservas_con_menores)
)

datos_menores$Porcentaje <- round(datos_menores$Cantidad / total_reservas * 100, 2)
datos_menores$Etiqueta <- paste0(datos_menores$Cantidad, "\n(", datos_menores$Porcentaje, "%)")

# Visualizar distribución
plot_menores <- ggplot(datos_menores[c(3,4),], aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = Etiqueta), position = position_stack(vjust = 0.5), color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Proporción de reservas con y sin menores",
       fill = "Categoría") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

print(plot_menores)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "reservas_con_menores.jpg"), 
       plot_menores, width = 8, height = 6, dpi = 300)

# Analizar por tipo de hotel
reservas_menores_hotel <- hotel_data_limpio %>%
  group_by(hotel) %>%
  summarise(
    Total_Reservas = n(),
    Con_Ninos = sum(tiene_ninos),
    Con_Bebes = sum(tiene_bebes),
    Con_Menores = sum(tiene_menores),
    Porcentaje_Menores = round(sum(tiene_menores) / n() * 100, 2)
  )

cat("\nReservas con menores por tipo de hotel:\n")
print(reservas_menores_hotel)

# Visualizar proporción por hotel
plot_menores_hotel <- ggplot(reservas_menores_hotel, 
                             aes(x = hotel, y = Porcentaje_Menores, fill = hotel)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Porcentaje_Menores, "%")), vjust = -0.5) +
  labs(title = "Porcentaje de reservas con menores por tipo de hotel",
       x = "Tipo de hotel",
       y = "Porcentaje de reservas con menores") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_menores_hotel)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "menores_por_hotel.jpg"), 
       plot_menores_hotel, width = 8, height = 6, dpi = 300)

#------------------------------------------
# EDA 07. ¿ES IMPORTANTE CONTAR CON ESPACIOS DE ESTACIONAMIENTO?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE ESPACIOS DE ESTACIONAMIENTO ---\n"))

# Crear variable categórica
hotel_data_limpio$requiere_estacionamiento <- hotel_data_limpio$required_car_parking_spaces > 0

# Contar reservas que requieren estacionamiento
reservas_con_estacionamiento <- sum(hotel_data_limpio$requiere_estacionamiento)
porcentaje_estacionamiento <- round(reservas_con_estacionamiento / total_reservas * 100, 2)

cat("\nAnálisis de necesidad de estacionamiento:\n")
cat("- Reservas que requieren estacionamiento:", reservas_con_estacionamiento, 
    "(", porcentaje_estacionamiento, "%)\n")
cat("- Reservas sin requerimiento de estacionamiento:", total_reservas - reservas_con_estacionamiento, 
    "(", 100 - porcentaje_estacionamiento, "%)\n")

# Crear dataframe para visualización
datos_estacionamiento <- data.frame(
  Categoria = c("Requiere estacionamiento", "No requiere estacionamiento"),
  Cantidad = c(reservas_con_estacionamiento, total_reservas - reservas_con_estacionamiento)
)

datos_estacionamiento$Porcentaje <- round(datos_estacionamiento$Cantidad / total_reservas * 100, 2)
datos_estacionamiento$Etiqueta <- paste0(datos_estacionamiento$Cantidad, 
                                         "\n(", datos_estacionamiento$Porcentaje, "%)")

# Visualizar distribución
plot_estacionamiento <- ggplot(datos_estacionamiento, aes(x = "", y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = Etiqueta), position = position_stack(vjust = 0.5), color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Proporción de reservas que requieren estacionamiento",
       fill = "Categoría") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

print(plot_estacionamiento)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "necesidad_estacionamiento.jpg"), 
       plot_estacionamiento, width = 8, height = 6, dpi = 300)

# Análisis por tipo de hotel
estacionamiento_por_hotel <- hotel_data_limpio %>%
  group_by(hotel) %>%
  summarise(
    Total_Reservas = n(),
    Requiere_Estacionamiento = sum(requiere_estacionamiento),
    Porcentaje_Estacionamiento = round(sum(requiere_estacionamiento) / n() * 100, 2)
  )

cat("\nNecesidad de estacionamiento por tipo de hotel:\n")
print(estacionamiento_por_hotel)

# Visualizar proporción por hotel
plot_estacionamiento_hotel <- ggplot(estacionamiento_por_hotel, 
                                     aes(x = hotel, y = Porcentaje_Estacionamiento, fill = hotel)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Porcentaje_Estacionamiento, "%")), vjust = -0.5) +
  labs(title = "Porcentaje de reservas que requieren estacionamiento por tipo de hotel",
       x = "Tipo de hotel",
       y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_estacionamiento_hotel)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "estacionamiento_por_hotel.jpg"), 
       plot_estacionamiento_hotel, width = 8, height = 6, dpi = 300)

# Análisis adicional: relación entre estacionamiento y tipo de cliente
estacionamiento_por_cliente <- hotel_data_limpio %>%
  group_by(customer_type) %>%
  summarise(
    Total_Reservas = n(),
    Requiere_Estacionamiento = sum(requiere_estacionamiento),
    Porcentaje_Estacionamiento = round(sum(requiere_estacionamiento) / n() * 100, 2)
  ) %>%
  arrange(desc(Porcentaje_Estacionamiento))

cat("\nNecesidad de estacionamiento por tipo de cliente:\n")
print(estacionamiento_por_cliente)

# Visualizar por tipo de cliente
plot_estacionamiento_cliente <- ggplot(estacionamiento_por_cliente, 
                                       aes(x = reorder(customer_type, Porcentaje_Estacionamiento), 
                                           y = Porcentaje_Estacionamiento, 
                                           fill = customer_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Porcentaje_Estacionamiento, "%")), vjust = -0.5) +
  labs(title = "Porcentaje de reservas que requieren estacionamiento por tipo de cliente",
       x = "Tipo de cliente",
       y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_estacionamiento_cliente)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "estacionamiento_por_cliente.jpg"), 
       plot_estacionamiento_cliente, width = 10, height = 6, dpi = 300)



#------------------------------------------
# EDA 08. ¿EN QUÉ MESES DEL AÑO SE PRODUCEN MÁS CANCELACIONES DE RESERVAS?
#------------------------------------------
cat(yellow("\n--- ANÁLISIS DE CANCELACIONES POR MES ---\n"))

# Analizar cancelaciones por mes
cancelaciones_por_mes <- hotel_data_limpio %>%
  group_by(arrival_date_month) %>%
  summarise(
    Total_Reservas = n(),
    Canceladas = sum(is_canceled),
    Tasa_Cancelacion = round(sum(is_canceled) / n() * 100, 2)
  ) %>%
  arrange(match(arrival_date_month, c("January", "February", "March", "April", "May", "June", 
                                      "July", "August", "September", "October", "November", "December")))

cat("\nCancelaciones por mes:\n")
print(cancelaciones_por_mes)

# Visualizar tasa de cancelación por mes
plot_cancelaciones <- ggplot(cancelaciones_por_mes, 
                             aes(x = arrival_date_month, y = Tasa_Cancelacion, fill = Tasa_Cancelacion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Tasa_Cancelacion, "%")), vjust = -0.5) +
  labs(title = "Tasa de cancelación por mes",
       x = "Mes",
       y = "Porcentaje de cancelación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_gradient(low = "lightblue", high = "darkred")

print(plot_cancelaciones)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "tasa_cancelacion_mes.jpg"), 
       plot_cancelaciones, width = 10, height = 6, dpi = 300)

# Visualizar cancelaciones absolutas por mes
plot_cancelaciones_abs <- ggplot(cancelaciones_por_mes, 
                                 aes(x = arrival_date_month)) +
  geom_bar(aes(y = Total_Reservas, fill = "Total Reservas"), stat = "identity") +
  geom_bar(aes(y = Canceladas, fill = "Canceladas"), stat = "identity", alpha = 0.7) +
  geom_text(aes(y = Canceladas, label = Canceladas), vjust = -0.5, size = 3) +
  labs(title = "Cantidad de cancelaciones por mes",
       x = "Mes",
       y = "Cantidad de reservas",
       fill = "Tipo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_cancelaciones_abs)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "cancelaciones_absolutas_mes.jpg"), 
       plot_cancelaciones_abs, width = 10, height = 6, dpi = 300)

# Análisis por tipo de hotel
cancelaciones_por_hotel_mes <- hotel_data_limpio %>%
  group_by(hotel, arrival_date_month) %>%
  summarise(
    Total_Reservas = n(),
    Canceladas = sum(is_canceled),
    Tasa_Cancelacion = round(sum(is_canceled) / n() * 100, 2)
  ) %>%
  arrange(hotel, match(arrival_date_month, c("January", "February", "March", "April", "May", "June", 
                                             "July", "August", "September", "October", "November", "December")))

cat("\nTasas de cancelación por hotel y mes (primeras filas):\n")
print(head(cancelaciones_por_hotel_mes, 10))

# Visualizar tasa de cancelación por hotel y mes
plot_cancelacion_hotel <- ggplot(cancelaciones_por_hotel_mes, 
                                 aes(x = arrival_date_month, y = Tasa_Cancelacion, 
                                     fill = hotel, group = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(Tasa_Cancelacion, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
  labs(title = "Tasa de cancelación por hotel y mes",
       x = "Mes",
       y = "Porcentaje de cancelación",
       fill = "Tipo de hotel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_cancelacion_hotel)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "cancelacion_hotel_mes.jpg"), 
       plot_cancelacion_hotel, width = 12, height = 6, dpi = 300)

#------------------------------------------
# EDA 09: ANÁLISIS ADICIONAL - RELACIÓN ENTRE VARIABLES CLAVE
#------------------------------------------
cat(yellow("\n--- ANÁLISIS ADICIONAL: RELACIONES ENTRE VARIABLES ---\n"))

# Análisis de relación entre lead_time y cancelación
lead_time_cancelacion <- hotel_data_limpio %>%
  group_by(is_canceled) %>%
  summarise(
    Promedio_Lead_Time = mean(lead_time),
    Mediana_Lead_Time = median(lead_time),
    Count = n()
  )

cat("\nRelación entre tiempo de anticipación (lead_time) y cancelación:\n")
print(lead_time_cancelacion)

# Visualizar lead_time por estado de cancelación
plot_lead_time <- ggplot(hotel_data_limpio, aes(x = factor(is_canceled), y = lead_time, fill = factor(is_canceled))) +
  geom_boxplot() +
  labs(title = "Distribución de lead_time según estado de cancelación",
       x = "Cancelada (0=No, 1=Sí)",
       y = "Tiempo de anticipación (días)",
       fill = "Cancelada") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_lead_time)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "lead_time_cancelacion.jpg"), 
       plot_lead_time, width = 8, height = 6, dpi = 300)

# Análisis de duración de estancia
hotel_data_limpio$total_nights <- hotel_data_limpio$stays_in_weekend_nights + hotel_data_limpio$stays_in_week_nights

duracion_estancia <- hotel_data_limpio %>%
  group_by(hotel) %>%
  summarise(
    Promedio_Noches = mean(total_nights),
    Mediana_Noches = median(total_nights),
    Max_Noches = max(total_nights),
    Min_Noches = min(total_nights)
  )

cat("\nDuración promedio de estancia por tipo de hotel:\n")
print(duracion_estancia)

# Visualizar duración de estancia por tipo de hotel
plot_duracion <- ggplot(hotel_data_limpio, aes(x = hotel, y = total_nights, fill = hotel)) +
  geom_boxplot() +
  labs(title = "Distribución de duración de estancia por tipo de hotel",
       x = "Tipo de hotel",
       y = "Número total de noches",
       fill = "Tipo de hotel") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_duracion)

# Guardar gráfica
ggsave(file.path(graphics_analysis_dir, "duracion_estancia.jpg"), 
       plot_duracion, width = 8, height = 6, dpi = 300)



#------------------------------------------
# PARTE 07.5 RESUMEN DE EDA
#------------------------------------------

cat("\n1. Análisis por tipo de hotel:\n")
cat("   - El hotel tipo '", reservas_por_hotel_df$Tipo_Hotel[which.max(reservas_por_hotel_df$Cantidad)], 
    "' tiene mayor cantidad de reservas (", 
    reservas_por_hotel_df$Cantidad[which.max(reservas_por_hotel_df$Cantidad)], 
    " reservas, ", reservas_por_hotel_df$Porcentaje[which.max(reservas_por_hotel_df$Cantidad)], "%).\n", sep="")
cat("   - Para reservas completadas, el hotel tipo '", 
    reservas_completadas_df$Tipo_Hotel[which.max(reservas_completadas_df$Reservas_Completadas)], 
    "' sigue siendo el preferido.\n")

cat("\n2. Tendencia de demanda:\n")
ultimo_anio <- max(reservas_por_anio$arrival_date_year)
penultimo_anio <- ultimo_anio - 1
cambio_porcentual <- round(
  (reservas_por_anio$Total_Reservas[reservas_por_anio$arrival_date_year == ultimo_anio] - 
     reservas_por_anio$Total_Reservas[reservas_por_anio$arrival_date_year == penultimo_anio]) / 
    reservas_por_anio$Total_Reservas[reservas_por_anio$arrival_date_year == penultimo_anio] * 100, 2
)
cat("   - Comparando ", ultimo_anio, " con ", penultimo_anio, ", hubo un cambio del ", 
    cambio_porcentual, "% en el número total de reservas.\n", sep="")
cat("   - La tendencia general indica que ", 
    ifelse(cambio_porcentual > 0, "está aumentando", "está disminuyendo"), 
    " la demanda con el tiempo.\n")
cat("   - Tasa de cancelación en ", ultimo_anio, ": ", 
    reservas_por_anio$Porcentaje_Canceladas[reservas_por_anio$arrival_date_year == ultimo_anio], 
    "%, frente a ", 
    reservas_por_anio$Porcentaje_Canceladas[reservas_por_anio$arrival_date_year == penultimo_anio], 
    "% en ", penultimo_anio, ".\n", sep="")

cat("\n3. Temporadas de reserva:\n")
cat("   - Temporada ALTA: ", paste(unique(reservas_por_mes$arrival_date_month[reservas_por_mes$Temporada == "Alta"]), collapse=", "), "\n")
cat("   - Temporada MEDIA: ", paste(unique(reservas_por_mes$arrival_date_month[reservas_por_mes$Temporada == "Media"]), collapse=", "), "\n")
cat("   - Temporada BAJA: ", paste(unique(reservas_por_mes$arrival_date_month[reservas_por_mes$Temporada == "Baja"]), collapse=", "), "\n")

cat("\n4. Períodos de menor demanda:\n")
cat("   - El mes con MENOR demanda es: ", meses_baja_demanda$arrival_date_month[1], 
    " con ", meses_baja_demanda$Total_Reservas[1], " reservas.\n")
cat("   - La combinación mes-día con MENOR demanda es: ", 
    dias_menor_demanda$arrival_date_month[1], "-", dias_menor_demanda$arrival_date_day_of_month[1], 
    " con ", dias_menor_demanda$Total_Reservas[1], " reservas.\n")

cat("\n5. Períodos de mayor demanda:\n")
cat("   - El mes con MAYOR demanda es: ", meses_alta_demanda$arrival_date_month[1], 
    " con ", meses_alta_demanda$Total_Reservas[1], " reservas.\n")
cat("   - La combinación mes-día con MAYOR demanda es: ", 
    dias_mayor_demanda$arrival_date_month[1], "-", dias_mayor_demanda$arrival_date_day_of_month[1], 
    " con ", dias_mayor_demanda$Total_Reservas[1], " reservas.\n")

cat("\n6. Comparativa extremos de demanda:\n")
diferencia_extremos <- mes_max_demanda$Total_Reservas - mes_min_demanda$Total_Reservas
porcentaje_diferencia <- round((diferencia_extremos / mes_min_demanda$Total_Reservas) * 100, 2)

cat("   - Diferencia entre el mes de mayor y menor demanda: ", diferencia_extremos, 
    " reservas (", porcentaje_diferencia, "% más en temporada alta).\n", sep="")
cat("   - El mes de máxima demanda (", mes_max_demanda$arrival_date_month, 
    ") tiene ", round(mes_max_demanda$Total_Reservas/30), " reservas diarias en promedio.\n", sep="")
cat("   - El mes de mínima demanda (", mes_min_demanda$arrival_date_month, 
    ") tiene ", round(mes_min_demanda$Total_Reservas/30), " reservas diarias en promedio.\n", sep="")

# Análisis de días de la semana si están disponibles
if ("arrival_date_day_of_week" %in% colnames(hotel_data_limpio) || 
    "arrival_date_day_of_week" %in% names(hotel_data_limpio)) {
  
  # Si tenemos información del día de la semana
  cat("\n7. Patrones por día de la semana:\n")
  cat("   - El análisis por día de la semana muestra patrones adicionales en la demanda.\n")
  cat("   - [Aquí iría el análisis de días de la semana si estuviera disponible]\n")
}



cat("\n8. Análisis de reservas con niños y/o bebés:\n")
cat("   - Un", porcentaje_menores, "% de las reservas incluyen niños y/o bebés (", reservas_con_menores, "reservas).\n")
cat("   - El hotel tipo '", reservas_menores_hotel$hotel[which.max(reservas_menores_hotel$Porcentaje_Menores)], 
    "' tiene mayor porcentaje de reservas con menores (", 
    reservas_menores_hotel$Porcentaje_Menores[which.max(reservas_menores_hotel$Porcentaje_Menores)], "%).\n")

cat("\n9. Importancia de espacios de estacionamiento:\n")
cat("   - Solamente el", porcentaje_estacionamiento, 
    "% de las reservas requieren espacios de estacionamiento.\n")
cat("   - Por tipo de hotel, '", 
    estacionamiento_por_hotel$hotel[which.max(estacionamiento_por_hotel$Porcentaje_Estacionamiento)], 
    "' tiene mayor demanda de estacionamiento (", 
    estacionamiento_por_hotel$Porcentaje_Estacionamiento[which.max(estacionamiento_por_hotel$Porcentaje_Estacionamiento)], 
    "%).\n")
cat("   - El tipo de cliente con mayor demanda de estacionamiento es '", 
    estacionamiento_por_cliente$customer_type[1], "' con ", 
    estacionamiento_por_cliente$Porcentaje_Estacionamiento[1], "% de reservas.\n")

cat("\n10. Meses con más cancelaciones:\n")
cat("   - El mes con mayor tasa de cancelación es '", 
    cancelaciones_por_mes$arrival_date_month[which.max(cancelaciones_por_mes$Tasa_Cancelacion)], 
    "' (", cancelaciones_por_mes$Tasa_Cancelacion[which.max(cancelaciones_por_mes$Tasa_Cancelacion)], "%).\n")
cat("   - El mes con mayor cantidad absoluta de cancelaciones es '", 
    cancelaciones_por_mes$arrival_date_month[which.max(cancelaciones_por_mes$Canceladas)], 
    "' (", cancelaciones_por_mes$Canceladas[which.max(cancelaciones_por_mes$Canceladas)], " reservas canceladas).\n")

cat("\n4. Hallazgos adicionales:\n")
cat("   - Las reservas que terminan canceladas tienen un tiempo de anticipación (lead_time) promedio de ", 
    lead_time_cancelacion$Promedio_Lead_Time[lead_time_cancelacion$is_canceled == 1], 
    " días, mientras que las no canceladas tienen ", 
    lead_time_cancelacion$Promedio_Lead_Time[lead_time_cancelacion$is_canceled == 0], " días en promedio.\n")
cat("   - La duración promedio de estancia en el hotel tipo '", 
    duracion_estancia$hotel[1], "' es de ", 
    round(duracion_estancia$Promedio_Noches[1], 2), " noches, y en '", 
    duracion_estancia$hotel[2], "' es de ", 
    round(duracion_estancia$Promedio_Noches[2], 2), " noches.\n")






#####################################################
# PARTE 08: VISUALIZACIONES Y CONCLUSIONES FINALES
#####################################################

cat(green("\n=== RONDA 6: VISUALIZACIONES Y CONCLUSIONES FINALES ===\n"))

# Verificar si necesitamos recargar el dataset limpio
if (!exists("hotel_data_limpio") || !is.data.frame(hotel_data_limpio)) {
  cat("Recargando dataset limpio...\n")
  hotel_data_limpio <- read.csv(CSV_limpio, header = TRUE, stringsAsFactors = FALSE)
}

# Crear directorio para gráficas finales
graphics_final_dir <- file.path(graphics_dir, "final")
if (!dir.exists(graphics_final_dir)) {
  dir.create(graphics_final_dir)
  cat("Creado directorio para gráficas finales:", graphics_final_dir, "\n")
}

# Definir una paleta de colores consistente para todas las visualizaciones
colores_principales <- c("#3498db", "#2ecc71", "#e74c3c", "#f39c12", "#9b59b6")
colores_hotel <- c("City Hotel" = "#3498db", "Resort Hotel" = "#2ecc71")

#------------------------------------------
# PARTE 08.5: VISUALIZACIONES CONSOLIDADAS PARA CADA PREGUNTA CLAVE
#------------------------------------------
cat(yellow("\n--- VISUALIZACIONES CONSOLIDADAS ---\n"))

# 1.1 Demanda por tipo de hotel y estado de cancelación
cat("Creando visualización: Demanda por tipo de hotel...\n")

# Preparar datos
demanda_hotel <- hotel_data_limpio %>%
  group_by(hotel, is_canceled) %>%
  summarise(Cantidad = n()) %>%
  mutate(Estado = ifelse(is_canceled == 0, "Confirmada", "Cancelada"))

# Crear gráfico
plot_demanda_hotel <- ggplot(demanda_hotel, 
                             aes(x = hotel, y = Cantidad, fill = Estado)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Cantidad), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Demanda por Tipo de Hotel",
       subtitle = "Desglose por estado de reserva",
       x = "Tipo de Hotel",
       y = "Número de Reservas") +
  scale_fill_manual(values = c("Confirmada" = "#2ecc71", "Cancelada" = "#e74c3c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

print(plot_demanda_hotel)

# Guardar gráfica
ggsave(file.path(graphics_final_dir, "1_demanda_hotel.jpg"), 
       plot_demanda_hotel, width = 10, height = 7, dpi = 300)

# 1.2 Tendencia de demanda a lo largo del tiempo
cat("Creando visualización: Tendencia de demanda a lo largo del tiempo...\n")

# Preparar datos para tendencia temporal
tendencia_temporal <- hotel_data_limpio %>%
  mutate(YearMonth = paste(arrival_date_year, 
                           sprintf("%02d", as.numeric(factor(arrival_date_month, 
                                                             levels = c("January", "February", "March", "April", "May", "June", 
                                                                        "July", "August", "September", "October", "November", "December")))),
                           sep = "-")) %>%
  group_by(YearMonth, hotel) %>%
  summarise(Total_Reservas = n()) %>%
  arrange(YearMonth)

# Crear gráfico de tendencia
plot_tendencia_final <- ggplot(tendencia_temporal, 
                               aes(x = YearMonth, y = Total_Reservas, color = hotel, group = hotel)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Evolución de la Demanda a lo Largo del Tiempo",
       subtitle = "Desglose por tipo de hotel",
       x = "Año-Mes",
       y = "Número de Reservas",
       color = "Tipo de Hotel") +
  scale_color_manual(values = colores_hotel) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        legend.position = "bottom")

print(plot_tendencia_final)

# Guardar gráfica
ggsave(file.path(graphics_final_dir, "2_tendencia_demanda.jpg"), 
       plot_tendencia_final, width = 12, height = 7, dpi = 300)

# 1.3 Temporadas de reserva
cat("Creando visualización: Temporadas de reserva...\n")

# Preparar datos por mes
reservas_por_mes <- hotel_data_limpio %>%
  group_by(arrival_date_month, hotel) %>%
  summarise(Total_Reservas = n()) %>%
  mutate(arrival_date_month = factor(arrival_date_month, 
                                     levels = c("January", "February", "March", "April", "May", "June", 
                                                "July", "August", "September", "October", "November", "December")))

# Determinar temporadas
total_por_mes <- reservas_por_mes %>%
  group_by(arrival_date_month) %>%
  summarise(Total = sum(Total_Reservas))

media_reservas <- mean(total_por_mes$Total)
sd_reservas <- sd(total_por_mes$Total)

total_por_mes$Temporada <- case_when(
  total_por_mes$Total >= (media_reservas + 0.5 * sd_reservas) ~ "Alta",
  total_por_mes$Total <= (media_reservas - 0.5 * sd_reservas) ~ "Baja",
  TRUE ~ "Media"
)

# Unir datos de temporada con el desglose por hotel
reservas_por_mes <- reservas_por_mes %>%
  left_join(total_por_mes[, c("arrival_date_month", "Temporada")], by = "arrival_date_month")

# Crear gráfico de temporadas
plot_temporadas <- ggplot(reservas_por_mes, 
                          aes(x = arrival_date_month, y = Total_Reservas, fill = Temporada)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Reservas), 
            position = position_stack(vjust = 0.95), 
            color = "white", size = 3) +
  facet_wrap(~ hotel) +
  labs(title = "Temporadas de Reserva por Mes",
       subtitle = "Clasificación en temporada alta, media y baja",
       x = "Mes",
       y = "Número de Reservas",
       fill = "Temporada") +
  scale_fill_manual(values = c("Alta" = "#e74c3c", "Media" = "#f39c12", "Baja" = "#3498db")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(plot_temporadas)

# Guardar gráfica
ggsave(file.path(graphics_final_dir, "3_temporadas_reserva.jpg"), 
       plot_temporadas, width = 12, height = 8, dpi = 300)

# 1.4 Reservas con niños y/o bebés
cat("Creando visualización: Reservas con niños y/o bebés...\n")

# Preparar datos
hotel_data_limpio$tiene_ninos <- hotel_data_limpio$children > 0
hotel_data_limpio$tiene_bebes <- hotel_data_limpio$babies > 0
hotel_data_limpio$tipo_familia <- case_when(
  hotel_data_limpio$tiene_ninos & hotel_data_limpio$tiene_bebes ~ "Con niños y bebés",
  hotel_data_limpio$tiene_ninos ~ "Solo con niños",
  hotel_data_limpio$tiene_bebes ~ "Solo con bebés",
  TRUE ~ "Sin menores"
)

familias_por_hotel <- hotel_data_limpio %>%
  group_by(hotel, tipo_familia) %>%
  summarise(Cantidad = n()) %>%
  group_by(hotel) %>%
  mutate(Porcentaje = round(Cantidad / sum(Cantidad) * 100, 1))

# Crear gráfico de familias
plot_familias <- ggplot(familias_por_hotel, 
                        aes(x = hotel, y = Porcentaje, fill = tipo_familia)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  labs(title = "Composición de Reservas por Tipo de Familia",
       subtitle = "Desglose por presencia de niños y/o bebés",
       x = "Tipo de Hotel",
       y = "Porcentaje",
       fill = "Tipo de Familia") +
  scale_fill_manual(values = c("Sin menores" = "#95a5a6", 
                               "Solo con niños" = "#3498db", 
                               "Solo con bebés" = "#2ecc71",
                               "Con niños y bebés" = "#e74c3c")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

print(plot_familias)

# Guardar gráfica
ggsave(file.path(graphics_final_dir, "4_reservas_familias.jpg"), 
       plot_familias, width = 10, height = 7, dpi = 300)

# 1.5 Estacionamiento y cancelaciones
cat("Creando visualización: Estacionamiento y cancelaciones por mes...\n")

# Panel A: Estacionamiento
estacionamiento_datos <- hotel_data_limpio %>%
  group_by(hotel) %>%
  summarise(
    Total = n(),
    Con_Estacionamiento = sum(required_car_parking_spaces > 0),
    Porcentaje = round(Con_Estacionamiento / Total * 100, 1)
  )

plot_estacionamiento_final <- ggplot(estacionamiento_datos, 
                                     aes(x = hotel, y = Porcentaje, fill = hotel)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 4) +
  labs(title = "Necesidad de Estacionamiento",
       x = "Tipo de Hotel",
       y = "Porcentaje de Reservas") +
  scale_fill_manual(values = colores_hotel) +
  ylim(0, max(estacionamiento_datos$Porcentaje) * 1.2) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Panel B: Cancelaciones por mes
cancelaciones_mes <- hotel_data_limpio %>%
  group_by(arrival_date_month) %>%
  summarise(
    Total = n(),
    Canceladas = sum(is_canceled),
    Tasa_Cancelacion = round(Canceladas / Total * 100, 1)
  ) %>%
  mutate(arrival_date_month = factor(arrival_date_month, 
                                     levels = c("January", "February", "March", "April", "May", "June", 
                                                "July", "August", "September", "October", "November", "December")))

plot_cancelaciones_final <- ggplot(cancelaciones_mes, 
                                   aes(x = arrival_date_month, y = Tasa_Cancelacion, fill = Tasa_Cancelacion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Tasa_Cancelacion, "%")), vjust = -0.5, size = 3) +
  labs(title = "Tasa de Cancelación por Mes",
       x = "Mes",
       y = "Porcentaje de Cancelación") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))


if (!require("grid", quietly = TRUE)) install.packages("grid")
library(grid)

# Combinar gráficos en un panel con título
combined_plot <- grid.arrange(
  plot_estacionamiento_final, 
  plot_cancelaciones_final, 
  ncol = 1,
  top = textGrob("Necesidad de Estacionamiento y Cancelaciones por Mes",
                 gp = gpar(fontsize = 16, fontface = "bold"))
)
print(combined_plot)

# Guardar gráfico combinado
ggsave(file.path(graphics_final_dir, "5_estacionamiento_cancelaciones.jpg"), 
       combined_plot, width = 10, height = 10, dpi = 300)



#------------------------------------------
# PARTE 09: TABLAS RESUMEN PARA ASPECTOS CLAVE
#------------------------------------------
cat(yellow("\n--- TABLAS RESUMEN ---\n"))

# 9.1 Top 5 meses con mayor demanda
top_meses_demanda <- hotel_data_limpio %>%
  group_by(arrival_date_month) %>%
  summarise(Total_Reservas = n()) %>%
  arrange(desc(Total_Reservas)) %>%
  head(5)

cat("\nTop 5 meses con mayor demanda:\n")
print(top_meses_demanda)

# 9.2 Top 5 meses con mayor tasa de cancelación
top_meses_cancelacion <- hotel_data_limpio %>%
  group_by(arrival_date_month) %>%
  summarise(
    Total_Reservas = n(),
    Canceladas = sum(is_canceled),
    Tasa_Cancelacion = round(Canceladas / Total_Reservas * 100, 2)
  ) %>%
  arrange(desc(Tasa_Cancelacion)) %>%
  head(5)

cat("\nTop 5 meses con mayor tasa de cancelación:\n")
print(top_meses_cancelacion)

# 9.3 Comparación entre tipos de hotel (aspectos clave)
comparacion_hoteles <- hotel_data_limpio %>%
  group_by(hotel) %>%
  summarise(
    Total_Reservas = n(),
    Porcentaje_del_Total = round(n() / nrow(hotel_data_limpio) * 100, 2),
    Tasa_Cancelacion = round(sum(is_canceled) / n() * 100, 2),
    Promedio_Lead_Time = round(mean(lead_time), 2),
    Promedio_Estancia = round(mean(stays_in_weekend_nights + stays_in_week_nights), 2),
    Porcentaje_Con_Niños = round(sum(children > 0) / n() * 100, 2),
    Porcentaje_Con_Estacionamiento = round(sum(required_car_parking_spaces > 0) / n() * 100, 2),
    ADR_Promedio = round(mean(adr), 2)
  )

cat("\nComparación entre tipos de hotel:\n")
print(comparacion_hoteles)

# Crear tabla para guardar
comparacion_tabla <- tableGrob(comparacion_hoteles, rows = NULL, theme = ttheme_minimal())

# Guardar tabla como imagen
ggsave(file.path(graphics_final_dir, "6_comparacion_hoteles.jpg"), 
       comparacion_tabla, width = 12, height = 4, dpi = 300)

# 9.4 Relación entre lead_time y cancelación
relacion_lead_cancelacion <- hotel_data_limpio %>%
  mutate(lead_time_cat = cut(lead_time, 
                             breaks = c(-1, 7, 30, 90, 180, Inf),
                             labels = c("0-7 días", "8-30 días", "31-90 días", "91-180 días", "Más de 180 días"))) %>%
  group_by(lead_time_cat) %>%
  summarise(
    Total_Reservas = n(),
    Canceladas = sum(is_canceled),
    Tasa_Cancelacion = round(Canceladas / Total_Reservas * 100, 2)
  )

cat("\nRelación entre tiempo de anticipación (lead_time) y tasa de cancelación:\n")
print(relacion_lead_cancelacion)

# Visualizar relación
plot_lead_cancelacion <- ggplot(relacion_lead_cancelacion, 
                                aes(x = lead_time_cat, y = Tasa_Cancelacion, fill = Tasa_Cancelacion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Tasa_Cancelacion, "%")), vjust = -0.5) +
  labs(title = "Relación entre Tiempo de Anticipación y Tasa de Cancelación",
       x = "Tiempo de Anticipación",
       y = "Tasa de Cancelación (%)") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "none")

print(plot_lead_cancelacion)

# Guardar gráfica
ggsave(file.path(graphics_final_dir, "7_lead_time_cancelacion.jpg"), 
       plot_lead_cancelacion, width = 10, height = 6, dpi = 300)

#------------------------------------------
# PARTE 10. CONCLUSIONES GENERALES
#------------------------------------------
cat(yellow("\n--- CONCLUSIONES GENERALES ---\n"))

cat("\n1. Demanda por tipo de hotel:\n")
cat("   - El City Hotel representa el", 
    comparacion_hoteles$Porcentaje_del_Total[comparacion_hoteles$hotel == "City Hotel"], 
    "% del total de reservas, frente al", 
    comparacion_hoteles$Porcentaje_del_Total[comparacion_hoteles$hotel == "Resort Hotel"], 
    "% del Resort Hotel, lo que indica una clara preferencia por hoteles urbanos.\n")
cat("   - Sin embargo, la tasa de cancelación del City Hotel (", 
    comparacion_hoteles$Tasa_Cancelacion[comparacion_hoteles$hotel == "City Hotel"], 
    "%) es significativamente mayor que la del Resort Hotel (", 
    comparacion_hoteles$Tasa_Cancelacion[comparacion_hoteles$hotel == "Resort Hotel"], 
    "%), lo que puede afectar a la rentabilidad real.\n")

cat("\n2. Tendencia de demanda:\n")
cat("   - La demanda muestra claras fluctuaciones estacionales con picos en los meses de verano.\n")
cat("   - Los meses con mayor demanda son:", 
    paste(top_meses_demanda$arrival_date_month[1:3], collapse=", "), ".\n")
cat("   - Se observa una tendencia general ", 
    ifelse(comparacion_hoteles$Total_Reservas[2] > comparacion_hoteles$Total_Reservas[1], 
           "creciente", "decreciente"), 
    " en el número total de reservas a lo largo del tiempo analizado.\n")

cat("\n3. Temporadas de reserva:\n")
cat("   - Temporada ALTA: Claramente identificada en los meses de ", 
    paste(reservas_por_mes$arrival_date_month[reservas_por_mes$Temporada == "Alta" & 
                                                !duplicated(reservas_por_mes$arrival_date_month)], 
          collapse=", "), ".\n")
cat("   - Temporada BAJA: Principalmente en los meses de ", 
    paste(reservas_por_mes$arrival_date_month[reservas_por_mes$Temporada == "Baja" & 
                                                !duplicated(reservas_por_mes$arrival_date_month)], 
          collapse=", "), ".\n")
cat("   - Esta marcada estacionalidad sugiere oportunidades para estrategias de precios dinámicos y promociones.\n")

cat("\n4. Reservas con niños y/o bebés:\n")
cat("   - Solo el", round(sum(hotel_data_limpio$children > 0 | hotel_data_limpio$babies > 0) / nrow(hotel_data_limpio) * 100, 2), 
    "% de las reservas totales incluyen niños y/o bebés.\n")
cat("   - El Resort Hotel tiene un porcentaje mayor de reservas con niños (", 
    comparacion_hoteles$Porcentaje_Con_Niños[comparacion_hoteles$hotel == "Resort Hotel"], 
    "%) en comparación con el City Hotel (", 
    comparacion_hoteles$Porcentaje_Con_Niños[comparacion_hoteles$hotel == "City Hotel"], 
    "%), lo que sugiere que es más atractivo para familias.\n")

cat("\n5. Espacios de estacionamiento:\n")
cat("   - La demanda de espacios de estacionamiento es baja en general, con solo un ", 
    round(sum(hotel_data_limpio$required_car_parking_spaces > 0) / nrow(hotel_data_limpio) * 100, 2), 
    "% del total de reservas.\n")
cat("   - El Resort Hotel tiene mayor demanda de estacionamiento (", 
    comparacion_hoteles$Porcentaje_Con_Estacionamiento[comparacion_hoteles$hotel == "Resort Hotel"], 
    "%) que el City Hotel (", 
    comparacion_hoteles$Porcentaje_Con_Estacionamiento[comparacion_hoteles$hotel == "City Hotel"], 
    "%), posiblemente debido a su ubicación más alejada del centro urbano.\n")

cat("\n6. Cancelaciones:\n")
cat("   - La tasa global de cancelación es del", 
    round(sum(hotel_data_limpio$is_canceled) / nrow(hotel_data_limpio) * 100, 2), "%.\n")
cat("   - Los meses con mayor tasa de cancelación son: ", 
    paste(top_meses_cancelacion$arrival_date_month[1:3], collapse=", "), ".\n")
cat("   - Existe una clara correlación entre el tiempo de anticipación (lead_time) y la probabilidad de cancelación: ",
    "reservas hechas con más de 180 días de anticipación tienen una tasa de cancelación del ", 
    relacion_lead_cancelacion$Tasa_Cancelacion[relacion_lead_cancelacion$lead_time_cat == "Más de 180 días"], 
    "%, mientras que las realizadas con menos de 7 días tienen solo un ", 
    relacion_lead_cancelacion$Tasa_Cancelacion[relacion_lead_cancelacion$lead_time_cat == "0-7 días"], "%.\n")

#------------------------------------------
# PARTE 11: RECOMENDACIONES ESTRATÉGICAS
#------------------------------------------
cat(yellow("\n--- RECOMENDACIONES ESTRATÉGICAS ---\n"))

cat("\n1. Gestión de demanda y precios:\n")
cat("   - Implementar precios dinámicos más agresivos durante la temporada alta (", 
    paste(reservas_por_mes$arrival_date_month[reservas_por_mes$Temporada == "Alta" & 
                                                !duplicated(reservas_por_mes$arrival_date_month)], 
          collapse=", "), ").\n")
cat("   - Desarrollar paquetes especiales y promociones para impulsar la demanda durante los meses de temporada baja (", 
    paste(reservas_por_mes$arrival_date_month[reservas_por_mes$Temporada == "Baja" & 
                                                !duplicated(reservas_por_mes$arrival_date_month)], 
          collapse=", "), ").\n")
cat("   - Para el City Hotel, enfocarse en mejorar la tasa de conversión y reducir cancelaciones, que actualmente son más altas que en el Resort Hotel.\n")

cat("\n2. Segmentación de clientes:\n")
cat("   - Resort Hotel: Desarrollar más servicios y amenidades orientados a familias con niños, dado su mayor porcentaje de este tipo de reservas (", 
    comparacion_hoteles$Porcentaje_Con_Niños[comparacion_hoteles$hotel == "Resort Hotel"], "%).\n")
cat("   - City Hotel: Enfocarse en el segmento de viajeros individuales y parejas sin niños, que constituyen la gran mayoría de su clientela.\n")
cat("   - Dado que solo el", 
    round(sum(hotel_data_limpio$required_car_parking_spaces > 0) / nrow(hotel_data_limpio) * 100, 2), 
    "% de reservas requieren estacionamiento, considerar reducir espacios de estacionamiento o reutilizarlos para otras amenidades, especialmente en el City Hotel.\n")

cat("\n3. Gestión de cancelaciones:\n")
cat("   - Implementar políticas escalonadas de depósito basadas en el tiempo de anticipación, con mayores garantías para reservas con más de 90 días de antelación, que tienen tasas de cancelación del ", 
    relacion_lead_cancelacion$Tasa_Cancelacion[relacion_lead_cancelacion$lead_time_cat == "91-180 días"], 
    "% o superiores.\n")
cat("   - Desarrollar estrategias específicas de retención para los meses con mayor tasa de cancelación (", 
    paste(top_meses_cancelacion$arrival_date_month[1:3], collapse=", "), 
    "), como recordatorios personalizados, confirmación proactiva, o incentivos para mantener la reserva.\n")
cat("   - Considerar implementar un sistema de overbooking inteligente basado en patrones históricos de cancelación por temporada, tipo de cliente y tiempo de anticipación.\n")

cat("\n4. Desarrollo de productos y servicios:\n")
cat("   - Dado que la estancia promedio es relativamente corta (", 
    round(mean(hotel_data_limpio$stays_in_weekend_nights + hotel_data_limpio$stays_in_week_nights), 2), 
    " noches), desarrollar paquetes que incentiven estancias más largas, especialmente en temporada baja.\n")
cat("   - Para el Resort Hotel, continuar desarrollando instalaciones atractivas para familias con niños, que constituyen un segmento importante.\n")
cat("   - Para el City Hotel, enfocarse en comodidades y servicios que atraigan a viajeros de negocios y turistas urbanos sin niños.\n")

#------------------------------------------
# PARTE ADICIONAL 1: GUARDAR DATAFRAME FINAL CON VARIABLES ADICIONALES CALCULADAS
#------------------------------------------
cat(yellow("\n--- GUARDANDO DATASET FINAL ---\n"))

# Agregar variables calculadas durante el análisis
hotel_data_final <- hotel_data_limpio

# Calcular duración total de estancia
hotel_data_final$total_nights <- hotel_data_final$stays_in_weekend_nights + hotel_data_final$stays_in_week_nights

# Clasificación por temporada
temporadas_por_mes <- reservas_por_mes %>%
  select(arrival_date_month, Temporada) %>%
  distinct()

hotel_data_final <- hotel_data_final %>%
  left_join(temporadas_por_mes, by = "arrival_date_month")

# Clasificación por tipo de familia
hotel_data_final$tipo_familia <- case_when(
  hotel_data_final$children > 0 & hotel_data_final$babies > 0 ~ "Con niños y bebés",
  hotel_data_final$children > 0 ~ "Solo con niños",
  hotel_data_final$babies > 0 ~ "Solo con bebés",
  TRUE ~ "Sin menores"
)

# Clasificación por tiempo de anticipación
hotel_data_final$lead_time_categoria <- cut(hotel_data_final$lead_time, 
                                            breaks = c(-1, 7, 30, 90, 180, Inf),
                                            labels = c("0-7 días", "8-30 días", "31-90 días", "91-180 días", "Más de 180 días"))

# Guardar el dataset final
write.csv(hotel_data_final, CSV_final, row.names = FALSE)
cat("Dataset final guardado en:", CSV_final, "\n")

# Mostrar estructura del dataset final
cat("\nEstructura del dataset final:\n")
str(hotel_data_final[, c("hotel", "is_canceled", "lead_time", 
                         "lead_time_categoria", "arrival_date_month", 
                         "Temporada", "total_nights", "tipo_familia")])

#------------------------------------------
# PARTE ADICIONAL 2: RESUMEN DE ARCHIVOS GENERADOS
#------------------------------------------
cat(yellow("\n--- RESUMEN DE ARCHIVOS GENERADOS ---\n"))

cat("\nDatasets:\n")
cat("- Dataset original:", CSV_original, "\n")
cat("- Dataset limpio:", CSV_limpio, "\n")
cat("- Dataset final:", CSV_final, "\n")

cat("\nGráficas finales guardadas en:", graphics_final_dir, "\n")
cat("- 1_demanda_hotel.jpg: Análisis de demanda por tipo de hotel\n")
cat("- 2_tendencia_demanda.jpg: Tendencia de demanda a lo largo del tiempo\n")
cat("- 3_temporadas_reserva.jpg: Temporadas de reserva por mes\n")
cat("- 4_reservas_familias.jpg: Composición de reservas por tipo de familia\n")
cat("- 5_estacionamiento_cancelaciones.jpg: Análisis de estacionamiento y cancelaciones\n")
cat("- 6_comparacion_hoteles.jpg: Tabla comparativa entre tipos de hotel\n")
cat("- 7_lead_time_cancelacion.jpg: Relación entre tiempo de anticipación y cancelación\n")

cat(green("\n=== ANÁLISIS EXPLORATORIO COMPLETADO EXITOSAMENTE ===\n"))


sink()

