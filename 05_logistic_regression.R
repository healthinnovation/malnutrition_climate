library(tidymodels)
library(xgboost)
library(haven)
library(magrittr)
library(fs)
library(readr)
library(dplyr)
library(dials)
library(corrplot)
library(ggplot2)
library(caret)

# Cargar los datos
path_2014 <- path("dataset/malnutrition_final14.csv")
malnutrition_14 <- read_csv(path_2014) %>%
  mutate(HHID = as.character(HHID)) 

path_2015 <- path("dataset/malnutrition_final15.csv")
malnutrition_15 <- read_csv(path_2015)

path_2016 <- path("dataset/malnutrition_final16.csv")
malnutrition_16 <- read_csv(path_2016)

path_2017 <- path("dataset/malnutrition_final17.csv")
malnutrition_17 <- read_csv(path_2017)

path_2018 <- path("dataset/malnutrition_final18.csv")
malnutrition_18 <- read_csv(path_2018)

path_2019 <- path("dataset/malnutrition_final19.csv")
malnutrition_19 <- read_csv(path_2019)


# Función para ajustar el modelo y realizar la evaluación
train_and_evaluate <- function(training_set, test_set, verbose = TRUE) {
  
  training_set <- training_set %>%
    dplyr::select(
      malnutrition, NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
      NDVI_seasonal_diff, pr_mean, pr_sd, pr_median, pr_IQR, 
      pr_last_months, pr_first_months, pr_seasonal_diff, TGAP) %>%
    mutate(malnutrition = factor(malnutrition))
  
  test_set <- test_set %>%
    dplyr::select(
      malnutrition, NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
      NDVI_seasonal_diff, pr_mean, pr_sd, pr_median, pr_IQR, 
      pr_last_months, pr_first_months, pr_seasonal_diff, TGAP) %>%
    mutate(malnutrition = factor(malnutrition))
  
  # Ajustar el modelo de regresión logística usando la función glm
  logistic_model <- glm(malnutrition ~ ., data = training_set, family = binomial)
  
  # Predecir las probabilidades de pertenencia a la clase positiva para el conjunto de prueba
  y_pred_probs <- predict(logistic_model, newdata = test_set, type = "response")
  
  if (verbose) {
    cat("Predictions made on", nrow(test_set), "samples.\n")
  }
  
  # Aplicar un umbral de clasificación para obtener las etiquetas de clase
  y_pred <- ifelse(y_pred_probs > 0.2, 1, 0)
  
  # Calcular la precisión del modelo
  accuracy <- sum(y_pred == test_set$malnutrition) / nrow(test_set)
  
  # Calcular la matriz de confusión
  confusion_matrix <- table(factor(y_pred, levels = c(0, 1)), factor(test_set$malnutrition, levels = c(0, 1)))
  
  # Retornar la precisión y la matriz de confusión
  return(list(accuracy = accuracy, confusion_matrix = confusion_matrix))
}

# Crear una lista para almacenar los resultados
results <- list()

# Entrenar y evaluar para cada conjunto de datos
datasets <- list(malnutrition_14, malnutrition_15, malnutrition_16, malnutrition_17, malnutrition_18, malnutrition_19)
for (i in seq_along(datasets)) {
  training_set <- bind_rows(datasets[1:i])  # Combinar los conjuntos de datos anteriores
  test_set <- datasets[[i+1]]
  
  # Mensaje de avance
  cat("Training and evaluating for", i+1, "vs", i, "...\n")
  
  # Llamar a la función train_and_evaluate
  result <- train_and_evaluate(training_set, test_set)
  
  # Almacenar los resultados
  results[[i]] <- result
}

# Imprimir los resultados
for (i in seq_along(results)) {
  cat("Result for", i+1, "vs", i, ":", "\n")
  cat("Accuracy:", results[[i]]$accuracy, "\n")
  cat("Confusion Matrix:\n")
  print(results[[i]]$confusion_matrix)
  cat("\n")
}


