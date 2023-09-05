library(tidyverse)
library(tidymodels)
library(haven)
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(infer)
library(naivebayes)


# 1. Function to read all years.................................................
set.seed(123)

read_and_mutate_malnutrition <- function() {
  malnutrition_list <- list()
  for (year in 14:19) {
    path <- file.path(paste0("dataset/malnutrition_final", year, ".csv"))
    malnutrition <- read_csv(path) %>%
      mutate(HHID = as.character(HHID))
    malnutrition_list[[paste0("malnutrition_", year)]] <- malnutrition
  }
  return(malnutrition_list)
}

# Llama a la función para cargar los datos de todos los años
malnutrition_data <- read_and_mutate_malnutrition()



# 1.1. Crear los vectores con variables ........................................

# Vector general de variables descriptivas
opt_1 <- c(
  "NDVI_mean", "NDVI_sd", "NDVI_median", "NDVI_IQR", 
  "NDVI_last_months", "NDVI_first_months", "NDVI_seasonal_diff", 
  "pr_mean", "pr_sd", "pr_median", "pr_IQR", "pr_last_months", 
  "pr_first_months", "pr_seasonal_diff", 
  "TMAX", "TMIN"
)

# Vector de umbrales 90/10
opt_2 <- c(
  "NDVI_high_90", "NDVI_low_10", "pr_high_90", "pr_low_10",
  "tmax_high_90", "tmin_low_10"
)

# Vector de umbrales 99/1
opt_3 <- c(
  "NDVI_high_99", "NDVI_low_1", "pr_high_99", "pr_low_1",
  "tmax_high_99", "tmin_low_1"
)

# Vector de umbrales 95/5
opt_4 <- c(
  "NDVI_high_95", "NDVI_low_5", "pr_high_95", "pr_low_5",
  "tmax_high_95", "tmin_low_5"
)

# Vector de meses consecutivos por encima o debajo de umbral
opt_5 <- c(
  "consecutivas_veg_90", "consecutivas_veg_10",
  "consecutivas_prec_90", "consecutivas_prec_10",
  "consecutivas_tmax_90", "consecutivas_tmax_10",
  "consecutivas_tmin_90", "consecutivas_tmin_10"
)

# Vector de grupos de meses consecutivos 
opt_6 <- c(
  "grupos_veg_90", "grupos_veg_10",
  "grupos_prec_90", "grupos_prec_10",
  "grupos_tmax_90", "grupos_tmax_10",
  "grupos_tmin_90", "grupos_tmin_10"
)



# 2. Function to create train and test set .....................................

create_training_and_test_sets <- function(train_years, test_year, selected_vars) {
  # Crear una lista para almacenar los conjuntos de datos de entrenamiento
  training_data_list <- list()
  
  # Iterar a través de los años de entrenamiento
  for (year in train_years) {
    # Obtener el conjunto de datos correspondiente al año
    data <- malnutrition_data[[paste0("malnutrition_", year)]]
    
    # Seleccionar las columnas relevantes y convertir malnutrition a factor
    data <- data %>%
      select(
       malnutrition,  all_of(selected_vars)) %>%
      mutate(malnutrition = as.factor(malnutrition))
    
    # Almacenar el conjunto de datos en la lista
    training_data_list[[year]] <- data
  }
  
  # Combinar todos los conjuntos de datos de entrenamiento en uno solo
  training_set <- do.call(rbind, training_data_list)
  
  # Obtener el conjunto de datos correspondiente al año de prueba
  test_set <- malnutrition_data[[paste0("malnutrition_", test_year)]]
  
  # Seleccionar las mismas columnas y convertir malnutrition a factor en el conjunto de prueba
  test_set <- test_set %>%
    select(
      malnutrition,  all_of(selected_vars)) %>%
    mutate(malnutrition = as.factor(malnutrition))
  
  # Retornar el conjunto de entrenamiento y el conjunto de prueba
  return(list(training_set = training_set, test_set = test_set))
}



# 3. Entrenar el modelo -> Naive Bayes .........................................

train_and_evaluate_naive_bayes_models <- function(option) {
  results <- list()
  
  for (eval_year in 15:19) {
    train_years <- 14:(eval_year - 1)
    test_year <- eval_year
    
    # Crear conjuntos de entrenamiento y prueba
    sets <- create_training_and_test_sets(train_years, test_year, option)
    training_set <- sets$training_set
    test_set <- sets$test_set
    
    # Entrenar el modelo Naive Bayes Multinomial
    nv_model <- naive_bayes(malnutrition ~ ., data = training_set)
    
    # Realizar predicciones en el conjunto de prueba PROBS
    predictions <- predict(nv_model, test_set, type = 'prob')
    
    # Convertir probabilidades en clases (usando un umbral de 0.5)
    predictions_yes <- ifelse(predictions[, "1"] >= 0.5, 1, 0)
    
    # Calcular la matriz de confusión
    confusion_matrix <- table(predictions_yes, test_set$malnutrition)
    
    # Almacenar resultados en la lista
    results[[paste0("Model_", eval_year)]] <- confusion_matrix
  }
  
  return(results)
}



# 4. Matrices de confusión ........................................


# Llama a la función para entrenar y evaluar los modelos
confusion_matrices <- train_and_evaluate_naive_bayes_models(opt_6)

for (i in 1:5) {
  cat("Model_", i + 14, ":\n")
  print(confusion_matrices[[i]])
  cat("\n")
}
