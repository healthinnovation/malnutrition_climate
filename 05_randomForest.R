library(tidymodels)
library(randomForest)
library(haven)
library(magrittr)
library(fs)
library(readr)
library(dplyr)
library(dials)
library(tune)
library(corrplot)
library(ggplot2)
library(caret)

# Cargar los datos
source("load_data.R")
dataset <- load_data()

malnutrition_14 <- dataset$malnutrition_14
malnutrition_15 <- dataset$malnutrition_15
malnutrition_14 <- dataset$malnutrition_14
malnutrition_16 <- dataset$malnutrition_16
malnutrition_17 <- dataset$malnutrition_17
malnutrition_18 <- dataset$malnutrition_18
malnutrition_19 <- dataset$malnutrition_19


# Función para ajustar el modelo y realizar la evaluación
train_and_evaluate <- function(training_set, test_set, verbose = TRUE) {
  
  training_set <- training_set %>%
    dplyr::select(
      malnutrition, longitudx, latitudy, NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
      NDVI_seasonal_diff, pr_mean, pr_sd, pr_median, pr_IQR, 
      pr_last_months, pr_first_months, pr_seasonal_diff, TGAP) %>%
    mutate(malnutrition = factor(malnutrition))
  
  test_set <- test_set %>%
    dplyr::select(
      malnutrition, longitudx, latitudy, NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
      NDVI_seasonal_diff, pr_mean, pr_sd, pr_median, pr_IQR, 
      pr_last_months, pr_first_months, pr_seasonal_diff, TGAP) %>%
    mutate(malnutrition = factor(malnutrition))
  
  # Escalar las variables numéricas en el conjunto de entrenamiento y prueba
  numeric_vars <- c("NDVI_mean", "NDVI_sd", "NDVI_median", "NDVI_IQR", "NDVI_last_months", "NDVI_first_months", 
                    "NDVI_seasonal_diff", "pr_mean", "pr_sd", "pr_median", "pr_IQR", 
                    "pr_last_months", "pr_first_months", "pr_seasonal_diff", "TGAP")
  
  training_set[, numeric_vars] <- scale(training_set[, numeric_vars])
  test_set[, numeric_vars] <- scale(test_set[, numeric_vars])
  
  # Ajustar el modelo utilizando Random Forest
  rf_model <- randomForest(malnutrition ~ ., data = training_set)
  
  # Predecir las probabilidades de pertenencia a la clase positiva para el conjunto de prueba
  y_pred_probs <- predict(rf_model, newdata = test_set, type = "response")
  
  if (verbose) {
    cat("Predictions made on", nrow(test_set), "samples.\n")
  }
  
  # Convertir los factores a valores numéricos
  y_pred_probs <- as.numeric(levels(y_pred_probs))[y_pred_probs]
  
  # Aplicar un umbral de clasificación para obtener las etiquetas de clase
  y_pred <- ifelse(y_pred_probs > 0.001, 1, 0)
  
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
for (i in 1:(length(datasets) - 1)) {
  training_set <- bind_rows(datasets[1:i])  # Combinar los conjuntos de datos anteriores
  if (i < length(datasets)) {
    test_set <- datasets[[i+1]]
    
    # Mensaje de avance
    cat("Training and evaluating for", i+1, "vs", i, "...\n")
    
    # Llamar a la función train_and_evaluate
    result <- train_and_evaluate(training_set, test_set)
    
    # Almacenar los resultados
    results[[i]] <- result
  }
}


# Imprimir los resultados
for (i in seq_along(results)) {
  cat("Result for", i+1, "vs", i, ":", "\n")
  cat("Accuracy:", results[[i]]$accuracy, "\n")
  cat("Confusion Matrix:\n")
  print(results[[i]]$confusion_matrix)
  cat("\n")
}


################# CROSS VALIDATION ###########################################


training_set <- data.frame(rbind(malnutrition_14, malnutrition_15, malnutrition_16, malnutrition_17, malnutrition_18)) %>%
  dplyr::select(
    malnutrition, NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
    NDVI_seasonal_diff, pr_mean, pr_sd, pr_median, pr_IQR, 
    pr_last_months, pr_first_months, pr_seasonal_diff, TGAP) %>%
  mutate(malnutrition = factor(malnutrition))

test_set <- malnutrition_19 %>%
  dplyr::select(
    malnutrition, NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
    NDVI_seasonal_diff, pr_mean, pr_sd, pr_median, pr_IQR, 
    pr_last_months, pr_first_months, pr_seasonal_diff, TGAP) %>%
  mutate(malnutrition = factor(malnutrition))

set.seed(1501)

#Train test split

data_train <- training_set
data_test  <- test_set

# Crear un objeto recipe para preprocesamiento
rec <- recipe(malnutrition ~ ., data = data_train) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# Preprocesamiento
rec_preprocessed <- prep(rec)

# Aplicar receta de preprocesamiento a los datos
malnutrition_preprocessed <-
  recipes::bake(
    rec_preprocessed, 
    new_data = data_train
  ) %>%  
  rsample::vfold_cv(v = 3, strata = "malnutrition", breaks = 5)

# Definir la fórmula del modelo
formula <- "malnutrition ~ ."

rf_spec <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = tune()
) %>%
  set_engine("randomForest") %>%
  set_mode("classification")

rf_params <- 
  dials::parameters(
    mtry(),
    min_n(),
    trees()
  ) %>%
  dials::finalize(data_train %>% select(-malnutrition))

rf_grid <- 
  dials::grid_random(
    rf_params, 
    size = 50
  )

knitr::kable(head(rf_grid))

rf_wf <- 
  workflows::workflow() %>%
  add_model(rf_spec) %>% 
  add_formula(malnutrition ~ .)


# Sintonización de hiperparámetros
rf_tuned <- tune::tune_grid(
  object = rf_wf,
  resamples = malnutrition_preprocessed,
  grid = rf_grid,
  metrics = yardstick::metric_set(accuracy, f_meas),
  control = tune::control_grid(verbose = TRUE)
)

#Mejores parámetros para ver accuracy recall y precision

library(kableExtra)

params_rf_ndvi <- rf_tuned %>%
  tune::show_best(metric = "f_meas") %>%
  dplyr::slice(1:20) %>%
  knitr::kable() %>%
  kable_styling("striped")

models_results<- collect_metrics(rf_tuned)

# Guardar la tabla en formato CSV
write_csv(models_results, "RF_cv_5.csv")

writeLines(capture.output(params_rf_ndvi), "params_rf_ndvi_5.html")

rf_best_params <- rf_tuned %>%
  tune::select_best("f_meas")

knitr::kable(rf_best_params)


rf_model_final <- rf_spec %>% 
  finalize_model(rf_best_params)

## EVALUACION ############
train_processed <- bake(rec_preprocessed,  new_data = data_train)
train_prediction <- rf_model_final %>%
  # fit the model on all the training data
  fit(
    formula = malnutrition ~ ., 
    data    = train_processed
  ) %>%
  # predict the sale prices for the training data
  predict(new_data = train_processed) %>%
  bind_cols(data_train)
rf_score_train <- 
  train_prediction %>%
  yardstick::metrics(malnutrition, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(rf_score_train)

test_processed  <- bake(rec_preprocessed, new_data = data_test)
# Modificar umbral de clasificación a 0.7
test_prediction <- rf_model_final %>%
  fit(
    formula = malnutrition ~ ., 
    data = train_processed
  ) %>%
  predict(new_data = test_processed, type = "prob") %>%
  mutate(.pred_class = if_else(.pred_1 > 0.1, "1", "0")) %>%
  bind_cols(data_test)

# Medir la precisión del modelo utilizando yardstick con el nuevo umbral
rf_score <- 
  test_prediction %>%
  mutate(.pred_class = as.factor(.pred_class)) %>%
  yardstick::metrics(malnutrition, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))


knitr::kable(rf_score)

# obtain confusion matrix
rf_confusion <- test_prediction %>% 
  mutate(.pred_class = as.factor(.pred_class)) %>%
  yardstick::conf_mat(malnutrition, .pred_class)

saveRDS(rf_model_final, file = "modelo_rf__NDVI_maxfmeas_5.rds")