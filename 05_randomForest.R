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
  "NDVI_mean",  
  "NDVI_seasonal_diff", 
  "pr_mean", "pr_seasonal_diff", 
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

################# CROSS VALIDATION ###########################################


# Crear conjuntos de entrenamiento y prueba
sets <- create_training_and_test_sets(14:16, 17, opt_1)
training_set <- sets$training_set %>%
  mutate(TGAP = TMAX - TMIN) %>%
  select(-TMAX, -TMIN)
test_set <- sets$test_set %>%
  mutate(TGAP = TMAX - TMIN) %>%
  select(-TMAX, -TMIN)


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
  rsample::vfold_cv(v = 3, strata = "malnutrition", breaks = 3)

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
    size =  10
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