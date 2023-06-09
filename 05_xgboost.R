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



training_set <- data.frame(rbind(malnutrition_14, malnutrition_15, 
                                 malnutrition_16, malnutrition_17, 
                                 malnutrition_18)) %>% # 
  dplyr::select(
    malnutrition, NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
    NDVI_seasonal_diff, pr_mean, pr_sd, pr_median, pr_IQR, 
    pr_last_months, pr_first_months, pr_seasonal_diff, TGAP) %>%
  mutate(malnutrition = factor(malnutrition))


#Colinealidad

# Calcular la matriz de correlación entre todas las variables numéricas
corr_matrix <- cor(training_set[sapply(training_set, is.numeric)], use = "pairwise.complete.obs")

# Identificar pares de variables con alta correlación
high_corr <- which(upper.tri(corr_matrix, diag = FALSE) & abs(corr_matrix) > 0.8, arr.ind = TRUE)

# Crear un dataframe para almacenar las variables con su correlación
correlation_df <- data.frame(Variable1 = colnames(corr_matrix)[high_corr[, 1]],
                             Variable2 = colnames(corr_matrix)[high_corr[, 2]],
                             Correlation = corr_matrix[high_corr])



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

# Definir el modelo
xgb_spec <- boost_tree(
  trees = 500,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  learn_rate = tune()) %>%
  set_engine("xgboost", nthreads = parallel::detectCores()) %>%
  set_mode("classification") %>%
  set_args(scoring = "log_loss", verbose = 1, early_stopping_rounds = 10) %>%
  set_mode("classification")

# grid specification
xgb_params <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )

xgb_grid <- 
  dials::grid_random(
    xgb_params, 
    size = 50
  )
knitr::kable(head(xgb_grid))

xgb_wf <- 
  workflows::workflow() %>%
  add_model(xgb_spec) %>% 
  add_formula(malnutrition ~ .)


# hyperparameter tuning
xgb_tuned <- tune::tune_grid(
  object = xgb_wf,
  resamples = malnutrition_preprocessed,
  grid = xgb_grid,
  metrics = yardstick::metric_set(accuracy, precision, recall),
  control = tune::control_grid(verbose = TRUE)
)


#Mejores parámetros para ver accuracy recall y precision

library(kableExtra)

params_xg_ndvi <- xgb_tuned %>%
  tune::show_best(metric = "accuracy") %>%
  dplyr::slice(1:20) %>%
  knitr::kable() %>%
  kable_styling("striped")

models_results<- collect_metrics(xgb_tuned)

writeLines(capture.output(params_xg_ndvi), "params_xg_ndvi.html")

xgb_best_params <- xgb_tuned %>%
  tune::select_best("accuracy")

knitr::kable(xgb_best_params)


xgb_model_final <- xgb_spec %>% 
  finalize_model(xgb_best_params)

##############################################################################

#xgb_model_final <- readRDS("modelo_xgb_1.rds")

train_processed <- bake(rec_preprocessed,  new_data = data_train)
train_prediction <- xgb_model_final %>%
  # fit the model on all the training data
  fit(
    formula = malnutrition ~ ., 
    data    = train_processed
  ) %>%
  # predict the sale prices for the training data
  predict(new_data = train_processed) %>%
  bind_cols(data_train)
xgb_score_train <- 
  train_prediction %>%
  yardstick::metrics(malnutrition, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(xgb_score_train)

test_processed  <- bake(rec_preprocessed, new_data = data_test)
# Modificar umbral de clasificación a 0.7
test_prediction <- xgb_model_final %>%
  fit(
    formula = malnutrition ~ ., 
    data = train_processed
  ) %>%
  predict(new_data = test_processed, type = "prob") %>%
  mutate(.pred_class = if_else(.pred_1 > 0.3, "1", "0")) %>%
  bind_cols(data_test)

# Medir la precisión del modelo utilizando yardstick con el nuevo umbral
xgb_score <- 
  test_prediction %>%
  mutate(.pred_class = as.factor(.pred_class)) %>%
  yardstick::metrics(malnutrition, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))


knitr::kable(xgb_score)

# obtain confusion matrix
xgb_confusion <- test_prediction %>% 
  mutate(.pred_class = as.factor(.pred_class)) %>%
  yardstick::conf_mat(malnutrition, .pred_class)

saveRDS(xgb_model_final, file = "modelo_xgb__NDVI_maxsens.rds")