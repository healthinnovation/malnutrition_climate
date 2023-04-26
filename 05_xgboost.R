library(tidymodels)
library(xgboost)
library(haven)
library(magrittr)
library(fs)
library(readr)
library(dplyr)
library(dials)

# Cargar los datos
path_2014 <- path("dataset/malnutrition_final14.csv")
malnutrition_14 <- read_csv(path_2014) %>%
  mutate(HHID = as.character(HHID)) %>% # En este año el HHID era numérico
  dplyr::select(
    malnutrition, longitudx, latitudy, 
    NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, NDVI_seasonal_diff,
    EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, EVI_first_months, EVI_seasonal_diff,
    pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, pr_seasonal_diff
  ) %>%
  mutate(malnutrition = factor(malnutrition))

path_2015 <- path("dataset/malnutrition_final15.csv")
malnutrition_15 <- read_csv(path_2015) %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate(malnutrition = factor(malnutrition))



set.seed(1501)

#Train test split

data_train <- malnutrition_14
data_test  <- malnutrition_15

# Crear un objeto recipe para preprocesamiento
rec <- recipe(malnutrition ~ ., data = data_train) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# Preprocesamiento
rec_preprocessed <- prep(rec)

# Aplicar receta de preprocesamiento a los datos
malnutrition_preprocessed_14 <-
  recipes::bake(
    rec_preprocessed, 
    new_data = data_train
  ) %>%  
  rsample::vfold_cv(v = 5)

# Definir la fórmula del modelo
formula <- "malnutrition ~ ."

# Definir el modelo
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = 0.8,
  mtry = 3,
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
  dials::grid_max_entropy(
    xgb_params, 
    size = 60
  )
knitr::kable(head(xgb_grid))

xgb_wf <- 
  workflows::workflow() %>%
  add_model(xgb_spec) %>% 
  add_formula(malnutrition ~ .)


# hyperparameter tuning
xgb_tuned <- tune::tune_grid(
  object = xgb_wf,
  resamples = malnutrition_preprocessed_14,
  grid = xgb_grid,
  metrics = yardstick::metric_set(accuracy, precision, recall),
  control = tune::control_grid(verbose = TRUE)
)


xgb_tuned %>%
  tune::show_best(metric = "accuracy") %>%
  knitr::kable()


xgb_best_params <- xgb_tuned %>%
  tune::select_best("accuracy")

knitr::kable(xgb_best_params)

xgb_model_final <- xgb_spec %>% 
  finalize_model(xgb_best_params)



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
test_prediction <- xgb_model_final %>%
  # fit the model on all the training data
  fit(
    formula = malnutrition ~ ., 
    data    = train_processed
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(data_test)
# measure the accuracy of our model using `yardstick`
xgb_score <- 
  test_prediction %>%
  yardstick::metrics(malnutrition, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

knitr::kable(xgb_score)

# obtain confusion matrix
xgb_confusion <- train_prediction %>% 
  yardstick::conf_mat(malnutrition, .pred_class)

