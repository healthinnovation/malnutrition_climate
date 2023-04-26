library(tidymodels)
library(haven)
library(fs)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(randomForest)
library(reshape2)
library(caret)
library(rsample)
library(recipes)

tidymodels_prefer()

path_2014 <- path("dataset/malnutrition_final14.csv")
malnutrition_14 <- read_csv(path_2014) %>%
  mutate(HHID = as.character(HHID))%>% # En este a?o el HHID era num?rico
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate(malnutrition = factor(malnutrition))

glimpse(malnutrition_14)

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

#Train test split

set.seed(1501)
data_train <- malnutrition_14
data_test  <- malnutrition_15

set.seed(1502)
data_folds <- 
  vfold_cv(data_train, strata = malnutrition, repeats = 5)

#Creating two recipes

#KNN, Neural Networks, SVM
normalized_rec <- 
  recipe(malnutrition ~ ., data = data_train) %>% 
  step_normalize(all_predictors()) 

poly_recipe <- 
  normalized_rec %>% 
  step_poly(all_predictors()) %>% 
  step_interact(~ all_predictors():all_predictors())


#For the models, we use the the parsnip addin to create a set of model specifications:

library(rules)
library(baguette)

# nnet_spec <- 
#   mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
#   set_engine("nnet", MaxNWts = 2600) %>% 
#   set_mode("classification")

mars_spec <- 
  mars(prod_degree = tune()) %>%  #<- use GCV to choose terms
  set_engine("earth") %>% 
  set_mode("classification")

svm_r_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

svm_p_spec <- 
  svm_poly(cost = tune(), degree = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

knn_spec <- 
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

bag_cart_spec <- 
  bag_tree() %>% 
  set_engine("rpart", times = 50L) %>% 
  set_mode("classification")

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 50) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

xgb_spec <- 
  boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
             min_n = tune(), sample_size = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# nnet_param <- 
#   nnet_spec %>% 
#   extract_parameter_set_dials() %>% 
#   update(hidden_units = hidden_units(c(1, 27)))



#Workflow set


# Needs normalization
normalized <- 
  workflow_set(
    preproc = list(normalized = normalized_rec), 
    models = list(SVM_radial = svm_r_spec, SVM_poly = svm_p_spec, 
                  KNN = knn_spec)# , neural_network = nnet_spec)
  )

# normalized <- 
#   normalized %>% 
#   option_add(param_info = nnet_param, id = "normalized_neural_network")

# Does not need normalization

model_vars <- 
  workflow_variables(outcomes = malnutrition, 
                     predictors = everything())

no_pre_proc <- 
  workflow_set(
    preproc = list(simple = model_vars), 
    models = list(MARS = mars_spec, CART = cart_spec, CART_bagged = bag_cart_spec,
                  RF = rf_spec, boosting = xgb_spec)
  )

# Assemble the set that uses nonlinear terms and interactions

with_features <- 
  workflow_set(
    preproc = list(full_quad = poly_recipe), 
    models = list(KNN = knn_spec)
  )

# Merge all of them with rbind

all_workflows <- 
  bind_rows(no_pre_proc, normalized, with_features) %>% 
  # Make the workflow ID's a little more simple: 
  mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))

# EFFICIENTLY SCREENING MODELS

library(finetune)
library(progressr)

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

race_results <- all_workflows %>%
    workflow_map(
      "tune_race_anova",
      seed = 1503,
      resamples = data_folds,
      grid = 25,
      control = race_ctrl
    )


autoplot(
  race_results,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(3.0, 9.5)) +
  theme(legend.position = "none")


# FINALIZING A MODEL

best_results <- 
  race_results %>% 
  extract_workflow_set_result("boosting") %>% 
  select_best(metric = "rmse")
# best_results


boosting_test_results <- 
  race_results %>% 
  extract_workflow("boosting") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = concrete_split)

collect_metrics(boosting_test_results)


boosting_test_results %>% 
  collect_predictions() %>% 
  ggplot(aes(x = malnutrition, y = .pred)) + 
  geom_abline(color = "gray50", lty = 2) + 
  geom_point(alpha = 0.5) + 
  coord_obs_pred() + 
  labs(x = "observed", y = "predicted")