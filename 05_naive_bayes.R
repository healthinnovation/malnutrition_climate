library(tidyverse)
library(tidymodels)
library(haven)
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(infer)
library(naivebayes)

path_2014 <- file.path("dataset/malnutrition_final14.csv")
malnutrition_14 <- read_csv(path_2014) %>%
  mutate(HHID = as.character(HHID)) 

path_2015 <- file.path("dataset/malnutrition_final15.csv")
malnutrition_15 <- read_csv(path_2015)


malnutrition_14_naives <- malnutrition_14 %>%
  select(
    -HHID, -area_residence, -region, -conglomerado,
    -NDVI_mean, -NDVI_sd, -NDVI_median, -NDVI_IQR, -NDVI_last_months, 
    -NDVI_first_months, -NDVI_seasonal_diff, -pr_mean, -pr_sd, -pr_median, 
    -pr_IQR, -pr_last_months, -pr_first_months, -pr_seasonal_diff, -TMAX, 
    -TMIN) %>%
  select(malnutrition, NDVI_high_90, NDVI_low_10, pr_high_90, pr_low_10,
         tmax_high_90, tmin_low_10, consecutivas_veg_90, consecutivas_veg_10,
         consecutivas_prec_90, consecutivas_prec_10, consecutivas_tmax_90,
         consecutivas_tmax_10, consecutivas_tmin_90, consecutivas_tmin_10,
         grupos_veg_90, grupos_veg_10, grupos_prec_90, grupos_prec_10,
         grupos_tmax_90, grupos_tmax_10, grupos_tmin_90, grupos_tmin_10) %>%
  mutate(malnutrition = as.factor(malnutrition))


set.seed(123)

indices_entrenamiento <- sample(nrow(malnutrition_14_naives), 
                                round(nrow(malnutrition_14_naives)), 
                                replace = FALSE)

training_set <- malnutrition_14_naives[indices_entrenamiento, ]



malnutrition_15_naives <- malnutrition_15 %>%
  #group_by(malnutrition) %>% 
  #slice_sample( prop = 1) %>%
  #ungroup %>%
  select(
    -HHID, -area_residence, -region, -conglomerado,
    -NDVI_mean, -NDVI_sd, -NDVI_median, -NDVI_IQR, -NDVI_last_months, 
    -NDVI_first_months, -NDVI_seasonal_diff, -pr_mean, -pr_sd, -pr_median, 
    -pr_IQR, -pr_last_months, -pr_first_months, -pr_seasonal_diff, -TMAX, 
    -TMIN) %>%
  select(malnutrition, NDVI_high_90, NDVI_low_10, pr_high_90, pr_low_10,
         tmax_high_90, tmin_low_10, consecutivas_veg_90, consecutivas_veg_10,
         consecutivas_prec_90, consecutivas_prec_10, consecutivas_tmax_90,
         consecutivas_tmax_10, consecutivas_tmin_90, consecutivas_tmin_10,
         grupos_veg_90, grupos_veg_10, grupos_prec_90, grupos_prec_10,
         grupos_tmax_90, grupos_tmax_10, grupos_tmin_90, grupos_tmin_10) %>%
  mutate(malnutrition = as.factor(malnutrition))


indices_prueba <- sample(nrow(malnutrition_15_naives), 
                                round(nrow(malnutrition_15_naives)), 
                                replace = FALSE)

test_set <- malnutrition_15_naives[indices_prueba, ]


# Crear el modelo Naive Bayes Multinomial
nv_model <- naive_bayes(malnutrition ~ ., data = training_set)


# Realizar predicciones en el conjunto de prueba PROBS
predictions <- predict(nv_model, test_set, type = 'prob')

predictions_yes <- ifelse(predictions[, "1"] >= 0.99, 1, 0)

# Matriz de confusión
table(predictions_yes, test_set$malnutrition)




# Realizar predicciones en el conjunto de prueba RESPONSE
predictions <- predict(nv_model, test_set)

# Matriz de confusión
table(predictions, test_set$malnutrition)