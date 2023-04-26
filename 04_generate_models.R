library(tidyverse)
library(haven)
library(fs)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
# Cargar el paquete randomForest
library(randomForest)
library(reshape2)
library(caret)



path_2014 <- path("dataset/malnutrition_final14.csv")
malnutrition_14 <- read_csv(path_2014) %>%
  mutate(HHID = as.character(HHID)) # En este a?o el HHID era num?rico

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


############2014->2015################


# Cargar los datos
datos <- malnutrition_14 %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
              
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

# datos$malnutrition <- factor(datos$malnutrition, levels = c(0, 1))

set.seed(123)
indices_entrenamiento <- sample(nrow(datos), round(nrow(datos)), replace = FALSE)
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- malnutrition_15 %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

# datos_prueba$malnutrition <- factor(datos_prueba$malnutrition, levels = c(0, 1))


# Entrenar el modelo de Random Forest
modelo_rf <- randomForest(malnutrition ~ ., data = datos_entrenamiento, ntree = 100, importance = TRUE, do.trace = 10)

summary(modelo_rf)

indices_entrenamiento <- sample(nrow(datos_prueba), round(nrow(datos_prueba)*1), replace = FALSE)
datos_prueba <- datos_prueba[indices_entrenamiento, ]

# Ver los resultados de la predicci?n en los datos de prueba
prediccion <- predict(modelo_rf, newdata = datos_prueba, type="response") %>%
  as.numeric(as.character(.))
prediccion <- ifelse(prediccion > 0.2, 1, 0)


# Generar la matriz de confusi?n

true_values <- data.frame(datos_prueba$malnutrition)


confusion_matrix <- table(unlist(true_values), unlist(prediccion))

confusion_matrix <- matrix(c(confusion_matrix[1,]/sum(confusion_matrix[1,]), 
                             confusion_matrix[2,]/sum(confusion_matrix[2,])), nrow=2,
                           dimnames = list(c("Not Malnourished", "Malnourished"), c("Not Malnourished", "Malnourished")))


# Calcular la precisi?n del modelo
precision <- sum(prediccion == datos_prueba$malnutrition) / nrow(datos_prueba)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


# Plotear la matriz de confusi?n 
conf_mat_plot <- ggplot(data = melt(confusion_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", fill = "Accuracy") +
  theme_minimal() +
  geom_text(aes(label = value))
print(conf_mat_plot)

# Guardar el modelo 14->15

varImpPlot(modelo_rf)

saveRDS(modelo_rf, "modelo_pred15.rds")



####################2014,2015->2016##############################

# Cargar los datos

datos <- data.frame(rbind(malnutrition_14, malnutrition_15)) %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

# datos$malnutrition <- factor(datos$malnutrition, levels = c(0, 1))

set.seed(123)
indices_entrenamiento <- sample(nrow(datos), round(nrow(datos)), replace = FALSE)
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- malnutrition_16 %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

# datos_prueba$malnutrition <- factor(datos_prueba$malnutrition, levels = c(0, 1))

indices_entrenamiento <- sample(nrow(datos_prueba), round(nrow(datos_prueba)*0.1), replace = FALSE)
datos_prueba <- datos_prueba[indices_entrenamiento, ]

modelo_rf <- randomForest(malnutrition ~ ., data = datos_entrenamiento, ntree = 70, importance = TRUE, do.trace = 10)

prediccion <- predict(modelo_rf, newdata = datos_prueba, type="response") %>%
  as.numeric(as.character(.))
prediccion <- ifelse(prediccion > 0.1, 1, 0)

# Generar la matriz de confusi?n

true_values <- data.frame(datos_prueba$malnutrition)

confusion_matrix <- table(unlist(true_values), unlist(prediccion))

confusion_matrix <- matrix(c(confusion_matrix[1,]/sum(confusion_matrix[1,]), 
                             confusion_matrix[2,]/sum(confusion_matrix[2,])), nrow=2,
                           dimnames = list(c("Not Malnourished", "Malnourished"), c("Not Malnourished", "Malnourished")))


# Calcular la precisi?n del modelo
precision <- sum(prediccion == datos_prueba$malnutrition) / nrow(datos_prueba)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


# Plotear la matriz de confusi?n 
conf_mat_plot <- ggplot(data = melt(confusion_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", fill = "Accuracy") +
  theme_minimal() +
  geom_text(aes(label = value))
print(conf_mat_plot)

# Guardar el modelo 14,15->16

saveRDS(modelo_rf, "modelo_pred16.rds")




####################2014,2015,2016->2017##############################

# Cargar los datos


datos <- data.frame(rbind(malnutrition_14, malnutrition_15, malnutrition_16)) %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

#datos$malnutrition <- factor(datos$malnutrition, levels = c(0, 1))

set.seed(123)
indices_entrenamiento <- sample(nrow(datos), round(nrow(datos)), replace = FALSE)
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- malnutrition_17 %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

#datos_prueba$malnutrition <- factor(datos_prueba$malnutrition, levels = c(0, 1))

indices_entrenamiento <- sample(nrow(datos_prueba), round(nrow(datos_prueba)*1), replace = FALSE)
datos_prueba <- datos_prueba[indices_entrenamiento, ]

modelo_rf <- randomForest(malnutrition ~ ., data = datos_entrenamiento, ntree = 100, importance = TRUE, do.trace = 10)

prediccion <- predict(modelo_rf, newdata = datos_prueba, type="response") %>%
  as.numeric(as.character(.))
prediccion <- ifelse(prediccion > 0.05, 1, 0)

# Generar la matriz de confusi?n

true_values <- data.frame(datos_prueba$malnutrition) 

confusion_matrix <- table(unlist(true_values), unlist(prediccion))

confusion_matrix <- matrix(c(confusion_matrix[1,]/sum(confusion_matrix[1,]), 
                             confusion_matrix[2,]/sum(confusion_matrix[2,])), nrow=2,
                           dimnames = list(c("Not Malnourished", "Malnourished"), c("Not Malnourished", "Malnourished")))


# Calcular la precisi?n del modelo
precision <- sum(prediccion == datos_prueba$malnutrition) / nrow(datos_prueba)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


# Plotear la matriz de confusi?n 
conf_mat_plot <- ggplot(data = melt(confusion_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", fill = "Accuracy") +
  theme_minimal() +
  geom_text(aes(label = value))
print(conf_mat_plot)

# Guardar el modelo 14,15,16->17

saveRDS(modelo_rf, "modelo_pred17.rds")



####################2014,2015,2016,2017->2018##############################

# Cargar los datos

datos <- data.frame(rbind(malnutrition_14, malnutrition_15, malnutrition_16, malnutrition_17)) %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

#datos$malnutrition <- factor(datos$malnutrition, levels = c(0, 1))

set.seed(123)
indices_entrenamiento <- sample(nrow(datos), round(nrow(datos)), replace = FALSE)
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- malnutrition_18 %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

#datos_prueba$malnutrition <- factor(datos_prueba$malnutrition, levels = c(0, 1))

indices_entrenamiento <- sample(nrow(datos_prueba), round(nrow(datos_prueba)*1), replace = FALSE)
datos_prueba <- datos_prueba[indices_entrenamiento, ]

modelo_rf <- randomForest(malnutrition ~ ., data = datos_entrenamiento, ntree = 70, importance = TRUE, do.trace = 10)

prediccion <- predict(modelo_rf, newdata = datos_prueba, type="response") %>%
  as.numeric(as.character(.))
prediccion <- ifelse(prediccion > 0.065, 1, 0)

# Generar la matriz de confusi?n

true_values <- data.frame(datos_prueba$malnutrition)

confusion_matrix <- table(unlist(true_values), unlist(prediccion))

confusion_matrix <- matrix(c(confusion_matrix[1,]/sum(confusion_matrix[1,]), 
                             confusion_matrix[2,]/sum(confusion_matrix[2,])), nrow=2,
                           dimnames = list(c("Not Malnourished", "Malnourished"), c("Not Malnourished", "Malnourished")))


# Calcular la precisi?n del modelo
precision <- sum(prediccion == datos_prueba$malnutrition) / nrow(datos_prueba)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


# Plotear la matriz de confusi?n 
conf_mat_plot <- ggplot(data = melt(confusion_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", fill = "Accuracy") +
  theme_minimal() +
  geom_text(aes(label = value))
print(conf_mat_plot)

# Guardar el modelo 14,15,16,17->18

saveRDS(modelo_rf, "modelo_pred18.rds")



####################2014,2015,2016,2017,2018->2019##############################

# Cargar los datos

datos <- data.frame(rbind(malnutrition_14, malnutrition_15, malnutrition_16, malnutrition_17, malnutrition_18)) %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

#datos$malnutrition <- factor(datos$malnutrition, levels = c(0, 1))

set.seed(123)
indices_entrenamiento <- sample(nrow(datos), round(nrow(datos)), replace = FALSE)
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- malnutrition_19 %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate_at(vars(contains("NDVI"), contains("EVI"), contains("pr")), scale)

#datos_prueba$malnutrition <- factor(datos_prueba$malnutrition, levels = c(0, 1))

indices_entrenamiento <- sample(nrow(datos_prueba), round(nrow(datos_prueba)*1), replace = FALSE)
datos_prueba <- datos_prueba[indices_entrenamiento, ]

modelo_rf <- randomForest(malnutrition ~ ., data = datos_entrenamiento, ntree = 50, importance = TRUE, do.trace = 10)

prediccion <- predict(modelo_rf, newdata = datos_prueba, type="response") %>%
  as.numeric(as.character(.))
prediccion <- ifelse(prediccion > 0.12, 1, 0)

# Generar la matriz de confusi?n

true_values <- data.frame(datos_prueba$malnutrition)

confusion_matrix <- table(unlist(true_values), unlist(prediccion))

confusion_matrix <- matrix(c(confusion_matrix[1,]/sum(confusion_matrix[1,]), 
                             confusion_matrix[2,]/sum(confusion_matrix[2,])), nrow=2,
                           dimnames = list(c("Not Malnourished", "Malnourished"), c("Not Malnourished", "Malnourished")))


# Calcular la precisi?n del modelo
precision <- sum(prediccion == datos_prueba$malnutrition) / nrow(datos_prueba)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)


# Plotear la matriz de confusi?n 
conf_mat_plot <- ggplot(data = melt(confusion_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted Class", y = "Actual Class", fill = "Accuracy") +
  theme_minimal() +
  geom_text(aes(label = value))
print(conf_mat_plot)


# Guardar el modelo 14,15,16,17,18->19

saveRDS(modelo_rf, "modelo_pred19.rds")
