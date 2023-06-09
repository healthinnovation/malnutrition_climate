library(tidyverse)
library(ggplot2)
library(GGally)  # Para plot de correlación
library(haven)
library(magrittr)
library(fs)
library(readr)
library(dplyr)
library(dials)
library(corrplot)


source("load_data.R")
dataset <- load_data()

malnutrition_14 <- dataset$malnutrition_14 %>%
  mutate(year = 2014)
malnutrition_15 <- dataset$malnutrition_15 %>%
  mutate(year = 2015)
malnutrition_16 <- dataset$malnutrition_16 %>%
  mutate(year = 2016)
malnutrition_17 <- dataset$malnutrition_17 %>%
  mutate(year = 2017)
malnutrition_18 <- dataset$malnutrition_18 %>%
  mutate(year = 2018)
malnutrition_19 <- dataset$malnutrition_19 %>%
  mutate(year = 2019)


datos <- data.frame(malnutrition_14) %>%
  dplyr::select(area_residence, region, malnutrition,
                
                NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, 
                NDVI_first_months, NDVI_seasonal_diff,
                NDVI_high_count, NDVI_low_count, NDVI_range,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff,
                pr_high_count, pr_low_count, pr_range,
                
                tmax_high_count, tmax_low_count, tmax_range,
                
                tmin_high_count, tmin_low_count, tmin_range,
                
                TMAX, TMIN)

datos <- datos %>%
  mutate(year = factor(year))

# Resumen de estadísticos descriptivos
summary(datos)

############### CASES BY YEAR ################################

# Obtener el resumen de casos de desnutrición aguda por año
summary <- datos %>%
  filter(malnutrition == 1) %>%
  group_by(year) %>%
  summarise(total_cases = n())

# Mostrar el resumen
print(summary)

# Graficar los casos totales por año
ggplot(summary, aes(x = year, y = total_cases)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Año", y = "Casos Totales", title = "Casos de Desnutrición Aguda por Año")


################### BOX PLOT ################################

# Gráficos de distribución y boxplot
datos %>%
  gather(key = "variable", value = "valor", -malnutrition, -HHID) %>%
  ggplot(aes(x = valor, fill = malnutrition)) +
  geom_bar(position = "dodge", alpha = 0.7, stat = "count") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribución de variables por estado de malnutrición", x = "Valor", y = "Frecuencia")



library(ggplot2)

datos %>%
  gather(key = "variable", value = "valor", -malnutrition) %>%
  ggplot(aes(x = malnutrition, y = valor, fill = malnutrition)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Diagrama de Caja y Bigotes por Variable",
       x = "Malnutrición",
       y = "Valor") +
  theme(legend.position = "none")

# Matriz de correlación
ggcorr(datos %>% select(-area_residence), label = TRUE)