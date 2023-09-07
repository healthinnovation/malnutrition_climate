library(tidyverse)
library(ggplot2)
library(GGally)  
library(haven)
library(magrittr)
library(fs)
library(readr)
library(dplyr)
library(dials)
library(corrplot)


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

# Llama a la funciÃ³n para cargar los datos de todos los aÃ±os
malnutrition_data <- read_and_mutate_malnutrition()

# 1.1. Crear los vectores con variables ........................................


malnutrition_14 <- malnutrition_data[["malnutrition_14"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2014, 
         TGAP = TMAX - TMIN) %>%
  group_by(malnutrition) %>%
  sample_n(size = 200, replace = TRUE) 

malnutrition_15 <- malnutrition_data[["malnutrition_15"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2015,
         TGAP = TMAX - TMIN) %>%
  group_by(malnutrition) %>%
  sample_n(size = 200, replace = TRUE) 

malnutrition_16 <- malnutrition_data[["malnutrition_16"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2016,
         TGAP = TMAX - TMIN) %>%
  group_by(malnutrition) %>%
  sample_n(size = 200, replace = TRUE) 

malnutrition_17 <- malnutrition_data[["malnutrition_17"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2017,
         TGAP = TMAX - TMIN) %>%
  group_by(malnutrition) %>%
  sample_n(size = 200, replace = TRUE) 

malnutrition_18 <- malnutrition_data[["malnutrition_18"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2018,
         TGAP = TMAX - TMIN) %>%
  group_by(malnutrition) %>%
  sample_n(size = 200, replace = TRUE) 

malnutrition_19 <- malnutrition_data[["malnutrition_19"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2019,
         TGAP = TMAX - TMIN) %>%
  group_by(malnutrition) %>%
  sample_n(size = 200, replace = TRUE) 

malnutrition_total <- rbind(malnutrition_14,malnutrition_15,
                            malnutrition_16,malnutrition_17,
                            malnutrition_18,malnutrition_19)

#2.  Crear histogramas para las variables numéricas continuas...................

# Gráfico para pr_mean
ggplot(malnutrition_total, aes(x = pr_mean, fill = malnutrition)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity") +
  facet_wrap(~year) + 
  labs(title ="Distribución de pr_mean a través de los años")

# Gráfico para pr_seasonal_diff
ggplot(malnutrition_total, aes(x = pr_seasonal_diff, fill = malnutrition)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity") +
  facet_wrap(~year) + 
  labs(title ="Distribución de pr_seasonal_diff a través de los años")


# Gráfico para NDVI_mean
ggplot(malnutrition_total, aes(x = NDVI_mean, fill = malnutrition)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity") +
  facet_wrap(~year) + 
  labs(title ="Distribución de NDVI_mean a través de los años")

# Gráfico para NDVI_seasonal_diff
ggplot(malnutrition_total, aes(x = NDVI_seasonal_diff, fill = malnutrition)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity") +
  facet_wrap(~year) + 
  labs(title ="Distribución de NDVI_seasonal_diff a través de los años")


# Gráfico para TGAP

ggplot(malnutrition_total, aes(x = TGAP, fill = malnutrition)) + 
  geom_histogram(colour = "black",
                 lwd = 0.75,
                 linetype = 1,
                 position = "identity") +
  facet_wrap(~year) + 
  labs(title ="Distribución de TGAP a través de los años")


# 3. Crear histograms para las variables numéricas  ..............................

# Gráfico de boxplot para consecutivas_veg_90
ggplot(malnutrition_total, aes(x = consecutivas_veg_90, fill = malnutrition)) +
  geom_bar(colour = "black",
           lwd = 0.25,
           linetype = 1,
           position = "jitter") +
  facet_wrap(~ year) +
  labs(title = "Conteo de consecutivas_veg_90 por año") +
  xlab("Consecutivas_veg_90") +
  ylab("Conteo")

# Gráfico de boxplot para consecutivas_prec_90
ggplot(malnutrition_total, aes(x = consecutivas_prec_90, fill = malnutrition)) +
  geom_bar(colour = "black",
           lwd = 0.25,
           linetype = 1,
           position = "jitter") +
  facet_wrap(~ year) +
  labs(title = "Conteo de consecutivas_prec_90 por año") +
  xlab("consecutivas_prec_90") +
  ylab("Conteo")

# Gráfico de boxplot para consecutivas_tmax_90
ggplot(malnutrition_total, aes(x = consecutivas_tmax_90, fill = malnutrition)) +
  geom_bar(colour = "black",
           lwd = 0.25,
           linetype = 1,
           position = "jitter") +
  facet_wrap(~ year) +
  labs(title = "Conteo de consecutivas_tmax_90 por año") +
  xlab("consecutivas_tmax_90") +
  ylab("Conteo")

# Gráfico de barras (histograma) para consecutivas_tmin_90
ggplot(malnutrition_total, aes(x = consecutivas_tmin_90, fill = malnutrition)) +
  geom_histogram(colour = "black", 
                 size = 0.25, 
                 linetype = 1,
                 position = "jitter", 
                 binwidth = 0.5) +
  facet_grid(malnutrition ~ year) +
  labs(title = "Conteo de consecutivas_tmin_90 por año") +
  xlab("consecutivas_tmin_90") +
  ylab("Conteo") +
  scale_y_sqrt()


# 4. Gráfico de boxplots para pr_mean, pr_seasonal_diff, NDVI_mean, NDVI_seasonal_diff y TGAP


malnutrition_14 <- malnutrition_data[["malnutrition_14"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2014, 
         TGAP = TMAX - TMIN) %>%
  sample_n(size = 2000, replace = TRUE) 

malnutrition_15 <- malnutrition_data[["malnutrition_15"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2015,
         TGAP = TMAX - TMIN) %>%
  sample_n(size = 2000, replace = TRUE) 

malnutrition_16 <- malnutrition_data[["malnutrition_16"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2016,
         TGAP = TMAX - TMIN) %>%
  sample_n(size = 2000, replace = TRUE) 

malnutrition_17 <- malnutrition_data[["malnutrition_17"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2017,
         TGAP = TMAX - TMIN) %>%
  sample_n(size = 2000, replace = TRUE) 

malnutrition_18 <- malnutrition_data[["malnutrition_18"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2018,
         TGAP = TMAX - TMIN) %>%
  sample_n(size = 2000, replace = TRUE) 

malnutrition_19 <- malnutrition_data[["malnutrition_19"]] %>%
  mutate(malnutrition = as.factor(malnutrition), year = 2019,
         TGAP = TMAX - TMIN) %>%
  sample_n(size = 2000, replace = TRUE) 

malnutrition_total <- rbind(malnutrition_14,malnutrition_15,
                            malnutrition_16,malnutrition_17,
                            malnutrition_18,malnutrition_19)


ggplot(malnutrition_total, aes(x = malnutrition, y = pr_mean, fill = malnutrition)) +
  geom_boxplot() +
  facet_wrap(~ year) +
  labs(title = "Distribución de pr_mean por malnutrition") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

ggplot(malnutrition_total, aes(x = malnutrition, y = pr_seasonal_diff, fill = malnutrition)) +
  geom_boxplot() +
  facet_wrap(~ year) +
  labs(title = "Distribución de pr_seasonal_diff por malnutrition") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

ggplot(malnutrition_total, aes(x = malnutrition, y = NDVI_mean, fill = malnutrition)) +
  geom_boxplot() +
  facet_wrap(~ year) +
  labs(title = "Distribución de NDVI_mean por malnutrition") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

ggplot(malnutrition_total, aes(x = malnutrition, y = NDVI_seasonal_diff, fill = malnutrition)) +
  geom_boxplot() +
  facet_wrap(~ year) +
  labs(title = "Distribución de NDVI_seasonal_diff por malnutrition") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

ggplot(malnutrition_total, aes(x = malnutrition, y = TGAP, fill = malnutrition)) +
  geom_boxplot() +
  facet_wrap(~ year) +
  labs(title = "Distribución de TGAP por malnutrition") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))
