library(tidyverse)
library(GGally)  # Para plot de correlación


path_2014 <- path("dataset/malnutrition_final14.csv")
malnutrition_14 <- read_csv(path_2014) %>%
  mutate(HHID = as.character(HHID))

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


datos <- data.frame(rbind(malnutrition_14, malnutrition_15, malnutrition_16, malnutrition_17, malnutrition_18)) %>%
  dplyr::select(malnutrition, longitudx, latitudy, 
                
                NDVI_mean, NDVI_sd, 
                NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, 
                NDVI_seasonal_diff,
                
                EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, 
                EVI_first_months, EVI_seasonal_diff,
                
                pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, 
                pr_first_months, pr_seasonal_diff) %>%
  mutate(malnutrition = factor(malnutrition))



# Resumen de estadísticos descriptivos
summary(datos)

# Gráficos de distribución y boxplot
datos %>%
  gather(key = "variable", value = "valor", -malnutrition) %>%
  ggplot(aes(x = valor, fill = malnutrition)) +
  geom_histogram(position = "dodge", alpha = 0.7, bins = 30) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribución de variables por estado de malnutrición", x = "Valor", y = "Frecuencia")

datos %>%
  gather(key = "variable", value = "valor", -malnutrition) %>%
  ggplot(aes(x = malnutrition, y = valor, fill = malnutrition)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Boxplot de variables por estado de malnutrición", x = "Malnutrición", y = "Valor")

# Matriz de correlación
ggcorr(datos %>% select(-latitudy, -longitudx), label = TRUE)