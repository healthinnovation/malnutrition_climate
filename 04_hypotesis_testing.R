library(tidyverse)
library(haven)
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(infer)


# 1. Cargar los datos ..........................................................
path_2014 <- file.path("dataset/malnutrition_final14.csv")
malnutrition_14 <- read_csv(path_2014) %>%
  mutate(HHID = as.character(HHID), malnutrition = as.factor(malnutrition)) 


# 2. Stratified sampling .......................................................
malnutrition_14_test <- malnutrition_14 %>%
  group_by(malnutrition) %>% 
  slice_sample( prop = 0.1) %>%
  ungroup


# 2.1. Prueba de normalidad Shapiro Wilk
malnutrition_14_norm <- malnutrition_14 %>%
  group_by(malnutrition) %>% 
  slice_sample( prop = 0.6) %>%
  ungroup

shapiro.test(malnutrition_14_norm$NDVI_mean)
#### p-value < 2.2e-16


shapiro.test(malnutrition_14_norm$pr_mean)
#### p-value < 2.2e-16



# 3. T test ....................................................................

# 3.1. NDVI_mean

malnutrition_yes <- malnutrition_14 %>%
  filter(malnutrition == "1") %>%
  select(NDVI_mean)
  

malnutrition_no <- malnutrition_14 %>%
  filter(malnutrition == "0") %>%
  select(NDVI_mean)
  

t.test(x = malnutrition_yes,
       y = malnutrition_no,
       alternative = "greater",
       mu = 0)

#### p-value = 0.000119


# 3.2. pr_mean

malnutrition_yes <- malnutrition_14 %>%
  filter(malnutrition == "1") %>%
  select(pr_mean)


malnutrition_no <- malnutrition_14 %>%
  filter(malnutrition == "0") %>%
  select(pr_mean)


t.test(x = malnutrition_yes,
       y = malnutrition_no,
       alternative = "greater",
       mu = 0)

#### p-value = 0.1037


# 4. Wilcoxon-Mann-Whitney test ................................................

# 4.1. NDVI_mean

kruskal.test(
  malnutrition ~ NDVI_mean,
  data = malnutrition_14
)

#### p-value = 1.679e-07

# 4.2. pr_mean

kruskal.test(
  malnutrition ~ pr_mean,
  data = malnutrition_14
)

#### p-value = 9.76e-07


