library(tidyverse)
library(haven)
library(fs)
library(readr)
library(dplyr)

path_cancer<- path("dataset/cancer data for MOOC 1.csv")
cancer <- read_csv(path_cancer)

fruit <- cancer$fruit
veg <- cancer$veg

fruitveg <- fruit+veg

table(fruitveg)

hist(cancer$fruitveg, xlab = "Portions of fruit and vegetables",
     
     main = "Daily consumption of fruit and vegetables combined", axes = F)

axis(side = 1, at = seq(0, 11, 1))

axis(side = 2, at = seq(0, 16, 2))

cancer$fit <- ifelse(cancer$bmi > 18.5 & cancer$bmi < 25, 1, 0)