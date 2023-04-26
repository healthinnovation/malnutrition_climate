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

### Running a hypothesis test

cancer_var <- cancer$cancer

overweight <- ifelse(cancer$bmi >= 25, 1, 0)

table(overweight)

overweight

chisq.test(x = overweight, y = cancer_var)


############################################################


path_cancer<- path("dataset/COPD_student_dataset.csv")
COPD <- read_csv(path_cancer)

hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)
hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1")   

#Summary for Walking time 

list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), 
     "Range" = range(COPD$MWT1Best, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE)) 

#Summary for lung capacity

list("Summary" = summary(COPD$FEV1), "Mean" = mean(COPD$FEV1, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$FEV1, na.rm=TRUE), 
     "Range" = range(COPD$FEV1, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$FEV1, na.rm=TRUE)) 

plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best") 


#Correlation in Pearons and Spearman

cor.test(COPD$FEV1, COPD$MWT1Best, use = "complete.obs", method = "pearson")

cor.test(COPD$FEV1, COPD$MWT1Best, use = "complete.obs", method = "spearman")


#Summary for age 


list("Summary" = summary(COPD$AGE), "Mean" = mean(COPD$AGE, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$AGE, na.rm=TRUE), 
     "Range" = range(COPD$AGE, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$AGE, na.rm=TRUE))

plot(COPD$AGE, COPD$MWT1Best, xlab = "AGE", ylab = "MWT1Best") 

#Correlation in Pearons and Spearman

cor.test(COPD$AGE, COPD$MWT1Best, use = "complete.obs", method = "pearson")

cor.test(COPD$AGE, COPD$MWT1Best, use = "complete.obs", method = "spearman")



# LM

MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)

summary(MWT1Best_FEV1) 
confint(MWT1Best_FEV1)

plot(MWT1Best_FEV1) 