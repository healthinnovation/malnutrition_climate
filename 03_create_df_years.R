library(tidyverse)
library(haven)
library(fs)
library(readr)
library(dplyr)
library(zoo)
library(ROSE)

malnutrition_final <- read_csv("dataset/malnutrition_final.csv")

#malnutrition_balanced <- ovun.sample(malnutrition ~ ., 
                                     data = malnutrition_final, method = "over", 
                                     p = 0.2)
#malnutrition_balanced <- malnutrition_balanced[["data"]]

## 2014

malnutrition_14 <- malnutrition_final %>%
  filter(year==2014) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  mutate(
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(select(., c("mean.6_NDVI", "mean.7_NDVI"))),
    NDVI_first_months = rowMeans(select(., c("mean.0_NDVI", "mean.1_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    EVI_mean = apply(select(., contains("EVI")), 1, mean, na.rm = TRUE),
    EVI_sd = apply(select(., contains("EVI")), 1, sd, na.rm = TRUE),
    EVI_median = apply(select(., contains("EVI")), 1, median, na.rm = TRUE),
    EVI_IQR = apply(select(., contains("EVI")), 1, IQR, na.rm = TRUE),
    EVI_last_months = rowMeans(select(., c("mean.6_EVI", "mean.7_EVI"))),
    EVI_first_months = rowMeans(select(., c("mean.0_EVI", "mean.1_EVI"))),
    EVI_seasonal_diff = EVI_last_months - EVI_first_months,
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(select(., c("mean.201407_pr", "mean.201408_pr"))),
    pr_first_months = rowMeans(select(., c("mean.201401_pr", "mean.201402_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201401_tmmx"):
                        which(names(malnutrition_final)=="mean.201412_tmmx")])/10,
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201401_tmmn"):
                        which(names(malnutrition_final)=="mean.201412_tmmn")])/10)%>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, NDVI_seasonal_diff,
            mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, mean.10_NDVI, mean.11_NDVI,
            
            EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, EVI_first_months, EVI_seasonal_diff,
            mean.0_EVI, mean.1_EVI, mean.2_EVI,mean.3_EVI, mean.4_EVI, mean.5_EVI, mean.6_EVI, mean.7_EVI, mean.8_EVI,mean.9_EVI, mean.10_EVI, mean.11_EVI,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, pr_seasonal_diff,
            mean.jan_pr=mean.201401_pr,mean.feb_pr=mean.201402_pr,mean.mar_pr=mean.201403_pr,mean.apr_pr=mean.201404_pr,mean.may_pr=mean.201405_pr,mean.jun_pr=mean.201406_pr,
            mean.jul_pr=mean.201407_pr,mean.aug_pr=mean.201408_pr,mean.sep_pr=mean.201409_pr,mean.oct_pr=mean.201410_pr,mean.nov_pr=mean.201411_pr,mean.dec_pr=mean.201412_pr,
            
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_14 <- ovun.sample(malnutrition ~ ., data = malnutrition_14, method = "over", p = 0.2)
malnutrition_14 <- as.data.frame(malnutrition_14$data)

malnutrition_path <- paste("malnutrition_final14.csv", sep = "")

doc_path <- ("dataset")

write_csv(as.data.frame(malnutrition_14), path(doc_path,malnutrition_path))  


## 2015

malnutrition_15 <- malnutrition_final %>%
  filter(year==2015) %>%
  
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  mutate(
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(select(., c("mean.18_NDVI", "mean.19_NDVI"))),
    NDVI_first_months = rowMeans(select(., c("mean.12_NDVI", "mean.13_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    EVI_mean = apply(select(., contains("EVI")), 1, mean, na.rm = TRUE),
    EVI_sd = apply(select(., contains("EVI")), 1, sd, na.rm = TRUE),
    EVI_median = apply(select(., contains("EVI")), 1, median, na.rm = TRUE),
    EVI_IQR = apply(select(., contains("EVI")), 1, IQR, na.rm = TRUE),
    EVI_last_months = rowMeans(select(., c("mean.18_EVI", "mean.19_EVI"))),
    EVI_first_months = rowMeans(select(., c("mean.12_EVI", "mean.13_EVI"))),
    EVI_seasonal_diff = EVI_last_months - EVI_first_months,
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(select(., c("mean.201507_pr", "mean.201508_pr"))),
    pr_first_months = rowMeans(select(., c("mean.201501_pr", "mean.201502_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201501_tmmx"):
                        which(names(malnutrition_final)=="mean.201512_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201501_tmmn"):
                        which(names(malnutrition_final)=="mean.201512_tmmn")])/10)%>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
 
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, NDVI_seasonal_diff, 
            mean.0_NDVI = mean.12_NDVI, mean.1_NDVI = mean.13_NDVI, mean.2_NDVI = mean.14_NDVI,mean.3_NDVI = mean.15_NDVI, mean.4_NDVI =mean.16_NDVI, mean.5_NDVI =mean.17_NDVI, 
            mean.6_NDVI =mean.18_NDVI, mean.7_NDVI =mean.19_NDVI, mean.8_NDVI =mean.20_NDVI,mean.9_NDVI =mean.21_NDVI, mean.10_NDVI =mean.22_NDVI, mean.11_NDVI =mean.23_NDVI,
            
            EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, EVI_first_months, EVI_seasonal_diff, 
            mean.0_EVI =mean.12_EVI, mean.1_EVI =mean.13_EVI, mean.2_EVI =mean.14_EVI,mean.3_EVI =mean.15_EVI, mean.4_EVI =mean.16_EVI, mean.5_EVI =mean.17_EVI, mean.6_EVI =mean.18_EVI, 
            mean.7_EVI =mean.19_EVI, mean.8_EVI =mean.20_EVI,mean.9_EVI =mean.21_EVI, mean.10_EVI =mean.22_EVI, mean.11_EVI =mean.23_EVI,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, pr_seasonal_diff, 
            mean.jan_pr=mean.201501_pr,mean.feb_pr=mean.201502_pr,mean.mar_pr=mean.201503_pr,mean.apr_pr=mean.201504_pr,mean.may_pr=mean.201505_pr,mean.jun_pr=mean.201506_pr,
            mean.jul_pr=mean.201507_pr,mean.aug_pr=mean.201508_pr,mean.sep_pr=mean.201509_pr,mean.oct_pr=mean.201510_pr,mean.nov_pr=mean.201511_pr,mean.dec_pr=mean.201512_pr,
            
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_15 <- ovun.sample(malnutrition ~ ., data = malnutrition_15, method = "over", p = 0.2)
malnutrition_15 <- as.data.frame(malnutrition_15$data)

malnutrition_path <- paste("malnutrition_final15.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_15, path(doc_path,malnutrition_path))  


## 2016

malnutrition_16 <- malnutrition_final %>%
  filter(year==2016) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) 
  na.omit() %>%
  mutate(
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(select(., c("mean.30_NDVI", "mean.31_NDVI"))),
    NDVI_first_months = rowMeans(select(., c("mean.24_NDVI", "mean.25_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    EVI_mean = apply(select(., contains("EVI")), 1, mean, na.rm = TRUE),
    EVI_sd = apply(select(., contains("EVI")), 1, sd, na.rm = TRUE),
    EVI_median = apply(select(., contains("EVI")), 1, median, na.rm = TRUE),
    EVI_IQR = apply(select(., contains("EVI")), 1, IQR, na.rm = TRUE),
    EVI_last_months = rowMeans(select(., c("mean.30_EVI", "mean.31_EVI"))),
    EVI_first_months = rowMeans(select(., c("mean.24_EVI", "mean.25_EVI"))),
    EVI_seasonal_diff = EVI_last_months - EVI_first_months,
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(select(., c("mean.201607_pr", "mean.201608_pr"))),
    pr_first_months = rowMeans(select(., c("mean.201601_pr", "mean.201602_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201601_tmmx"):
                        which(names(malnutrition_final)=="mean.201612_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201601_tmmn"):
                        which(names(malnutrition_final)=="mean.201612_tmmn")])/10)%>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region,
    
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, NDVI_seasonal_diff, 
            mean.0_NDVI = mean.12_NDVI, mean.1_NDVI = mean.13_NDVI, mean.2_NDVI = mean.14_NDVI,mean.3_NDVI = mean.15_NDVI, mean.4_NDVI =mean.16_NDVI, mean.5_NDVI =mean.17_NDVI, 
            mean.6_NDVI =mean.18_NDVI, mean.7_NDVI =mean.19_NDVI, mean.8_NDVI =mean.20_NDVI,mean.9_NDVI =mean.21_NDVI, mean.10_NDVI =mean.22_NDVI, mean.11_NDVI =mean.23_NDVI,
            
            EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, EVI_first_months, EVI_seasonal_diff, 
            mean.0_EVI =mean.12_EVI, mean.1_EVI =mean.13_EVI, mean.2_EVI =mean.14_EVI,mean.3_EVI =mean.15_EVI, mean.4_EVI =mean.16_EVI, mean.5_EVI =mean.17_EVI, mean.6_EVI =mean.18_EVI, 
            mean.7_EVI =mean.19_EVI, mean.8_EVI =mean.20_EVI,mean.9_EVI =mean.21_EVI, mean.10_EVI =mean.22_EVI, mean.11_EVI =mean.23_EVI,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, pr_seasonal_diff, 
            mean.jan_pr=mean.201601_pr,mean.feb_pr=mean.201602_pr,mean.mar_pr=mean.201603_pr,mean.apr_pr=mean.201604_pr,mean.may_pr=mean.201605_pr,mean.jun_pr=mean.201606_pr,
            mean.jul_pr=mean.201607_pr,mean.aug_pr=mean.201608_pr,mean.sep_pr=mean.201609_pr,mean.oct_pr=mean.201610_pr,mean.nov_pr=mean.201611_pr,mean.dec_pr=mean.201612_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_16 <- ovun.sample(malnutrition ~ ., data = malnutrition_16, method = "over", p = 0.2)
malnutrition_16 <- as.data.frame(malnutrition_16$data)


malnutrition_path <- paste("malnutrition_final16.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_16, path(doc_path,malnutrition_path))  

## 2017

malnutrition_17 <- malnutrition_final %>%
  filter(year==2017) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) 
  na.omit() %>%
  mutate(
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(select(., c("mean.42_NDVI", "mean.43_NDVI"))),
    NDVI_first_months = rowMeans(select(., c("mean.36_NDVI", "mean.37_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    EVI_mean = apply(select(., contains("EVI")), 1, mean, na.rm = TRUE),
    EVI_sd = apply(select(., contains("EVI")), 1, sd, na.rm = TRUE),
    EVI_median = apply(select(., contains("EVI")), 1, median, na.rm = TRUE),
    EVI_IQR = apply(select(., contains("EVI")), 1, IQR, na.rm = TRUE),
    EVI_last_months = rowMeans(select(., c("mean.42_EVI", "mean.43_EVI"))),
    EVI_first_months = rowMeans(select(., c("mean.36_EVI", "mean.37_EVI"))),
    EVI_seasonal_diff = EVI_last_months - EVI_first_months,
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(select(., c("mean.201707_pr", "mean.201708_pr"))),
    pr_first_months = rowMeans(select(., c("mean.201701_pr", "mean.201702_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201701_tmmx"):
                        which(names(malnutrition_final)=="mean.201712_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201701_tmmn"):
                        which(names(malnutrition_final)=="mean.201712_tmmn")])/10) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, NDVI_seasonal_diff, 
            mean.0_NDVI = mean.12_NDVI, mean.1_NDVI = mean.13_NDVI, mean.2_NDVI = mean.14_NDVI,mean.3_NDVI = mean.15_NDVI, mean.4_NDVI =mean.16_NDVI, mean.5_NDVI =mean.17_NDVI, 
            mean.6_NDVI =mean.18_NDVI, mean.7_NDVI =mean.19_NDVI, mean.8_NDVI =mean.20_NDVI,mean.9_NDVI =mean.21_NDVI, mean.10_NDVI =mean.22_NDVI, mean.11_NDVI =mean.23_NDVI,
            
            EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, EVI_first_months, EVI_seasonal_diff, 
            mean.0_EVI =mean.12_EVI, mean.1_EVI =mean.13_EVI, mean.2_EVI =mean.14_EVI,mean.3_EVI =mean.15_EVI, mean.4_EVI =mean.16_EVI, mean.5_EVI =mean.17_EVI, mean.6_EVI =mean.18_EVI, 
            mean.7_EVI =mean.19_EVI, mean.8_EVI =mean.20_EVI,mean.9_EVI =mean.21_EVI, mean.10_EVI =mean.22_EVI, mean.11_EVI =mean.23_EVI,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, pr_seasonal_diff, 
            mean.jan_pr=mean.201701_pr,mean.feb_pr=mean.201702_pr,mean.mar_pr=mean.201703_pr,mean.apr_pr=mean.201704_pr,mean.may_pr=mean.201705_pr,mean.jun_pr=mean.201706_pr,
            mean.jul_pr=mean.201707_pr,mean.aug_pr=mean.201708_pr,mean.sep_pr=mean.201709_pr,mean.oct_pr=mean.201710_pr,mean.nov_pr=mean.201711_pr,mean.dec_pr=mean.201712_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_17 <- ovun.sample(malnutrition ~ ., data = malnutrition_17, method = "over", p = 0.2)
malnutrition_17 <- as.data.frame(malnutrition_17$data)

malnutrition_path <- paste("malnutrition_final17.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_17, path(doc_path,malnutrition_path))  

## 2018

malnutrition_18 <- malnutrition_final %>%
  filter(year==2018) %>%
  #mutate(across(.cols = everything(), .f = ~ifelse(is.na(.), mean(., na.rm=TRUE), .))) 
  na.omit()%>%
  mutate(
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(select(., c("mean.54_NDVI", "mean.55_NDVI"))),
    NDVI_first_months = rowMeans(select(., c("mean.48_NDVI", "mean.49_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    EVI_mean = apply(select(., contains("EVI")), 1, mean, na.rm = TRUE),
    EVI_sd = apply(select(., contains("EVI")), 1, sd, na.rm = TRUE),
    EVI_median = apply(select(., contains("EVI")), 1, median, na.rm = TRUE),
    EVI_IQR = apply(select(., contains("EVI")), 1, IQR, na.rm = TRUE),
    EVI_last_months = rowMeans(select(., c("mean.54_EVI", "mean.55_EVI"))),
    EVI_first_months = rowMeans(select(., c("mean.48_EVI", "mean.49_EVI"))),
    EVI_seasonal_diff = EVI_last_months - EVI_first_months,
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(select(., c("mean.201807_pr", "mean.201808_pr"))),
    pr_first_months = rowMeans(select(., c("mean.201801_pr", "mean.201802_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201801_tmmx"):
                        which(names(malnutrition_final)=="mean.201812_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201801_tmmn"):
                        which(names(malnutrition_final)=="mean.201812_tmmn")])/10) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, NDVI_seasonal_diff, 
            mean.0_NDVI = mean.12_NDVI, mean.1_NDVI = mean.13_NDVI, mean.2_NDVI = mean.14_NDVI,mean.3_NDVI = mean.15_NDVI, mean.4_NDVI =mean.16_NDVI, mean.5_NDVI =mean.17_NDVI, 
            mean.6_NDVI =mean.18_NDVI, mean.7_NDVI =mean.19_NDVI, mean.8_NDVI =mean.20_NDVI,mean.9_NDVI =mean.21_NDVI, mean.10_NDVI =mean.22_NDVI, mean.11_NDVI =mean.23_NDVI,
            
            EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, EVI_first_months, EVI_seasonal_diff, 
            mean.0_EVI =mean.12_EVI, mean.1_EVI =mean.13_EVI, mean.2_EVI =mean.14_EVI,mean.3_EVI =mean.15_EVI, mean.4_EVI =mean.16_EVI, mean.5_EVI =mean.17_EVI, mean.6_EVI =mean.18_EVI, 
            mean.7_EVI =mean.19_EVI, mean.8_EVI =mean.20_EVI,mean.9_EVI =mean.21_EVI, mean.10_EVI =mean.22_EVI, mean.11_EVI =mean.23_EVI,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, pr_seasonal_diff, 
            mean.jan_pr=mean.201801_pr,mean.feb_pr=mean.201802_pr,mean.mar_pr=mean.201803_pr,mean.apr_pr=mean.201804_pr,mean.may_pr=mean.201805_pr,mean.jun_pr=mean.201806_pr,
            mean.jul_pr=mean.201807_pr,mean.aug_pr=mean.201808_pr,mean.sep_pr=mean.201809_pr,mean.oct_pr=mean.201810_pr,mean.nov_pr=mean.201811_pr,mean.dec_pr=mean.201812_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_18 <- ovun.sample(malnutrition ~ ., data = malnutrition_18, method = "over", p = 0.2)
malnutrition_18 <- as.data.frame(malnutrition_18$data)

malnutrition_path <- paste("malnutrition_final18.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_18, path(doc_path,malnutrition_path))  

## 2019

malnutrition_19 <- malnutrition_final %>%
  filter(year==2019) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) 
  na.omit() %>%
  mutate(
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(select(., c("mean.66_NDVI", "mean.67_NDVI"))),
    NDVI_first_months = rowMeans(select(., c("mean.60_NDVI", "mean.61_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    EVI_mean = apply(select(., contains("EVI")), 1, mean, na.rm = TRUE),
    EVI_sd = apply(select(., contains("EVI")), 1, sd, na.rm = TRUE),
    EVI_median = apply(select(., contains("EVI")), 1, median, na.rm = TRUE),
    EVI_IQR = apply(select(., contains("EVI")), 1, IQR, na.rm = TRUE),
    EVI_last_months = rowMeans(select(., c("mean.66_EVI", "mean.67_EVI"))),
    EVI_first_months = rowMeans(select(., c("mean.60_EVI", "mean.61_EVI"))),
    EVI_seasonal_diff = EVI_last_months - EVI_first_months,
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(select(., c("mean.201907_pr", "mean.201908_pr"))),
    pr_first_months = rowMeans(select(., c("mean.201901_pr", "mean.201902_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201901_tmmx"):
                        which(names(malnutrition_final)=="mean.201912_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201901_tmmn"):
                        which(names(malnutrition_final)=="mean.201912_tmmn")])/10) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, NDVI_first_months, NDVI_seasonal_diff, 
            mean.0_NDVI = mean.12_NDVI, mean.1_NDVI = mean.13_NDVI, mean.2_NDVI = mean.14_NDVI,mean.3_NDVI = mean.15_NDVI, mean.4_NDVI =mean.16_NDVI, mean.5_NDVI =mean.17_NDVI, 
            mean.6_NDVI =mean.18_NDVI, mean.7_NDVI =mean.19_NDVI, mean.8_NDVI =mean.20_NDVI,mean.9_NDVI =mean.21_NDVI, mean.10_NDVI =mean.22_NDVI, mean.11_NDVI =mean.23_NDVI,
            
            EVI_mean, EVI_sd, EVI_median, EVI_IQR, EVI_last_months, EVI_first_months, EVI_seasonal_diff, 
            mean.0_EVI =mean.12_EVI, mean.1_EVI =mean.13_EVI, mean.2_EVI =mean.14_EVI,mean.3_EVI =mean.15_EVI, mean.4_EVI =mean.16_EVI, mean.5_EVI =mean.17_EVI, mean.6_EVI =mean.18_EVI, 
            mean.7_EVI =mean.19_EVI, mean.8_EVI =mean.20_EVI,mean.9_EVI =mean.21_EVI, mean.10_EVI =mean.22_EVI, mean.11_EVI =mean.23_EVI,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, pr_seasonal_diff, 
            mean.jan_pr=mean.201901_pr,mean.feb_pr=mean.201902_pr,mean.mar_pr=mean.201903_pr,mean.apr_pr=mean.201904_pr,mean.may_pr=mean.201905_pr,mean.jun_pr=mean.201906_pr,
            mean.jul_pr=mean.201907_pr,mean.aug_pr=mean.201908_pr,mean.sep_pr=mean.201909_pr,mean.oct_pr=mean.201910_pr,mean.nov_pr=mean.201911_pr,mean.dec_pr=mean.201912_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_19 <- ovun.sample(malnutrition ~ ., data = malnutrition_19, method = "over", p = 0.2)
malnutrition_19 <- as.data.frame(malnutrition_19$data)

malnutrition_path <- paste("malnutrition_final19.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_19, path(doc_path,malnutrition_path))  
