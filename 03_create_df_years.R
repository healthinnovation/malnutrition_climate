malnutrition_final <- read_csv("dataset/malnutrition_final.csv") 

## 2014

malnutrition_14 <- malnutrition_final %>%
  filter(year==2014) %>%
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.0_NDVI", "mean.1_NDVI", "mean.2_NDVI","mean.3_NDVI",
                                                        "mean.4_NDVI","mean.5_NDVI","mean.6_NDVI","mean.7_NDVI",
                                                        "mean.8_NDVI","mean.9_NDVI","mean.10_NDVI","mean.11_NDVI"))),
            list( ~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0))) %>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.0_EVI", "mean.1_EVI", "mean.2_EVI","mean.3_EVI",
                                                        "mean.4_EVI","mean.5_EVI","mean.6_EVI","mean.7_EVI",
                                                        "mean.8_EVI","mean.9_EVI","mean.10_EVI","mean.11_EVI"))),
            list(~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0)))%>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.201401_pr","mean.201402_pr","mean.201403_pr",
                                                        "mean.201404_pr","mean.201405_pr","mean.201406_pr",
                                                        "mean.201407_pr","mean.201408_pr","mean.201409_pr",
                                                        "mean.201410_pr","mean.201411_pr","mean.201412_pr"))),
            list(~ case_when(
              . <= 50 ~ 0,
              . < 50 & . <= 150 ~ 1,
              . > 150  ~ 2, 
              TRUE ~ 3))) %>%
  mutate(
    NDVI = rowMeans(.[,which(names(malnutrition_final)=="mean.0_NDVI"):
                        which(names(malnutrition_final)=="mean.11_NDVI")]),
    
    SAVI = rowMeans(.[,which(names(malnutrition_final)=="mean.0_EVI"):
                        which(names(malnutrition_final)=="mean.11_EVI")]),
    
    PP = rowSums(.[,which(names(malnutrition_final)=="mean.201401_pr"):
                     which(names(malnutrition_final)=="mean.201412_pr")]),
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201401_tmmx"):
                        which(names(malnutrition_final)=="mean.201412_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201401_tmmn"):
                        which(names(malnutrition_final)=="mean.201412_tmmn")])/10)%>%
  mutate(
    NDVI = case_when(
      NDVI >=0 & NDVI <=0.3 ~ 1, 
      NDVI > 0.3 ~ 2,
      TRUE ~ 0),
    
    SAVI = case_when(
      SAVI >=0 & SAVI <=0.3 ~ 1, 
      SAVI > 0.3 ~ 2,
      TRUE ~ 0), 
    
    PP = case_when(
      PP <= 500 ~ 0,
      PP < 500 & PP <= 1000 ~ 1,
      PP > 1000  ~ 2, 
      TRUE ~ 3)) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            weight, height, age, order, 
            NDVI, mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, mean.10_NDVI, mean.11_NDVI,
            SAVI, mean.0_EVI, mean.1_EVI, mean.2_EVI,mean.3_EVI, mean.4_EVI, mean.5_EVI, mean.6_EVI, mean.7_EVI, mean.8_EVI,mean.9_EVI, mean.10_EVI, mean.11_EVI,
            PP, mean.jan_pr=mean.201401_pr,mean.feb_pr=mean.201402_pr,mean.mar_pr=mean.201403_pr,mean.apr_pr=mean.201404_pr,mean.may_pr=mean.201405_pr,mean.jun_pr=mean.201406_pr,
            mean.jul_pr=mean.201407_pr,mean.aug_pr=mean.201408_pr,mean.sep_pr=mean.201409_pr,mean.oct_pr=mean.201410_pr,mean.nov_pr=mean.201411_pr,mean.dec_pr=mean.201412_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)


malnutrition_path <- paste("malnutrition_final14.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_14, path(doc_path,malnutrition_path))  


## 2015

malnutrition_15 <- malnutrition_final %>%
  filter(year==2015) %>%
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.12_NDVI", "mean.13_NDVI", "mean.14_NDVI","mean.15_NDVI",
                                                        "mean.16_NDVI","mean.17_NDVI","mean.18_NDVI","mean.19_NDVI",
                                                        "mean.20_NDVI","mean.21_NDVI","mean.22_NDVI","mean.23_NDVI"))),
            list( ~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0))) %>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.12_EVI", "mean.13_EVI", "mean.14_EVI","mean.15_EVI",
                                                        "mean.16_EVI","mean.17_EVI","mean.18_EVI","mean.19_EVI",
                                                        "mean.20_EVI","mean.21_EVI","mean.22_EVI","mean.23_EVI"))),
            list(~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0)))%>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.201501_pr","mean.201502_pr","mean.201503_pr",
                                                        "mean.201504_pr","mean.201505_pr","mean.201506_pr",
                                                        "mean.201507_pr","mean.201508_pr","mean.201509_pr",
                                                        "mean.201510_pr","mean.201511_pr","mean.201512_pr"))),
            list(~ case_when(
              . <= 50 ~ 0,
              . < 50 & . <= 150 ~ 1,
              . > 150  ~ 2, 
              TRUE ~ 3))) %>%
  mutate(
    NDVI = rowMeans(.[,which(names(malnutrition_final)=="mean.12_NDVI"):
                        which(names(malnutrition_final)=="mean.23_NDVI")]),
    
    SAVI = rowMeans(.[,which(names(malnutrition_final)=="mean.12_EVI"):
                        which(names(malnutrition_final)=="mean.23_EVI")]),
    
    PP = rowSums(.[,which(names(malnutrition_final)=="mean.201501_pr"):
                     which(names(malnutrition_final)=="mean.201512_pr")]),
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201501_tmmx"):
                        which(names(malnutrition_final)=="mean.201512_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201501_tmmn"):
                        which(names(malnutrition_final)=="mean.201512_tmmn")])/10)%>%
  mutate(
    NDVI = case_when(
      NDVI >=0 & NDVI <=0.3 ~ 1, 
      NDVI > 0.3 ~ 2,
      TRUE ~ 0),
    
    SAVI = case_when(
      SAVI >=0 & SAVI <=0.3 ~ 1, 
      SAVI > 0.3 ~ 2,
      TRUE ~ 0), 
    
    PP = case_when(
      PP <= 500 ~ 0,
      PP < 500 & PP <= 1000 ~ 1,
      PP > 1000  ~ 2, 
      TRUE ~ 3)) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            weight, height, age, order, 
            NDVI, mean.0_NDVI = mean.12_NDVI, mean.1_NDVI = mean.13_NDVI, mean.2_NDVI = mean.14_NDVI,mean.3_NDVI = mean.15_NDVI, mean.4_NDVI =mean.16_NDVI, mean.5_NDVI =mean.17_NDVI, 
            mean.6_NDVI =mean.18_NDVI, mean.7_NDVI =mean.19_NDVI, mean.8_NDVI =mean.20_NDVI,mean.9_NDVI =mean.21_NDVI, mean.10_NDVI =mean.22_NDVI, mean.11_NDVI =mean.23_NDVI,
            SAVI, mean.0_EVI =mean.12_EVI, mean.1_EVI =mean.13_EVI, mean.2_EVI =mean.14_EVI,mean.3_EVI =mean.15_EVI, mean.4_EVI =mean.16_EVI, mean.5_EVI =mean.17_EVI, mean.6_EVI =mean.18_EVI, 
            mean.7_EVI =mean.19_EVI, mean.8_EVI =mean.20_EVI,mean.9_EVI =mean.21_EVI, mean.10_EVI =mean.22_EVI, mean.11_EVI =mean.23_EVI,
            PP, mean.jan_pr=mean.201501_pr,mean.feb_pr=mean.201502_pr,mean.mar_pr=mean.201503_pr,mean.apr_pr=mean.201504_pr,mean.may_pr=mean.201505_pr,mean.jun_pr=mean.201506_pr,
            mean.jul_pr=mean.201507_pr,mean.aug_pr=mean.201508_pr,mean.sep_pr=mean.201509_pr,mean.oct_pr=mean.201510_pr,mean.nov_pr=mean.201511_pr,mean.dec_pr=mean.201512_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_path <- paste("malnutrition_final15.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_15, path(doc_path,malnutrition_path))  


## 2016

malnutrition_16 <- malnutrition_final %>%
  filter(year==2016) %>%
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.24_NDVI", "mean.25_NDVI", "mean.26_NDVI","mean.27_NDVI",
                                                        "mean.28_NDVI","mean.29_NDVI","mean.30_NDVI","mean.31_NDVI",
                                                        "mean.32_NDVI","mean.33_NDVI","mean.34_NDVI","mean.35_NDVI"))),
            list( ~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0))) %>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.24_EVI", "mean.25_EVI", "mean.26_EVI","mean.27_EVI",
                                                        "mean.28_EVI","mean.29_EVI","mean.30_EVI","mean.31_EVI",
                                                        "mean.32_EVI","mean.33_EVI","mean.34_EVI","mean.35_EVI"))),
            list(~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0)))%>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.201601_pr","mean.201602_pr","mean.201603_pr",
                                                        "mean.201604_pr","mean.201605_pr","mean.201606_pr",
                                                        "mean.201607_pr","mean.201608_pr","mean.201609_pr",
                                                        "mean.201610_pr","mean.201611_pr","mean.201612_pr"))),
            list(~ case_when(
              . <= 50 ~ 0,
              . < 50 & . <= 150 ~ 1,
              . > 150  ~ 2, 
              TRUE ~ 3))) %>%
  mutate(
    NDVI = rowMeans(.[,which(names(malnutrition_final)=="mean.24_NDVI"):
                        which(names(malnutrition_final)=="mean.35_NDVI")]),
    
    SAVI = rowMeans(.[,which(names(malnutrition_final)=="mean.24_EVI"):
                        which(names(malnutrition_final)=="mean.35_EVI")]),
    
    PP = rowSums(.[,which(names(malnutrition_final)=="mean.201601_pr"):
                     which(names(malnutrition_final)=="mean.201612_pr")]),
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201601_tmmx"):
                        which(names(malnutrition_final)=="mean.201612_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201601_tmmn"):
                        which(names(malnutrition_final)=="mean.201612_tmmn")])/10)%>%
  mutate(
    NDVI = case_when(
      NDVI >=0 & NDVI <=0.3 ~ 1, 
      NDVI > 0.3 ~ 2,
      TRUE ~ 0),
    
    SAVI = case_when(
      SAVI >=0 & SAVI <=0.3 ~ 1, 
      SAVI > 0.3 ~ 2,
      TRUE ~ 0), 
    
    PP = case_when(
      PP <= 500 ~ 0,
      PP < 500 & PP <= 1000 ~ 1,
      PP > 1000  ~ 2, 
      TRUE ~ 3)) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            weight, height, age, order, 
            NDVI, mean.0_NDVI=mean.24_NDVI, mean.1_NDVI=mean.25_NDVI, mean.2_NDVI=mean.26_NDVI,mean.3_NDVI=mean.27_NDVI, mean.4_NDVI=mean.28_NDVI, mean.5_NDVI=mean.29_NDVI, 
            mean.6_NDVI=mean.30_NDVI, mean.7_NDVI=mean.31_NDVI, mean.8_NDVI=mean.32_NDVI,mean.9_NDVI=mean.33_NDVI, mean.10_NDVI=mean.34_NDVI, mean.11_NDVI=mean.35_NDVI,
            SAVI, mean.0_EVI=mean.24_EVI, mean.1_EVI=mean.25_EVI, mean.2_EVI=mean.26_EVI,mean.3_EVI=mean.27_EVI, mean.4_EVI=mean.28_EVI, mean.5_EVI=mean.29_EVI, 
            mean.6_EVI=mean.30_EVI, mean.7_EVI=mean.31_EVI, mean.8_EVI=mean.32_EVI,mean.9_EVI=mean.33_EVI,mean.10_EVI= mean.34_EVI,mean.11_EVI= mean.35_EVI,
            PP, mean.jan_pr=mean.201601_pr,mean.feb_pr=mean.201602_pr,mean.mar_pr=mean.201603_pr,mean.apr_pr=mean.201604_pr,mean.may_pr=mean.201605_pr,mean.jun_pr=mean.201606_pr,
            mean.jul_pr=mean.201607_pr,mean.aug_pr=mean.201608_pr,mean.sep_pr=mean.201609_pr,mean.oct_pr=mean.201610_pr,mean.nov_pr=mean.201611_pr,mean.dec_pr=mean.201612_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)


malnutrition_path <- paste("malnutrition_final16.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_16, path(doc_path,malnutrition_path))  

## 2017

malnutrition_17 <- malnutrition_final %>%
  filter(year==2017) %>%
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.36_NDVI", "mean.37_NDVI", "mean.38_NDVI","mean.39_NDVI",
                                                        "mean.40_NDVI","mean.41_NDVI","mean.42_NDVI","mean.43_NDVI",
                                                        "mean.44_NDVI","mean.45_NDVI","mean.46_NDVI","mean.47_NDVI"))),
            list( ~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0))) %>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.36_EVI", "mean.37_EVI", "mean.38_EVI","mean.39_EVI",
                                                        "mean.40_EVI","mean.41_EVI","mean.42_EVI","mean.43_EVI",
                                                        "mean.44_EVI","mean.45_EVI","mean.46_EVI","mean.47_EVI"))),
            list(~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0)))%>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.201701_pr","mean.201702_pr","mean.201703_pr",
                                                        "mean.201704_pr","mean.201705_pr","mean.201706_pr",
                                                        "mean.201707_pr","mean.201708_pr","mean.201709_pr",
                                                        "mean.201710_pr","mean.201711_pr","mean.201712_pr"))),
            list(~ case_when(
              . <= 50 ~ 0,
              . < 50 & . <= 150 ~ 1,
              . > 150  ~ 2, 
              TRUE ~ 3))) %>%
  mutate(
    NDVI = rowMeans(.[,which(names(malnutrition_final)=="mean.36_NDVI"):
                        which(names(malnutrition_final)=="mean.47_NDVI")]),
    
    SAVI = rowMeans(.[,which(names(malnutrition_final)=="mean.36_EVI"):
                        which(names(malnutrition_final)=="mean.47_EVI")]),
    
    PP = rowSums(.[,which(names(malnutrition_final)=="mean.201701_pr"):
                     which(names(malnutrition_final)=="mean.201712_pr")]),
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201701_tmmx"):
                        which(names(malnutrition_final)=="mean.201712_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201701_tmmn"):
                        which(names(malnutrition_final)=="mean.201712_tmmn")])/10)%>%
  mutate(
    NDVI = case_when(
      NDVI >=0 & NDVI <=0.3 ~ 1, 
      NDVI > 0.3 ~ 2,
      TRUE ~ 0),
    
    SAVI = case_when(
      SAVI >=0 & SAVI <=0.3 ~ 1, 
      SAVI > 0.3 ~ 2,
      TRUE ~ 0), 
    
    PP = case_when(
      PP <= 500 ~ 0,
      PP < 500 & PP <= 1000 ~ 1,
      PP > 1000  ~ 2, 
      TRUE ~ 3)) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            weight, height, age, order, 
            NDVI, mean.0_NDVI=mean.36_NDVI, mean.1_NDVI=mean.37_NDVI,mean.2_NDVI= mean.38_NDVI,mean.3_NDVI=mean.39_NDVI,mean.4_NDVI= mean.40_NDVI, mean.5_NDVI=mean.41_NDVI, 
            mean.6_NDVI=mean.42_NDVI, mean.7_NDVI=mean.43_NDVI, mean.8_NDVI=mean.44_NDVI,mean.9_NDVI=mean.45_NDVI, mean.10_NDVI=mean.46_NDVI, mean.11_NDVI=mean.47_NDVI,
            SAVI, mean.0_EVI=mean.36_EVI, mean.1_EVI=mean.37_EVI, mean.2_EVI=mean.38_EVI,mean.3_EVI=mean.39_EVI, mean.4_EVI=mean.40_EVI, mean.5_EVI=mean.41_EVI, mean.6_EVI=mean.42_EVI, 
            mean.7_EVI=mean.43_EVI,mean.8_EVI= mean.44_EVI,mean.9_EVI=mean.45_EVI, mean.10_EVI=mean.46_EVI, mean.11_EVI=mean.47_EVI,
            PP, mean.jan_pr=mean.201701_pr,mean.feb_pr=mean.201702_pr,mean.mar_pr=mean.201703_pr,mean.apr_pr=mean.201704_pr,mean.may_pr=mean.201705_pr,mean.jun_pr=mean.201706_pr,
            mean.jul_pr=mean.201707_pr,mean.aug_pr=mean.201708_pr,mean.sep_pr=mean.201709_pr,mean.oct_pr=mean.201710_pr,mean.nov_pr=mean.201711_pr,mean.dec_pr=mean.201712_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)


malnutrition_path <- paste("malnutrition_final17.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_17, path(doc_path,malnutrition_path))  

## 2018

malnutrition_18 <- malnutrition_final %>%
  filter(year==2018) %>%
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.48_NDVI", "mean.49_NDVI", "mean.50_NDVI","mean.51_NDVI",
                                                        "mean.52_NDVI","mean.53_NDVI","mean.54_NDVI","mean.55_NDVI",
                                                        "mean.56_NDVI","mean.57_NDVI","mean.58_NDVI","mean.59_NDVI"))),
            list( ~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0))) %>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.48_EVI", "mean.49_EVI", "mean.50_EVI","mean.51_EVI",
                                                        "mean.52_EVI","mean.53_EVI","mean.54_EVI","mean.55_EVI",
                                                        "mean.56_EVI","mean.57_EVI","mean.58_EVI","mean.59_EVI"))),
            list(~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0)))%>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.201801_pr","mean.201802_pr","mean.201803_pr",
                                                        "mean.201804_pr","mean.201805_pr","mean.201806_pr",
                                                        "mean.201807_pr","mean.201808_pr","mean.201809_pr",
                                                        "mean.201810_pr","mean.201811_pr","mean.201812_pr"))),
            list(~ case_when(
              . <= 50 ~ 0,
              . < 50 & . <= 150 ~ 1,
              . > 150  ~ 2, 
              TRUE ~ 3))) %>%
  mutate(
    NDVI = rowMeans(.[,which(names(malnutrition_final)=="mean.48_NDVI"):
                        which(names(malnutrition_final)=="mean.59_NDVI")]),
    
    SAVI = rowMeans(.[,which(names(malnutrition_final)=="mean.48_EVI"):
                        which(names(malnutrition_final)=="mean.59_EVI")]),
    
    PP = rowSums(.[,which(names(malnutrition_final)=="mean.201801_pr"):
                     which(names(malnutrition_final)=="mean.201812_pr")]),
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201801_tmmx"):
                        which(names(malnutrition_final)=="mean.201812_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201801_tmmn"):
                        which(names(malnutrition_final)=="mean.201812_tmmn")])/10)%>%
  mutate(
    NDVI = case_when(
      NDVI >=0 & NDVI <=0.3 ~ 1, 
      NDVI > 0.3 ~ 2,
      TRUE ~ 0),
    
    SAVI = case_when(
      SAVI >=0 & SAVI <=0.3 ~ 1, 
      SAVI > 0.3 ~ 2,
      TRUE ~ 0), 
    
    PP = case_when(
      PP <= 500 ~ 0,
      PP < 500 & PP <= 1000 ~ 1,
      PP > 1000  ~ 2, 
      TRUE ~ 3)) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            weight, height, age, order, 
            NDVI, mean.0_NDVI=mean.48_NDVI, mean.1_NDVI=mean.49_NDVI,mean.2_NDVI=mean.50_NDVI,mean.3_NDVI=mean.51_NDVI,mean.4_NDVI= mean.52_NDVI,mean.5_NDVI= mean.53_NDVI, 
            mean.6_NDVI=mean.54_NDVI,mean.7_NDVI= mean.55_NDVI,mean.8_NDVI= mean.56_NDVI,mean.9_NDVI=mean.57_NDVI,mean.10_NDVI= mean.58_NDVI,mean.11_NDVI= mean.59_NDVI,
            SAVI, mean.0_EVI=mean.48_EVI,mean.1_EVI= mean.49_EVI,mean.2_EVI= mean.50_EVI,mean.3_EVI=mean.51_EVI,mean.4_EVI= mean.52_EVI,mean.5_EVI= mean.53_EVI, 
            mean.6_EVI=mean.54_EVI,mean.7_EVI= mean.55_EVI,mean.8_EVI= mean.56_EVI,mean.9_EVI=mean.57_EVI, mean.10_EVI=mean.58_EVI,mean.11_EVI= mean.59_EVI,
            PP, mean.jan_pr=mean.201801_pr,mean.feb_pr=mean.201802_pr,mean.mar_pr=mean.201803_pr,mean.apr_pr=mean.201804_pr,mean.may_pr=mean.201805_pr,mean.jun_pr=mean.201806_pr,
            mean.jul_pr=mean.201807_pr,mean.aug_pr=mean.201808_pr,mean.sep_pr=mean.201809_pr,mean.oct_pr=mean.201810_pr,mean.nov_pr=mean.201811_pr,mean.dec_pr=mean.201812_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)


malnutrition_path <- paste("malnutrition_final18.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_18, path(doc_path,malnutrition_path))  

## 2019

malnutrition_19 <- malnutrition_final %>%
  filter(year==2019) %>%
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.60_NDVI", "mean.61_NDVI", "mean.62_NDVI","mean.63_NDVI",
                                                        "mean.64_NDVI","mean.65_NDVI","mean.66_NDVI","mean.67_NDVI",
                                                        "mean.68_NDVI","mean.69_NDVI","mean.70_NDVI","mean.71_NDVI"))),
            list( ~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0))) %>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.60_EVI", "mean.61_EVI", "mean.62_EVI","mean.63_EVI",
                                                        "mean.64_EVI","mean.65_EVI","mean.66_EVI","mean.67_EVI",
                                                        "mean.68_EVI","mean.69_EVI","mean.70_EVI","mean.71_EVI"))),
            list(~ case_when(
              . >=0 & . <=0.3 ~ 1, 
              . > 0.3 ~ 2,
              TRUE ~ 0)))%>%
  
  mutate_at(vars(which(names(malnutrition_final) %in% c("mean.201901_pr","mean.201902_pr","mean.201903_pr",
                                                        "mean.201904_pr","mean.201905_pr","mean.201906_pr",
                                                        "mean.201907_pr","mean.201908_pr","mean.201909_pr",
                                                        "mean.201910_pr","mean.201911_pr","mean.201912_pr"))),
            list(~ case_when(
              . <= 50 ~ 0,
              . < 50 & . <= 150 ~ 1,
              . > 150  ~ 2, 
              TRUE ~ 3))) %>%
  mutate(
    NDVI = rowMeans(.[,which(names(malnutrition_final)=="mean.60_NDVI"):
                        which(names(malnutrition_final)=="mean.71_NDVI")]),
    
    SAVI = rowMeans(.[,which(names(malnutrition_final)=="mean.60_EVI"):
                        which(names(malnutrition_final)=="mean.71_EVI")]),
    
    PP = rowSums(.[,which(names(malnutrition_final)=="mean.201901_pr"):
                     which(names(malnutrition_final)=="mean.201912_pr")]),
    
    TMAX = rowMeans(.[,which(names(malnutrition_final)=="mean.201901_tmmx"):
                        which(names(malnutrition_final)=="mean.201912_tmmx")])/10,
    
    TMIN = rowMeans(.[,which(names(malnutrition_final)=="mean.201901_tmmn"):
                        which(names(malnutrition_final)=="mean.201912_tmmn")])/10)%>%
  mutate(
    NDVI = case_when(
      NDVI >=0 & NDVI <=0.3 ~ 1, 
      NDVI > 0.3 ~ 2,
      TRUE ~ 0),
    
    SAVI = case_when(
      SAVI >=0 & SAVI <=0.3 ~ 1, 
      SAVI > 0.3 ~ 2,
      TRUE ~ 0), 
    
    PP = case_when(
      PP <= 500 ~ 0,
      PP < 500 & PP <= 1000 ~ 1,
      PP > 1000  ~ 2, 
      TRUE ~ 3)) %>%
  
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, 
            weight, height, age, order, 
            NDVI, mean.0_NDVI=mean.60_NDVI, mean.1_NDVI=mean.61_NDVI,mean.2_NDVI= mean.62_NDVI,
            mean.3_NDVI=mean.63_NDVI, mean.4_NDVI=mean.64_NDVI, mean.5_NDVI=mean.65_NDVI, mean.6_NDVI=mean.66_NDVI, mean.7_NDVI=mean.67_NDVI, mean.8_NDVI=mean.68_NDVI,
            mean.9_NDVI=mean.69_NDVI, mean.10_NDVI=mean.70_NDVI, mean.11_NDVI=mean.71_NDVI,
            SAVI, mean.0_EVI=mean.60_EVI, mean.1_EVI=mean.61_EVI, mean.2_EVI=mean.62_EVI,
            mean.3_EVI=mean.63_EVI, mean.4_EVI=mean.64_EVI, mean.5_EVI=mean.65_EVI, mean.6_EVI=mean.66_EVI, mean.7_EVI=mean.67_EVI, mean.8_EVI=mean.68_EVI,
            mean.9_EVI=mean.69_EVI, mean.10_EVI=mean.70_EVI, mean.11_EVI=mean.71_EVI,
            PP, mean.jan_pr=mean.201901_pr,mean.feb_pr=mean.201902_pr,mean.mar_pr=mean.201903_pr,mean.apr_pr=mean.201904_pr,mean.may_pr=mean.201905_pr,mean.jun_pr=mean.201906_pr,
            mean.jul_pr=mean.201907_pr,mean.aug_pr=mean.201908_pr,mean.sep_pr=mean.201909_pr,mean.oct_pr=mean.201910_pr,mean.nov_pr=mean.201911_pr,mean.dec_pr=mean.201912_pr,
            TGAP = TMAX - TMIN, TMAX, TMIN)

malnutrition_path <- paste("malnutrition_final19.csv", sep = "")

doc_path <- ("dataset")

write_csv(malnutrition_19, path(doc_path,malnutrition_path))  
