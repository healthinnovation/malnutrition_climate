library(tidyverse)
library(haven)
library(fs)
library(readr)
library(dplyr)
library(zoo)
library(ROSE)

malnutrition_final <- read_csv("dataset/malnutrition_final.csv") %>%
  distinct(HHID.x, order, .keep_all = TRUE)

#malnutrition_balanced <- ovun.sample(malnutrition ~ ., 
#                                     data = malnutrition_final, method = "over", 
#                                     p = 0.2)
#malnutrition_balanced <- malnutrition_balanced[["data"]]


## 2014


# 1. Generar un dataframe inicial ..............................................  
malnutrition_14 <- malnutrition_final %>%
  filter(year==2014) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, 
            mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, 
            mean.10_NDVI, mean.11_NDVI,
            
            mean.201401_pr,mean.201402_pr,mean.201403_pr,mean.201404_pr, 
            mean.201405_pr,mean.201406_pr, mean.201407_pr,mean.201408_pr,
            mean.201409_pr,mean.201410_pr,mean.201411_pr,mean.201412_pr,
            
            mean.201401_tmmx,mean.201402_tmmx,mean.201403_tmmx,mean.201404_tmmx,
            mean.201405_tmmx,mean.201406_tmmx, mean.201407_tmmx,mean.201408_tmmx,
            mean.201409_tmmx,mean.201410_tmmx,mean.201411_tmmx,mean.201412_tmmx,
            
            mean.201401_tmmn,mean.201402_tmmn,mean.201403_tmmn,mean.201404_tmmn,
            mean.201405_tmmn,mean.201406_tmmn, mean.201407_tmmn,mean.201408_tmmn,
            mean.201409_tmmn,mean.201410_tmmn,mean.201411_tmmn,mean.201412_tmmn)




# 2. Generar los umbrales climatológicos .......................................

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_14))
columns_tmmn <- grepl("tmmn", names(malnutrition_14))

# Dividir las columnas seleccionadas entre 10
malnutrition_14[, columns_tmmx] <- malnutrition_14[, columns_tmmx] / 10
malnutrition_14[, columns_tmmn] <- malnutrition_14[, columns_tmmn] / 10


# Crear dataframe de umbrales por región
umbrales_region <- data.frame(conglomerado = unique(malnutrition_14$conglomerado),
                              veg_low_10 = rep(0, 970),
                              veg_low_5 = rep(0, 970),
                              veg_low_1 = rep(0, 970),
                              veg_high_90 = rep(0, 970),
                              veg_high_95 = rep(0, 970),
                              veg_high_99 = rep(0, 970),
                              prec_low_10 = rep(0, 970),
                              prec_low_5 = rep(0, 970),
                              prec_low_1 = rep(0, 970),
                              prec_high_90 = rep(0, 970),
                              prec_high_95 = rep(0, 970),
                              prec_high_99 = rep(0, 970),
                              tmax_low_10 = rep(0, 970),
                              tmax_low_5 = rep(0, 970),
                              tmax_low_1 = rep(0, 970),
                              tmax_high_90 = rep(0, 970),
                              tmax_high_95 = rep(0, 970),
                              tmax_high_99 = rep(0, 970),
                              tmin_low_10 = rep(0, 970),
                              tmin_low_5 = rep(0, 970),
                              tmin_low_1 = rep(0, 970),
                              tmin_high_90 = rep(0, 970),
                              tmin_high_95 = rep(0, 970),
                              tmin_high_99 = rep(0, 970))

congs <- unique(malnutrition_14$conglomerado)

# Calcular los umbrales para cada región

for (i in 1:length(congs)) {
  data_filtered <- malnutrition_14 %>%
    filter(conglomerado == congs[i])
  
  # Vegetacion
  
  umbrales_region$veg_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 
                                            0.10, na.rm = TRUE)
  umbrales_region$veg_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 
                                             0.90, na.rm =TRUE)
  umbrales_region$veg_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 
                                           0.05, na.rm = TRUE)
  umbrales_region$veg_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 
                                             0.95, na.rm =TRUE)
  umbrales_region$veg_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 
                                           0.01, na.rm = TRUE)
  umbrales_region$veg_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 
                                             0.99, na.rm =TRUE)
  
  # Precipitacion
  
  
  umbrales_region$prec_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 
    0.10, na.rm =TRUE)
  umbrales_region$prec_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 
    0.90, na.rm =TRUE)
  umbrales_region$prec_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 
    0.05, na.rm =TRUE)
  umbrales_region$prec_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 
    0.95, na.rm =TRUE)
  umbrales_region$prec_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 
    0.01, na.rm =TRUE)
  umbrales_region$prec_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 
    0.99, na.rm =TRUE)
  
  
  # Temperatura maxima
  
  umbrales_region$tmax_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 
    0.10, na.rm =TRUE)
  umbrales_region$tmax_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 
    0.90, na.rm =TRUE)
  umbrales_region$tmax_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 
    0.05, na.rm =TRUE)
  umbrales_region$tmax_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 
    0.95, na.rm =TRUE)
  umbrales_region$tmax_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 
    0.01, na.rm =TRUE)
  umbrales_region$tmax_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 
    0.99, na.rm =TRUE)
  
  # Temperatura minima
  
  umbrales_region$tmin_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 
    0.10, na.rm =TRUE)
  umbrales_region$tmin_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 
    0.90, na.rm =TRUE)
  umbrales_region$tmin_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 
    0.05, na.rm =TRUE)
  umbrales_region$tmin_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 
    0.95, na.rm =TRUE)
  umbrales_region$tmin_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 
    0.01, na.rm =TRUE)
  umbrales_region$tmin_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 
    0.99, na.rm =TRUE)
}



# 3. Aplicar los umbrales en términos simples ..................................

# Realizar merge con el dataframe "malnutrition_14"
malnutrition_14_with_thresholds <- malnutrition_14 %>%
  left_join(umbrales_region, by = "conglomerado")


malnutrition_14_thresholded <- malnutrition_14_with_thresholds %>%
  
  mutate(
    
    #NDVI thresholds
    NDVI_high_90 = rowSums(sapply(select(., contains("NDVI")), 
                                  function(x) x > veg_high_90)),
    NDVI_low_10 = rowSums(sapply(select(., contains("NDVI")), 
                                 function(x) x < veg_low_10)),
    NDVI_high_95 = rowSums(sapply(select(., contains("NDVI")), 
                                  function(x) x > veg_high_95)),
    NDVI_low_5 = rowSums(sapply(select(., contains("NDVI")), 
                                function(x) x < veg_low_5)),
    NDVI_high_99 = rowSums(sapply(select(., contains("NDVI")), 
                                  function(x) x > veg_high_99)),
    NDVI_low_1 = rowSums(sapply(select(., contains("NDVI")), 
                                function(x) x < veg_low_1)),
    NDVI_range = apply(select(., contains("NDVI")), 1, 
                       function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    #NDVI metrics
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(
      select(., c("mean.6_NDVI", "mean.7_NDVI", "mean.8_NDVI"))),
    NDVI_first_months = rowMeans(
      select(., c("mean.0_NDVI", "mean.1_NDVI", "mean.2_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    
    #PR thresholds
    pr_high_90 = rowSums(sapply(select(., contains("pr")), 
                                function(x) x > prec_high_90)),
    pr_low_10 = rowSums(sapply(select(., contains("pr")), 
                               function(x) x < prec_low_10)),
    pr_high_95 = rowSums(sapply(select(., contains("pr")), 
                                function(x) x > prec_high_95)),
    pr_low_5 = rowSums(sapply(select(., contains("pr")), 
                              function(x) x < prec_low_5)),
    pr_high_99 = rowSums(sapply(select(., contains("pr")), 
                                function(x) x > prec_high_99)),
    pr_low_1 = rowSums(sapply(select(., contains("pr")), 
                              function(x) x < prec_low_1)),
    pr_range = apply(select(., contains("pr")), 1, 
                     function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    #PR metrics
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(
      select(., c("mean.201406_pr","mean.201407_pr", "mean.201408_pr"))),
    pr_first_months = rowMeans(
      select(., c("mean.201401_pr", "mean.201402_pr", "mean.201403_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    
    #Maximum temperature thresholds
    tmax_high_90 = rowSums(sapply(select(., contains("tmmx")), 
                                  function(x) x > tmax_high_90)),
    tmax_low_10 = rowSums(sapply(select(., contains("tmmx")), 
                                 function(x) x < tmax_low_10)),
    tmax_high_95 = rowSums(sapply(select(., contains("tmmx")), 
                                  function(x) x > tmax_high_95)),
    tmax_low_5 = rowSums(sapply(select(., contains("tmmx")), 
                                function(x) x < tmax_low_5)),
    tmax_high_99 = rowSums(sapply(select(., contains("tmmx")), 
                                  function(x) x > tmax_high_99)),
    tmax_low_1 = rowSums(sapply(select(., contains("tmmx")), 
                                function(x) x < tmax_low_1)),
    tmax_range = apply(select(., contains("tmmx")), 1, 
                       function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    TMAX = rowMeans(.[,which(names(malnutrition_14)=="mean.201401_tmmx"):
                        which(names(malnutrition_14)=="mean.201412_tmmx")]),
    
    
    #Minimum temperature metrics
    tmin_high_90 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_90)),
    tmin_low_10 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_10)),
    tmin_high_95 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_95)),
    tmin_low_5 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_5)),
    tmin_high_99 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_99)),
    tmin_low_1 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_1)),
    tmin_range = apply(
      select(., contains("tmmn")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    TMIN = rowMeans(.[,which(names(malnutrition_14)=="mean.201401_tmmn"):
                        which(names(malnutrition_14)=="mean.201412_tmmn")]))%>%
  
  
  #Seleccionamos las variables que uniremos posteriormente a los umbrales
  #contando los meses consecutivos y grupos de meses consecutivos
  transmute(HHID, area_residence, region, conglomerado, longitudx, latitudy,
            
            malnutrition,
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, 
            NDVI_first_months, NDVI_seasonal_diff, NDVI_high_90, NDVI_high_95, 
            NDVI_high_99, NDVI_low_10, NDVI_low_5, NDVI_low_1,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, 
            pr_seasonal_diff, pr_high_90, pr_high_95, pr_high_99, pr_low_10,
            pr_low_5, pr_low_1,
            
            tmax_high_90, tmax_high_95, tmax_high_99, 
            tmax_low_10, tmax_low_5, tmax_low_1,
            
            tmin_high_90, tmin_high_95, tmin_high_99, 
            tmin_low_10, tmin_low_5, tmin_low_1,
            
            TMAX, TMIN)


# 4. Transformación de umbrales para consecución temporal ......................

# Obtener las columnas que contienen "NDVI", "pr", "tmmx", "tmmn"
cols_with_NDVI <- grep("NDVI", names(malnutrition_14_with_thresholds), 
                       value = TRUE)
cols_with_pr <- grep("pr$", names(malnutrition_14_with_thresholds), 
                     value = TRUE)
cols_with_tmmx <- grep("tmmx$", names(malnutrition_14_with_thresholds), 
                       value = TRUE)
cols_with_tmmn <- grep("tmmn$", names(malnutrition_14_with_thresholds), 
                       value = TRUE)


# Crear nuevas columnas con 1 y 0 basadas en los umbrales
malnutrition_14_trans <- malnutrition_14_with_thresholds %>%
  mutate(
    across(cols_with_NDVI, ~ ifelse(. > veg_high_90, 1, 0),
           .names = "veg_tr_90_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_10, 1, 0),
           .names = "veg_tr_10_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_95, 1, 0),
           .names = "veg_tr_95_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_5, 1, 0),
           .names = "veg_tr_5_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_99, 1, 0),
           .names = "veg_tr_99_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_1, 1, 0),
           .names = "veg_tr_1_{.col}"),
    
    across(cols_with_pr, ~ ifelse(. > prec_high_90, 1, 0),
           .names = "prec_tr_90_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_10, 1, 0),
           .names = "prec_tr_10_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_95, 1, 0),
           .names = "prec_tr_95_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_5, 1, 0),
           .names = "prec_tr_5_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_99, 1, 0),
           .names = "prec_tr_99_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_1, 1, 0),
           .names = "prec_tr_1_{.col}"),
    
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_90, 1, 0),
           .names = "tmax_tr_90_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_10, 1, 0),
           .names = "tmax_tr_10_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_95, 1, 0),
           .names = "tmax_tr_95_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_5, 1, 0),
           .names = "tmax_tr_5_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_99, 1, 0),
           .names = "tmax_tr_99_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_1, 1, 0),
           .names = "tmax_tr_1_{.col}"),
    
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_90, 1, 0),
           .names = "tmin_tr_90_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_10, 1, 0),
           .names = "tmin_tr_10_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_95, 1, 0),
           .names = "tmin_tr_95_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_5, 1, 0),
           .names = "tmin_tr_5_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_99, 1, 0),
           .names = "tmin_tr_99_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_1, 1, 0),
           .names = "tmin_tr_1_{.col}")
    
  )



# 5. Funciones para conteo temporal con umbrales ...............................

# Definir umbrales
umbrales <- c("90", "95", "99", "1", "5", "10")

count_consecutive_ones <- function(x) {
  consecutivos <- rle(x)
  consecutivos$values[is.na(consecutivos$values)] <- 0
  if (all(x == 0)) {
    return(0)
  } else {
    max_consecutivos <- max(consecutivos$lengths[consecutivos$values == 1])
    return(max_consecutivos)
  }
}

# Función para contar grupos consecutivos de 1
count_consecutive_groups <- function(x) {
  consecutivos <- rle(x)
  grupos <- sum(consecutivos$lengths[consecutivos$values == 1] >= 2)
  return(grupos)
}


cols_veg_99 <- grep(paste0("veg_tr_", "99"), names(malnutrition_14_trans), value = TRUE)
cols_veg_95 <- grep(paste0("veg_tr_", "95"), names(malnutrition_14_trans), value = TRUE)
cols_veg_90 <- grep(paste0("veg_tr_", "90"), names(malnutrition_14_trans), value = TRUE)
cols_veg_1 <- grep("veg_tr_1_", names(malnutrition_14_trans), value = TRUE)
cols_veg_5 <- grep(paste0("veg_tr_", "5"), names(malnutrition_14_trans), value = TRUE)
cols_veg_10 <- grep(paste0("veg_tr_", "10"), names(malnutrition_14_trans), value = TRUE)

cols_prec_99 <- grep(paste0("prec_tr_", "99"), names(malnutrition_14_trans), value = TRUE)
cols_prec_95 <- grep(paste0("prec_tr_", "95"), names(malnutrition_14_trans), value = TRUE)
cols_prec_90 <- grep(paste0("prec_tr_", "90"), names(malnutrition_14_trans), value = TRUE)
cols_prec_1 <- grep("prec_tr_1_", names(malnutrition_14_trans), value = TRUE)
cols_prec_5 <- grep(paste0("prec_tr_", "5"), names(malnutrition_14_trans), value = TRUE)
cols_prec_10 <- grep(paste0("prec_tr_", "10"), names(malnutrition_14_trans), value = TRUE)

cols_tmax_99 <- grep(paste0("tmax_tr_", "99"), names(malnutrition_14_trans), value = TRUE)
cols_tmax_95 <- grep(paste0("tmax_tr_", "95"), names(malnutrition_14_trans), value = TRUE)
cols_tmax_90 <- grep(paste0("tmax_tr_", "90"), names(malnutrition_14_trans), value = TRUE)
cols_tmax_1 <- grep("tmax_tr_1_", names(malnutrition_14_trans), value = TRUE)
cols_tmax_5 <- grep(paste0("tmax_tr_", "5"), names(malnutrition_14_trans), value = TRUE)
cols_tmax_10 <- grep(paste0("tmax_tr_", "10"), names(malnutrition_14_trans), value = TRUE)

cols_tmin_99 <- grep(paste0("tmin_tr_", "99"), names(malnutrition_14_trans), value = TRUE)
cols_tmin_95 <- grep(paste0("tmin_tr_", "95"), names(malnutrition_14_trans), value = TRUE)
cols_tmin_90 <- grep(paste0("tmin_tr_", "90"), names(malnutrition_14_trans), value = TRUE)
cols_tmin_1 <- grep("tmin_tr_1_", names(malnutrition_14_trans), value = TRUE)
cols_tmin_5 <- grep(paste0("tmin_tr_", "5"), names(malnutrition_14_trans), value = TRUE)
cols_tmin_10 <- grep(paste0("tmin_tr_", "10"), names(malnutrition_14_trans), value = TRUE)

# Cummulative sum

malnutrition_14_new <- malnutrition_14_trans[, 80:length(names(malnutrition_14_trans))]

malnutrition_14_new <- malnutrition_14_new %>%
  mutate(
    consecutivas_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_ones),
    consecutivas_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_ones),
    consecutivas_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_ones),
    consecutivas_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_ones),
    consecutivas_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_ones),
    consecutivas_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_ones),
    
    consecutivas_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_ones),
    consecutivas_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_ones),
    consecutivas_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_ones),
    consecutivas_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_ones),
    consecutivas_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_ones),
    consecutivas_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_ones),
    
    consecutivas_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_ones),
    consecutivas_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_ones),
    consecutivas_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_ones),
    consecutivas_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_ones),
    consecutivas_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_ones),
    consecutivas_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_ones),
    
    consecutivas_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_ones),
    consecutivas_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_ones),
    consecutivas_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_ones),
    consecutivas_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_ones),
    consecutivas_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_ones),
    consecutivas_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_ones)
  )

# Groups

malnutrition_14_new <- malnutrition_14_new %>%
  mutate(
    grupos_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_groups),
    grupos_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_groups),
    grupos_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_groups),
    grupos_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_groups),
    grupos_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_groups),
    grupos_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_groups),
    
    grupos_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_groups),
    grupos_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_groups),
    grupos_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_groups),
    grupos_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_groups),
    grupos_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_groups),
    grupos_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_groups),
    
    grupos_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_groups),
    grupos_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_groups),
    grupos_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_groups),
    grupos_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_groups),
    grupos_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_groups),
    grupos_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_groups),
    
    grupos_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_groups),
    grupos_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_groups),
    grupos_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_groups),
    grupos_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_groups),
    grupos_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_groups),
    grupos_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_groups)
  )

malnutrition_14_final <- malnutrition_14_new[, 289:length(names(malnutrition_14_new))]

malnutrition_14 <- cbind(malnutrition_14_thresholded, malnutrition_14_final)

malnutrition_path <- paste("malnutrition_final14.csv", sep = "")

doc_path <- ("dataset")

write_csv(as.data.frame(malnutrition_14), path(doc_path,malnutrition_path))  


## 2015


# 1. Generar un dataframe inicial ..............................................
malnutrition_15 <- malnutrition_final %>%
  filter(year==2015) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, 
            area_residence, region, conglomerado,
            
            mean.12_NDVI, mean.13_NDVI, mean.14_NDVI,mean.15_NDVI, 
            mean.16_NDVI, mean.17_NDVI, 
            mean.18_NDVI, mean.19_NDVI, mean.20_NDVI,mean.21_NDVI, 
            mean.22_NDVI, mean.23_NDVI,
            
            mean.201501_pr,mean.201502_pr,mean.201503_pr,mean.201504_pr,
            mean.201505_pr,mean.201506_pr,
            mean.201507_pr,mean.201508_pr,mean.201509_pr,mean.201510_pr,
            mean.201511_pr,mean.201512_pr,
            
            mean.201501_tmmx,mean.201502_tmmx,mean.201503_tmmx,mean.201504_tmmx,
            mean.201505_tmmx,mean.201506_tmmx,
            mean.201507_tmmx,mean.201508_tmmx,mean.201509_tmmx,mean.201510_tmmx,
            mean.201511_tmmx,mean.201512_tmmx,
            
            mean.201501_tmmn,mean.201502_tmmn,mean.201503_tmmn,mean.201504_tmmn,
            mean.201505_tmmn,mean.201506_tmmn,
            mean.201507_tmmn,mean.201508_tmmn,mean.201509_tmmn,mean.201510_tmmn,
            mean.201511_tmmn,mean.201512_tmmn)


# 2. Generar los umbrales climatológicos .......................................

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_15))
columns_tmmn <- grepl("tmmn", names(malnutrition_15))

# Dividir las columnas seleccionadas entre 10
malnutrition_15[, columns_tmmx] <- malnutrition_15[, columns_tmmx] / 10
malnutrition_15[, columns_tmmn] <- malnutrition_15[, columns_tmmn] / 10


# Crear dataframe de umbrales por región
umbrales_region <- data.frame(conglomerado = unique(malnutrition_15$conglomerado),
                              veg_low_10 = rep(0, 2032),
                              veg_low_5 = rep(0, 2032),
                              veg_low_1 = rep(0, 2032),
                              veg_high_90 = rep(0, 2032),
                              veg_high_95 = rep(0, 2032),
                              veg_high_99 = rep(0, 2032),
                              prec_low_10 = rep(0, 2032),
                              prec_low_5 = rep(0, 2032),
                              prec_low_1 = rep(0, 2032),
                              prec_high_90 = rep(0, 2032),
                              prec_high_95 = rep(0, 2032),
                              prec_high_99 = rep(0, 2032),
                              tmax_low_10 = rep(0, 2032),
                              tmax_low_5 = rep(0, 2032),
                              tmax_low_1 = rep(0, 2032),
                              tmax_high_90 = rep(0, 2032),
                              tmax_high_95 = rep(0, 2032),
                              tmax_high_99 = rep(0, 2032),
                              tmin_low_10 = rep(0, 2032),
                              tmin_low_5 = rep(0, 2032),
                              tmin_low_1 = rep(0, 2032),
                              tmin_high_90 = rep(0, 2032),
                              tmin_high_95 = rep(0, 2032),
                              tmin_high_99 = rep(0, 2032))

congs <- unique(malnutrition_15$conglomerado)

# Calcular los umbrales para cada región
for (i in 1:length(congs)) {
  data_filtered <- malnutrition_15 %>%
    filter(conglomerado == congs[i])
  
  # Vegetacion
  
  umbrales_region$veg_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.10, 
    na.rm = TRUE)
  umbrales_region$veg_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$veg_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.05, 
    na.rm = TRUE)
  umbrales_region$veg_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$veg_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.01, 
    na.rm = TRUE)
  umbrales_region$veg_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Precipitacion
  
  
  umbrales_region$prec_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$prec_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$prec_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$prec_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$prec_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$prec_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  
  # Temperatura maxima
  
  umbrales_region$tmax_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmax_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmax_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmax_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmax_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmax_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Temperatura minima
  
  umbrales_region$tmin_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmin_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmin_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmin_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmin_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmin_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.99, 
    na.rm =TRUE)
}


# Realizar merge con el dataframe "malnutrition_14"
malnutrition_15_with_thresholds <- malnutrition_15 %>%
  left_join(umbrales_region, by = "conglomerado")

##################################################################################

malnutrition_15_full <- malnutrition_final %>%
  filter(year==2015) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, region, conglomerado,
            
            mean.12_NDVI, mean.13_NDVI, mean.14_NDVI,mean.15_NDVI, 
            mean.16_NDVI, mean.17_NDVI, 
            mean.18_NDVI, mean.19_NDVI, mean.20_NDVI,mean.21_NDVI, 
            mean.22_NDVI, mean.23_NDVI,
            
            mean.201501_pr,mean.201502_pr,mean.201503_pr,mean.201504_pr,
            mean.201505_pr,mean.201506_pr,
            mean.201507_pr,mean.201508_pr,mean.201509_pr,mean.201510_pr,
            mean.201511_pr,mean.201512_pr,
            
            mean.201501_tmmx,mean.201502_tmmx,mean.201503_tmmx,mean.201504_tmmx,
            mean.201505_tmmx,mean.201506_tmmx,
            mean.201507_tmmx,mean.201508_tmmx,mean.201509_tmmx,mean.201510_tmmx,
            mean.201511_tmmx,mean.201512_tmmx,
            
            mean.201501_tmmn,mean.201502_tmmn,mean.201503_tmmn,mean.201504_tmmn,
            mean.201505_tmmn,mean.201506_tmmn,
            mean.201507_tmmn,mean.201508_tmmn,mean.201509_tmmn,mean.201510_tmmn,
            mean.201511_tmmn,mean.201512_tmmn,
            
            
            
            mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, 
            mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, 
            mean.10_NDVI, mean.11_NDVI,
            
            mean.201401_pr,mean.201402_pr,mean.201403_pr,mean.201404_pr, 
            mean.201405_pr,mean.201406_pr, mean.201407_pr,mean.201408_pr,
            mean.201409_pr,mean.201410_pr,mean.201411_pr,mean.201412_pr,
            
            mean.201401_tmmx,mean.201402_tmmx,mean.201403_tmmx,mean.201404_tmmx,
            mean.201405_tmmx,mean.201406_tmmx, mean.201407_tmmx,mean.201408_tmmx,
            mean.201409_tmmx,mean.201410_tmmx,mean.201411_tmmx,mean.201412_tmmx,
            
            mean.201401_tmmn,mean.201402_tmmn,mean.201403_tmmn,mean.201404_tmmn,
            mean.201405_tmmn,mean.201406_tmmn, mean.201407_tmmn,mean.201408_tmmn,
            mean.201409_tmmn,mean.201410_tmmn,mean.201411_tmmn,mean.201412_tmmn
            )

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_15_full))
columns_tmmn <- grepl("tmmn", names(malnutrition_15_full))

# Dividir las columnas seleccionadas entre 10
malnutrition_15_full[, columns_tmmx] <- malnutrition_15_full[, columns_tmmx] / 10
malnutrition_15_full[, columns_tmmn] <- malnutrition_15_full[, columns_tmmn] / 10


# 3. Aplicar los umbrales en términos simples ..................................

malnutrition_15_full_with_thresholds <- malnutrition_15_full %>%
  left_join(umbrales_region, by = "conglomerado")

malnutrition_15_thresholded <- malnutrition_15_full_with_thresholds %>%
  
  mutate(
    
    #NDVI
    NDVI_high_90 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_90)),
    NDVI_low_10 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_10)),
    NDVI_high_95 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_95)),
    NDVI_low_5 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_5)),
    NDVI_high_99 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_99)),
    NDVI_low_1 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_1)),
    
    NDVI_range = apply(
      select(., contains("NDVI")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(
      select(., c("mean.17_NDVI", "mean.18_NDVI", "mean.19_NDVI"))),
    NDVI_first_months = rowMeans(
      select(., c("mean.12_NDVI", "mean.13_NDVI", "mean.14_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    
    #PR 
    pr_high_90 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_90)),
    pr_low_10 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_10)),
    pr_high_95 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_95)),
    pr_low_5 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_5)),
    pr_high_99 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_99)),
    pr_low_1 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_1)),
    
    pr_range = apply(
      select(., contains("pr")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(
      select(., c("mean.201506_pr","mean.201507_pr", "mean.201508_pr"))),
    pr_first_months = rowMeans(
      select(., c("mean.201501_pr", "mean.201502_pr", "mean.201503_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    
    #tmmx
    tmax_high_90 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_90)),
    tmax_low_10 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_10)),
    tmax_high_95 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_95)),
    tmax_low_5 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_5)),
    tmax_high_99 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_99)),
    tmax_low_1 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_1)),
    
    tmax_range = apply(
      select(., contains("tmmx")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMAX = rowMeans(.[,which(names(malnutrition_15_full)=="mean.201501_tmmx"):
                        which(names(malnutrition_15_full)=="mean.201512_tmmx")]),
    
    
    #tmmn
    tmin_high_90 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_90)),
    tmin_low_10 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_10)),
    tmin_high_95 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_95)),
    tmin_low_5 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_5)),
    tmin_high_99 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_99)),
    tmin_low_1 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_1)),
    
    tmin_range = apply(
      select(., contains("tmmn")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMIN = rowMeans(.[,which(names(malnutrition_15_full)=="mean.201501_tmmn"):
                        which(names(malnutrition_15_full)=="mean.201512_tmmn")]))%>%
  
  transmute(HHID, area_residence, region, conglomerado, longitudx, latitudy,
            
            malnutrition,
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, 
            NDVI_first_months, NDVI_seasonal_diff,
            NDVI_high_90, NDVI_high_95, NDVI_high_99, NDVI_low_10, NDVI_low_5, 
            NDVI_low_1,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, 
            pr_seasonal_diff,
            pr_high_90, pr_high_95, pr_high_99, pr_low_10,pr_low_5, pr_low_1,
            
            tmax_high_90, tmax_high_95, tmax_high_99, tmax_low_10, tmax_low_5, 
            tmax_low_1,
            
            tmin_high_90, tmin_high_95, tmin_high_99, tmin_low_10, tmin_low_5, 
            tmin_low_1,
            
            TMAX, TMIN)


# 4. Transformación de umbrales para consecución temporal ......................

### Transformación a 0 y 1

# Obtener las columnas que contienen "NDVI"
cols_with_NDVI <- grep("NDVI", names(malnutrition_15_with_thresholds), 
                       value = TRUE)
cols_with_pr <- grep("pr$", names(malnutrition_15_with_thresholds), 
                     value = TRUE)
cols_with_tmmx <- grep("tmmx$", names(malnutrition_15_with_thresholds), 
                       value = TRUE)
cols_with_tmmn <- grep("tmmn$", names(malnutrition_15_with_thresholds), 
                       value = TRUE)


# Crear nuevas columnas con 1 y 0 basadas en los umbrales
malnutrition_15_trans <- malnutrition_15_with_thresholds %>%
  mutate(
    across(cols_with_NDVI, ~ ifelse(. > veg_high_90, 1, 0),
           .names = "veg_tr_90_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_10, 1, 0),
           .names = "veg_tr_10_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_95, 1, 0),
           .names = "veg_tr_95_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_5, 1, 0),
           .names = "veg_tr_5_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_99, 1, 0),
           .names = "veg_tr_99_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_1, 1, 0),
           .names = "veg_tr_1_{.col}"),
    
    across(cols_with_pr, ~ ifelse(. > prec_high_90, 1, 0),
           .names = "prec_tr_90_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_10, 1, 0),
           .names = "prec_tr_10_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_95, 1, 0),
           .names = "prec_tr_95_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_5, 1, 0),
           .names = "prec_tr_5_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_99, 1, 0),
           .names = "prec_tr_99_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_1, 1, 0),
           .names = "prec_tr_1_{.col}"),
    
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_90, 1, 0),
           .names = "tmax_tr_90_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_10, 1, 0),
           .names = "tmax_tr_10_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_95, 1, 0),
           .names = "tmax_tr_95_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_5, 1, 0),
           .names = "tmax_tr_5_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_99, 1, 0),
           .names = "tmax_tr_99_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_1, 1, 0),
           .names = "tmax_tr_1_{.col}"),
    
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_90, 1, 0),
           .names = "tmin_tr_90_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_10, 1, 0),
           .names = "tmin_tr_10_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_95, 1, 0),
           .names = "tmin_tr_95_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_5, 1, 0),
           .names = "tmin_tr_5_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_99, 1, 0),
           .names = "tmin_tr_99_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_1, 1, 0),
           .names = "tmin_tr_1_{.col}")
    
  )



# 5. Funciones para conteo temporal con umbrales ...............................

# Definir umbrales
umbrales <- c("90", "95", "99", "1", "5", "10")

count_consecutive_ones <- function(x) {
  consecutivos <- rle(x)
  consecutivos$values[is.na(consecutivos$values)] <- 0
  if (all(x == 0)) {
    return(0)
  } else {
    max_consecutivos <- max(consecutivos$lengths[consecutivos$values == 1])
    return(max_consecutivos)
  }
}


# Función para contar grupos consecutivos de 1
count_consecutive_groups <- function(x) {
  consecutivos <- rle(x)
  grupos <- sum(consecutivos$lengths[consecutivos$values == 1] >= 2)
  grupos
}

cols_veg_99 <- grep(paste0("veg_tr_", "99"), names(malnutrition_15_trans), 
                    value = TRUE)
cols_veg_95 <- grep(paste0("veg_tr_", "95"), names(malnutrition_15_trans), 
                    value = TRUE)
cols_veg_90 <- grep(paste0("veg_tr_", "90"), names(malnutrition_15_trans), 
                    value = TRUE)
cols_veg_1 <- grep("veg_tr_1_", names(malnutrition_15_trans), value = TRUE)
cols_veg_5 <- grep(paste0("veg_tr_", "5"), names(malnutrition_15_trans), 
                   value = TRUE)
cols_veg_10 <- grep(paste0("veg_tr_", "10"), names(malnutrition_15_trans), 
                    value = TRUE)

cols_prec_99 <- grep(paste0("prec_tr_", "99"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_prec_95 <- grep(paste0("prec_tr_", "95"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_prec_90 <- grep(paste0("prec_tr_", "90"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_prec_1 <- grep("prec_tr_1_", names(malnutrition_15_trans), value = TRUE)
cols_prec_5 <- grep(paste0("prec_tr_", "5"), names(malnutrition_15_trans), 
                    value = TRUE)
cols_prec_10 <- grep(paste0("prec_tr_", "10"), names(malnutrition_15_trans), 
                     value = TRUE)

cols_tmax_99 <- grep(paste0("tmax_tr_", "99"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_tmax_95 <- grep(paste0("tmax_tr_", "95"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_tmax_90 <- grep(paste0("tmax_tr_", "90"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_tmax_1 <- grep("tmax_tr_1_", names(malnutrition_15_trans), value = TRUE)
cols_tmax_5 <- grep(paste0("tmax_tr_", "5"), names(malnutrition_15_trans), 
                    value = TRUE)
cols_tmax_10 <- grep(paste0("tmax_tr_", "10"), names(malnutrition_15_trans), 
                     value = TRUE)

cols_tmin_99 <- grep(paste0("tmin_tr_", "99"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_tmin_95 <- grep(paste0("tmin_tr_", "95"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_tmin_90 <- grep(paste0("tmin_tr_", "90"), names(malnutrition_15_trans), 
                     value = TRUE)
cols_tmin_1 <- grep("tmin_tr_1_", names(malnutrition_15_trans), value = TRUE)
cols_tmin_5 <- grep(paste0("tmin_tr_", "5"), names(malnutrition_15_trans), 
                    value = TRUE)
cols_tmin_10 <- grep(paste0("tmin_tr_", "10"), names(malnutrition_15_trans), 
                     value = TRUE)

# Cummulative sum

malnutrition_15_new <- malnutrition_15_trans[, 80:length(names(
  malnutrition_15_trans))]

malnutrition_15_new <- malnutrition_15_new %>%
  mutate(
    consecutivas_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_ones),
    consecutivas_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_ones),
    consecutivas_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_ones),
    consecutivas_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_ones),
    consecutivas_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_ones),
    consecutivas_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_ones),
    
    consecutivas_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_ones),
    consecutivas_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_ones),
    consecutivas_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_ones),
    consecutivas_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_ones),
    consecutivas_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_ones),
    consecutivas_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_ones),
    
    consecutivas_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_ones),
    consecutivas_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_ones),
    consecutivas_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_ones),
    consecutivas_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_ones),
    consecutivas_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_ones),
    consecutivas_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_ones),
    
    consecutivas_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_ones),
    consecutivas_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_ones),
    consecutivas_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_ones),
    consecutivas_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_ones),
    consecutivas_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_ones),
    consecutivas_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_ones)
  )

# Groups

malnutrition_15_new <- malnutrition_15_new %>%
  mutate(
    grupos_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_groups),
    grupos_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_groups),
    grupos_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_groups),
    grupos_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_groups),
    grupos_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_groups),
    grupos_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_groups),
    
    grupos_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_groups),
    grupos_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_groups),
    grupos_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_groups),
    grupos_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_groups),
    grupos_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_groups),
    grupos_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_groups),
    
    grupos_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_groups),
    grupos_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_groups),
    grupos_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_groups),
    grupos_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_groups),
    grupos_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_groups),
    grupos_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_groups),
    
    grupos_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_groups),
    grupos_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_groups),
    grupos_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_groups),
    grupos_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_groups),
    grupos_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_groups),
    grupos_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_groups)
  )

malnutrition_15_final <- malnutrition_15_new[, 289:length(
  names(malnutrition_15_new))]

malnutrition_15 <- cbind(malnutrition_15_thresholded, malnutrition_15_final)

malnutrition_path <- paste("malnutrition_final15.csv", sep = "")

doc_path <- ("dataset")

write_csv(as.data.frame(malnutrition_15), path(doc_path,malnutrition_path))  
























## 2016

malnutrition_16 <- malnutrition_final %>%
  filter(year==2016) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.24_NDVI, mean.25_NDVI, mean.26_NDVI,mean.27_NDVI, 
            mean.28_NDVI, mean.29_NDVI, 
            mean.30_NDVI, mean.31_NDVI, mean.32_NDVI,mean.33_NDVI, 
            mean.34_NDVI, mean.35_NDVI,
            
            mean.201601_pr,mean.201602_pr,mean.201603_pr,mean.201604_pr,
            mean.201605_pr,mean.201606_pr,
            mean.201607_pr,mean.201608_pr,mean.201609_pr,mean.201610_pr,
            mean.201611_pr,mean.201612_pr,
            
            mean.201601_tmmx,mean.201602_tmmx,mean.201603_tmmx,mean.201604_tmmx,
            mean.201605_tmmx,mean.201606_tmmx,
            mean.201607_tmmx,mean.201608_tmmx,mean.201609_tmmx,mean.201610_tmmx,
            mean.201611_tmmx,mean.201612_tmmx,
            
            mean.201601_tmmn,mean.201602_tmmn,mean.201603_tmmn,mean.201604_tmmn,
            mean.201605_tmmn,mean.201606_tmmn,
            mean.201607_tmmn,mean.201608_tmmn,mean.201609_tmmn,mean.201610_tmmn,
            mean.201611_tmmn,mean.201612_tmmn)


# 2. Generar los umbrales climatológicos .......................................

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_16))
columns_tmmn <- grepl("tmmn", names(malnutrition_16))

# Dividir las columnas seleccionadas entre 10
malnutrition_16[, columns_tmmx] <- malnutrition_16[, columns_tmmx] / 10
malnutrition_16[, columns_tmmn] <- malnutrition_16[, columns_tmmn] / 10


# Crear dataframe de umbrales por región
umbrales_region <- data.frame(conglomerado = unique(malnutrition_16$conglomerado),
                              veg_low_10 = rep(0, 2047),
                              veg_low_5 = rep(0, 2047),
                              veg_low_1 = rep(0, 2047),
                              veg_high_90 = rep(0, 2047),
                              veg_high_95 = rep(0, 2047),
                              veg_high_99 = rep(0, 2047),
                              prec_low_10 = rep(0, 2047),
                              prec_low_5 = rep(0, 2047),
                              prec_low_1 = rep(0, 2047),
                              prec_high_90 = rep(0, 2047),
                              prec_high_95 = rep(0, 2047),
                              prec_high_99 = rep(0, 2047),
                              tmax_low_10 = rep(0, 2047),
                              tmax_low_5 = rep(0, 2047),
                              tmax_low_1 = rep(0, 2047),
                              tmax_high_90 = rep(0, 2047),
                              tmax_high_95 = rep(0, 2047),
                              tmax_high_99 = rep(0, 2047),
                              tmin_low_10 = rep(0, 2047),
                              tmin_low_5 = rep(0, 2047),
                              tmin_low_1 = rep(0, 2047),
                              tmin_high_90 = rep(0, 2047),
                              tmin_high_95 = rep(0, 2047),
                              tmin_high_99 = rep(0, 2047))

congs <- unique(malnutrition_16$conglomerado)

# Calcular los umbrales para cada región
for (i in 1:length(congs)) {
  data_filtered <- malnutrition_16 %>%
    filter(conglomerado == congs[i])
  
  # Vegetacion
  
  umbrales_region$veg_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.10, 
    na.rm = TRUE)
  umbrales_region$veg_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$veg_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.05, 
    na.rm = TRUE)
  umbrales_region$veg_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$veg_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.01, 
    na.rm = TRUE)
  umbrales_region$veg_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Precipitacion
  
  
  umbrales_region$prec_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$prec_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$prec_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$prec_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$prec_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$prec_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  
  # Temperatura maxima
  
  umbrales_region$tmax_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmax_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmax_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmax_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmax_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmax_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Temperatura minima
  
  umbrales_region$tmin_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmin_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmin_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmin_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmin_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmin_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.99, 
    na.rm =TRUE)
}


# Realizar merge con el dataframe "malnutrition_16"
malnutrition_16_with_thresholds <- malnutrition_16 %>%
  left_join(umbrales_region, by = "conglomerado")

##################################################################################

malnutrition_16_full <- malnutrition_final %>%
  filter(year==2016) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.12_NDVI, mean.13_NDVI, mean.14_NDVI,mean.15_NDVI, 
            mean.16_NDVI, mean.17_NDVI, 
            mean.18_NDVI, mean.19_NDVI, mean.20_NDVI,mean.21_NDVI, 
            mean.22_NDVI, mean.23_NDVI,
            
            mean.201501_pr,mean.201502_pr,mean.201503_pr,mean.201504_pr,
            mean.201505_pr,mean.201506_pr,
            mean.201507_pr,mean.201508_pr,mean.201509_pr,mean.201510_pr,
            mean.201511_pr,mean.201512_pr,
            
            mean.201501_tmmx,mean.201502_tmmx,mean.201503_tmmx,mean.201504_tmmx,
            mean.201505_tmmx,mean.201506_tmmx,
            mean.201507_tmmx,mean.201508_tmmx,mean.201509_tmmx,mean.201510_tmmx,
            mean.201511_tmmx,mean.201512_tmmx,
            
            mean.201501_tmmn,mean.201502_tmmn,mean.201503_tmmn,mean.201504_tmmn,
            mean.201505_tmmn,mean.201506_tmmn,
            mean.201507_tmmn,mean.201508_tmmn,mean.201509_tmmn,mean.201510_tmmn,
            mean.201511_tmmn,mean.201512_tmmn,
            
            
            
            mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, 
            mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, 
            mean.10_NDVI, mean.11_NDVI,
            
            mean.201401_pr,mean.201402_pr,mean.201403_pr,mean.201404_pr, 
            mean.201405_pr,mean.201406_pr, mean.201407_pr,mean.201408_pr,
            mean.201409_pr,mean.201410_pr,mean.201411_pr,mean.201412_pr,
            
            mean.201401_tmmx,mean.201402_tmmx,mean.201403_tmmx,mean.201404_tmmx,
            mean.201405_tmmx,mean.201406_tmmx, mean.201407_tmmx,mean.201408_tmmx,
            mean.201409_tmmx,mean.201410_tmmx,mean.201411_tmmx,mean.201412_tmmx,
            
            mean.201401_tmmn,mean.201402_tmmn,mean.201403_tmmn,mean.201404_tmmn,
            mean.201405_tmmn,mean.201406_tmmn, mean.201407_tmmn,mean.201408_tmmn,
            mean.201409_tmmn,mean.201410_tmmn,mean.201411_tmmn,mean.201412_tmmn,
            
            
            
            mean.24_NDVI, mean.25_NDVI, mean.26_NDVI,mean.27_NDVI, 
            mean.28_NDVI, mean.29_NDVI, 
            mean.30_NDVI, mean.31_NDVI, mean.32_NDVI,mean.33_NDVI, 
            mean.34_NDVI, mean.35_NDVI,
            
            mean.201601_pr,mean.201602_pr,mean.201603_pr,mean.201604_pr,
            mean.201605_pr,mean.201606_pr,
            mean.201607_pr,mean.201608_pr,mean.201609_pr,mean.201610_pr,
            mean.201611_pr,mean.201612_pr,
            
            mean.201601_tmmx,mean.201602_tmmx,mean.201603_tmmx,mean.201604_tmmx,
            mean.201605_tmmx,mean.201606_tmmx,
            mean.201607_tmmx,mean.201608_tmmx,mean.201609_tmmx,mean.201610_tmmx,
            mean.201611_tmmx,mean.201612_tmmx,
            
            mean.201601_tmmn,mean.201602_tmmn,mean.201603_tmmn,mean.201604_tmmn,
            mean.201605_tmmn,mean.201606_tmmn,
            mean.201607_tmmn,mean.201608_tmmn,mean.201609_tmmn,mean.201610_tmmn,
            mean.201611_tmmn,mean.201612_tmmn
  )

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_16_full))
columns_tmmn <- grepl("tmmn", names(malnutrition_16_full))

# Dividir las columnas seleccionadas entre 10
malnutrition_16_full[, columns_tmmx] <- malnutrition_16_full[, columns_tmmx] / 10
malnutrition_16_full[, columns_tmmn] <- malnutrition_16_full[, columns_tmmn] / 10


# 3. Aplicar los umbrales en términos simples ..................................

malnutrition_16_full_with_thresholds <- malnutrition_16_full %>%
  left_join(umbrales_region, by = "conglomerado")

malnutrition_16_thresholded <- malnutrition_16_full_with_thresholds %>%
  
  mutate(
    
    #NDVI
    NDVI_high_90 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_90)),
    NDVI_low_10 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_10)),
    NDVI_high_95 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_95)),
    NDVI_low_5 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_5)),
    NDVI_high_99 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_99)),
    NDVI_low_1 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_1)),
    
    NDVI_range = apply(
      select(., contains("NDVI")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(
      select(., c("mean.29_NDVI", "mean.30_NDVI", "mean.31_NDVI"))),
    NDVI_first_months = rowMeans(
      select(., c("mean.24_NDVI", "mean.25_NDVI", "mean.26_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    
    #PR 
    pr_high_90 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_90)),
    pr_low_10 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_10)),
    pr_high_95 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_95)),
    pr_low_5 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_5)),
    pr_high_99 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_99)),
    pr_low_1 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_1)),
    
    pr_range = apply(
      select(., contains("pr")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(
      select(., c("mean.201606_pr","mean.201607_pr", "mean.201608_pr"))),
    pr_first_months = rowMeans(
      select(., c("mean.201601_pr", "mean.201602_pr", "mean.201603_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    
    #tmmx
    tmax_high_90 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_90)),
    tmax_low_10 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_10)),
    tmax_high_95 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_95)),
    tmax_low_5 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_5)),
    tmax_high_99 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_99)),
    tmax_low_1 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_1)),
    
    tmax_range = apply(
      select(., contains("tmmx")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMAX = rowMeans(.[,which(names(malnutrition_16_full)=="mean.201601_tmmx"):
                        which(names(malnutrition_16_full)=="mean.201612_tmmx")]),
    
    
    #tmmn
    tmin_high_90 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_90)),
    tmin_low_10 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_10)),
    tmin_high_95 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_95)),
    tmin_low_5 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_5)),
    tmin_high_99 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_99)),
    tmin_low_1 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_1)),
    
    tmin_range = apply(
      select(., contains("tmmn")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMIN = rowMeans(.[,which(names(malnutrition_16_full)=="mean.201601_tmmn"):
                        which(names(malnutrition_16_full)=="mean.201612_tmmn")]))%>%
  
  transmute(HHID, area_residence, region, conglomerado, longitudx, latitudy,
            
            malnutrition,
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, 
            NDVI_first_months, NDVI_seasonal_diff,
            NDVI_high_90, NDVI_high_95, NDVI_high_99, NDVI_low_10, NDVI_low_5, 
            NDVI_low_1,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, 
            pr_seasonal_diff,
            pr_high_90, pr_high_95, pr_high_99, pr_low_10,pr_low_5, pr_low_1,
            
            tmax_high_90, tmax_high_95, tmax_high_99, tmax_low_10, tmax_low_5, 
            tmax_low_1,
            
            tmin_high_90, tmin_high_95, tmin_high_99, tmin_low_10, tmin_low_5, 
            tmin_low_1,
            
            TMAX, TMIN)


# 4. Transformación de umbrales para consecución temporal ......................

### Transformación a 0 y 1

# Obtener las columnas que contienen "NDVI"
cols_with_NDVI <- grep("NDVI", names(malnutrition_16_with_thresholds), 
                       value = TRUE)
cols_with_pr <- grep("pr$", names(malnutrition_16_with_thresholds), 
                     value = TRUE)
cols_with_tmmx <- grep("tmmx$", names(malnutrition_16_with_thresholds), 
                       value = TRUE)
cols_with_tmmn <- grep("tmmn$", names(malnutrition_16_with_thresholds), 
                       value = TRUE)


# Crear nuevas columnas con 1 y 0 basadas en los umbrales
malnutrition_16_trans <- malnutrition_16_with_thresholds %>%
  mutate(
    across(cols_with_NDVI, ~ ifelse(. > veg_high_90, 1, 0),
           .names = "veg_tr_90_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_10, 1, 0),
           .names = "veg_tr_10_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_95, 1, 0),
           .names = "veg_tr_95_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_5, 1, 0),
           .names = "veg_tr_5_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_99, 1, 0),
           .names = "veg_tr_99_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_1, 1, 0),
           .names = "veg_tr_1_{.col}"),
    
    across(cols_with_pr, ~ ifelse(. > prec_high_90, 1, 0),
           .names = "prec_tr_90_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_10, 1, 0),
           .names = "prec_tr_10_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_95, 1, 0),
           .names = "prec_tr_95_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_5, 1, 0),
           .names = "prec_tr_5_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_99, 1, 0),
           .names = "prec_tr_99_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_1, 1, 0),
           .names = "prec_tr_1_{.col}"),
    
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_90, 1, 0),
           .names = "tmax_tr_90_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_10, 1, 0),
           .names = "tmax_tr_10_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_95, 1, 0),
           .names = "tmax_tr_95_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_5, 1, 0),
           .names = "tmax_tr_5_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_99, 1, 0),
           .names = "tmax_tr_99_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_1, 1, 0),
           .names = "tmax_tr_1_{.col}"),
    
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_90, 1, 0),
           .names = "tmin_tr_90_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_10, 1, 0),
           .names = "tmin_tr_10_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_95, 1, 0),
           .names = "tmin_tr_95_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_5, 1, 0),
           .names = "tmin_tr_5_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_99, 1, 0),
           .names = "tmin_tr_99_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_1, 1, 0),
           .names = "tmin_tr_1_{.col}")
    
  )



# 5. Funciones para conteo temporal con umbrales ...............................

# Definir umbrales
umbrales <- c("90", "95", "99", "1", "5", "10")

count_consecutive_ones <- function(x) {
  consecutivos <- rle(x)
  consecutivos$values[is.na(consecutivos$values)] <- 0
  if (all(x == 0)) {
    return(0)
  } else {
    max_consecutivos <- max(consecutivos$lengths[consecutivos$values == 1])
    return(max_consecutivos)
  }
}


# Función para contar grupos consecutivos de 1
count_consecutive_groups <- function(x) {
  consecutivos <- rle(x)
  grupos <- sum(consecutivos$lengths[consecutivos$values == 1] >= 2)
  grupos
}

cols_veg_99 <- grep(paste0("veg_tr_", "99"), names(malnutrition_16_trans), 
                    value = TRUE)
cols_veg_95 <- grep(paste0("veg_tr_", "95"), names(malnutrition_16_trans), 
                    value = TRUE)
cols_veg_90 <- grep(paste0("veg_tr_", "90"), names(malnutrition_16_trans), 
                    value = TRUE)
cols_veg_1 <- grep("veg_tr_1_", names(malnutrition_16_trans), value = TRUE)
cols_veg_5 <- grep(paste0("veg_tr_", "5"), names(malnutrition_16_trans), 
                   value = TRUE)
cols_veg_10 <- grep(paste0("veg_tr_", "10"), names(malnutrition_16_trans), 
                    value = TRUE)

cols_prec_99 <- grep(paste0("prec_tr_", "99"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_prec_95 <- grep(paste0("prec_tr_", "95"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_prec_90 <- grep(paste0("prec_tr_", "90"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_prec_1 <- grep("prec_tr_1_", names(malnutrition_16_trans), value = TRUE)
cols_prec_5 <- grep(paste0("prec_tr_", "5"), names(malnutrition_16_trans), 
                    value = TRUE)
cols_prec_10 <- grep(paste0("prec_tr_", "10"), names(malnutrition_16_trans), 
                     value = TRUE)

cols_tmax_99 <- grep(paste0("tmax_tr_", "99"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_tmax_95 <- grep(paste0("tmax_tr_", "95"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_tmax_90 <- grep(paste0("tmax_tr_", "90"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_tmax_1 <- grep("tmax_tr_1_", names(malnutrition_16_trans), value = TRUE)
cols_tmax_5 <- grep(paste0("tmax_tr_", "5"), names(malnutrition_16_trans), 
                    value = TRUE)
cols_tmax_10 <- grep(paste0("tmax_tr_", "10"), names(malnutrition_16_trans), 
                     value = TRUE)

cols_tmin_99 <- grep(paste0("tmin_tr_", "99"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_tmin_95 <- grep(paste0("tmin_tr_", "95"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_tmin_90 <- grep(paste0("tmin_tr_", "90"), names(malnutrition_16_trans), 
                     value = TRUE)
cols_tmin_1 <- grep("tmin_tr_1_", names(malnutrition_16_trans), value = TRUE)
cols_tmin_5 <- grep(paste0("tmin_tr_", "5"), names(malnutrition_16_trans), 
                    value = TRUE)
cols_tmin_10 <- grep(paste0("tmin_tr_", "10"), names(malnutrition_16_trans), 
                     value = TRUE)

# Cummulative sum

malnutrition_16_new <- malnutrition_16_trans[, 80:length(names(
  malnutrition_16_trans))]

malnutrition_16_new <- malnutrition_16_new %>%
  mutate(
    consecutivas_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_ones),
    consecutivas_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_ones),
    consecutivas_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_ones),
    consecutivas_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_ones),
    consecutivas_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_ones),
    consecutivas_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_ones),
    
    consecutivas_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_ones),
    consecutivas_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_ones),
    consecutivas_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_ones),
    consecutivas_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_ones),
    consecutivas_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_ones),
    consecutivas_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_ones),
    
    consecutivas_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_ones),
    consecutivas_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_ones),
    consecutivas_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_ones),
    consecutivas_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_ones),
    consecutivas_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_ones),
    consecutivas_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_ones),
    
    consecutivas_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_ones),
    consecutivas_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_ones),
    consecutivas_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_ones),
    consecutivas_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_ones),
    consecutivas_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_ones),
    consecutivas_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_ones)
  )

# Groups

malnutrition_16_new <- malnutrition_16_new %>%
  mutate(
    grupos_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_groups),
    grupos_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_groups),
    grupos_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_groups),
    grupos_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_groups),
    grupos_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_groups),
    grupos_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_groups),
    
    grupos_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_groups),
    grupos_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_groups),
    grupos_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_groups),
    grupos_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_groups),
    grupos_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_groups),
    grupos_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_groups),
    
    grupos_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_groups),
    grupos_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_groups),
    grupos_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_groups),
    grupos_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_groups),
    grupos_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_groups),
    grupos_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_groups),
    
    grupos_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_groups),
    grupos_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_groups),
    grupos_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_groups),
    grupos_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_groups),
    grupos_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_groups),
    grupos_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_groups)
  )

malnutrition_16_final <- malnutrition_16_new[, 289:length(
  names(malnutrition_16_new))]

malnutrition_16 <- cbind(malnutrition_16_thresholded, malnutrition_16_final)

malnutrition_path <- paste("malnutrition_final16.csv", sep = "")

doc_path <- ("dataset")

write_csv(as.data.frame(malnutrition_16), path(doc_path,malnutrition_path))  























## 2017


malnutrition_17 <- malnutrition_final %>%
  filter(year==2017) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.36_NDVI, mean.37_NDVI, mean.38_NDVI,mean.39_NDVI, mean.40_NDVI, 
            mean.41_NDVI, 
            mean.42_NDVI, mean.43_NDVI, mean.44_NDVI,mean.45_NDVI, mean.46_NDVI, 
            mean.47_NDVI,
            
            mean.201701_pr,mean.201702_pr,mean.201703_pr,mean.201704_pr,
            mean.201705_pr,mean.201706_pr,
            mean.201707_pr,mean.201708_pr,mean.201709_pr,mean.201710_pr,
            mean.201711_pr,mean.201712_pr,
            
            mean.201701_tmmx,mean.201702_tmmx,mean.201703_tmmx,mean.201704_tmmx,
            mean.201705_tmmx,mean.201706_tmmx,
            mean.201707_tmmx,mean.201708_tmmx,mean.201709_tmmx,mean.201710_tmmx,
            mean.201711_tmmx,mean.201712_tmmx,
            
            mean.201701_tmmn,mean.201702_tmmn,mean.201703_tmmn,mean.201704_tmmn,
            mean.201705_tmmn,mean.201706_tmmn,
            mean.201707_tmmn,mean.201708_tmmn,mean.201709_tmmn,mean.201710_tmmn,
            mean.201711_tmmn,mean.201712_tmmn)


# 2. Generar los umbrales climatológicos .......................................

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_17))
columns_tmmn <- grepl("tmmn", names(malnutrition_17))

# Dividir las columnas seleccionadas entre 10
malnutrition_17[, columns_tmmx] <- malnutrition_17[, columns_tmmx] / 10
malnutrition_17[, columns_tmmn] <- malnutrition_17[, columns_tmmn] / 10


# Crear dataframe de umbrales por región
umbrales_region <- data.frame(conglomerado = unique(malnutrition_17$conglomerado),
                              veg_low_10 = rep(0, 2056),
                              veg_low_5 = rep(0, 2056),
                              veg_low_1 = rep(0, 2056),
                              veg_high_90 = rep(0, 2056),
                              veg_high_95 = rep(0, 2056),
                              veg_high_99 = rep(0, 2056),
                              prec_low_10 = rep(0, 2056),
                              prec_low_5 = rep(0, 2056),
                              prec_low_1 = rep(0, 2056),
                              prec_high_90 = rep(0, 2056),
                              prec_high_95 = rep(0, 2056),
                              prec_high_99 = rep(0, 2056),
                              tmax_low_10 = rep(0, 2056),
                              tmax_low_5 = rep(0, 2056),
                              tmax_low_1 = rep(0, 2056),
                              tmax_high_90 = rep(0, 2056),
                              tmax_high_95 = rep(0, 2056),
                              tmax_high_99 = rep(0, 2056),
                              tmin_low_10 = rep(0, 2056),
                              tmin_low_5 = rep(0, 2056),
                              tmin_low_1 = rep(0, 2056),
                              tmin_high_90 = rep(0, 2056),
                              tmin_high_95 = rep(0, 2056),
                              tmin_high_99 = rep(0, 2056))

congs <- unique(malnutrition_17$conglomerado)

# Calcular los umbrales para cada región
for (i in 1:length(congs)) {
  data_filtered <- malnutrition_17 %>%
    filter(conglomerado == congs[i])
  
  # Vegetacion
  
  umbrales_region$veg_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.10, 
    na.rm = TRUE)
  umbrales_region$veg_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$veg_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.05, 
    na.rm = TRUE)
  umbrales_region$veg_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$veg_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.01, 
    na.rm = TRUE)
  umbrales_region$veg_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Precipitacion
  
  
  umbrales_region$prec_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$prec_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$prec_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$prec_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$prec_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$prec_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  
  # Temperatura maxima
  
  umbrales_region$tmax_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmax_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmax_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmax_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmax_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmax_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Temperatura minima
  
  umbrales_region$tmin_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmin_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmin_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmin_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmin_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmin_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.99, 
    na.rm =TRUE)
}


# Realizar merge con el dataframe "malnutrition_16"
malnutrition_17_with_thresholds <- malnutrition_17 %>%
  left_join(umbrales_region, by = "conglomerado")

##################################################################################

malnutrition_17_full <- malnutrition_final %>%
  filter(year==2017) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.12_NDVI, mean.13_NDVI, mean.14_NDVI,mean.15_NDVI, 
            mean.16_NDVI, mean.17_NDVI, 
            mean.18_NDVI, mean.19_NDVI, mean.20_NDVI,mean.21_NDVI, 
            mean.22_NDVI, mean.23_NDVI,
            
            mean.201501_pr,mean.201502_pr,mean.201503_pr,mean.201504_pr,
            mean.201505_pr,mean.201506_pr,
            mean.201507_pr,mean.201508_pr,mean.201509_pr,mean.201510_pr,
            mean.201511_pr,mean.201512_pr,
            
            mean.201501_tmmx,mean.201502_tmmx,mean.201503_tmmx,mean.201504_tmmx,
            mean.201505_tmmx,mean.201506_tmmx,
            mean.201507_tmmx,mean.201508_tmmx,mean.201509_tmmx,mean.201510_tmmx,
            mean.201511_tmmx,mean.201512_tmmx,
            
            mean.201501_tmmn,mean.201502_tmmn,mean.201503_tmmn,mean.201504_tmmn,
            mean.201505_tmmn,mean.201506_tmmn,
            mean.201507_tmmn,mean.201508_tmmn,mean.201509_tmmn,mean.201510_tmmn,
            mean.201511_tmmn,mean.201512_tmmn,
            
            
            
            mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, 
            mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, 
            mean.10_NDVI, mean.11_NDVI,
            
            mean.201401_pr,mean.201402_pr,mean.201403_pr,mean.201404_pr, 
            mean.201405_pr,mean.201406_pr, mean.201407_pr,mean.201408_pr,
            mean.201409_pr,mean.201410_pr,mean.201411_pr,mean.201412_pr,
            
            mean.201401_tmmx,mean.201402_tmmx,mean.201403_tmmx,mean.201404_tmmx,
            mean.201405_tmmx,mean.201406_tmmx, mean.201407_tmmx,mean.201408_tmmx,
            mean.201409_tmmx,mean.201410_tmmx,mean.201411_tmmx,mean.201412_tmmx,
            
            mean.201401_tmmn,mean.201402_tmmn,mean.201403_tmmn,mean.201404_tmmn,
            mean.201405_tmmn,mean.201406_tmmn, mean.201407_tmmn,mean.201408_tmmn,
            mean.201409_tmmn,mean.201410_tmmn,mean.201411_tmmn,mean.201412_tmmn,
            
            
            
            mean.24_NDVI, mean.25_NDVI, mean.26_NDVI,mean.27_NDVI, 
            mean.28_NDVI, mean.29_NDVI, 
            mean.30_NDVI, mean.31_NDVI, mean.32_NDVI,mean.33_NDVI, 
            mean.34_NDVI, mean.35_NDVI,
            
            mean.201601_pr,mean.201602_pr,mean.201603_pr,mean.201604_pr,
            mean.201605_pr,mean.201606_pr,
            mean.201607_pr,mean.201608_pr,mean.201609_pr,mean.201610_pr,
            mean.201611_pr,mean.201612_pr,
            
            mean.201601_tmmx,mean.201602_tmmx,mean.201603_tmmx,mean.201604_tmmx,
            mean.201605_tmmx,mean.201606_tmmx,
            mean.201607_tmmx,mean.201608_tmmx,mean.201609_tmmx,mean.201610_tmmx,
            mean.201611_tmmx,mean.201612_tmmx,
            
            mean.201601_tmmn,mean.201602_tmmn,mean.201603_tmmn,mean.201604_tmmn,
            mean.201605_tmmn,mean.201606_tmmn,
            mean.201607_tmmn,mean.201608_tmmn,mean.201609_tmmn,mean.201610_tmmn,
            mean.201611_tmmn,mean.201612_tmmn,
            
            mean.36_NDVI, mean.37_NDVI, mean.38_NDVI,mean.39_NDVI, mean.40_NDVI, 
            mean.41_NDVI, 
            mean.42_NDVI, mean.43_NDVI, mean.44_NDVI,mean.45_NDVI, mean.46_NDVI, 
            mean.47_NDVI,
            
            mean.201701_pr,mean.201702_pr,mean.201703_pr,mean.201704_pr,
            mean.201705_pr,mean.201706_pr,
            mean.201707_pr,mean.201708_pr,mean.201709_pr,mean.201710_pr,
            mean.201711_pr,mean.201712_pr,
            
            mean.201701_tmmx,mean.201702_tmmx,mean.201703_tmmx,mean.201704_tmmx,
            mean.201705_tmmx,mean.201706_tmmx,
            mean.201707_tmmx,mean.201708_tmmx,mean.201709_tmmx,mean.201710_tmmx,
            mean.201711_tmmx,mean.201712_tmmx,
            
            mean.201701_tmmn,mean.201702_tmmn,mean.201703_tmmn,mean.201704_tmmn,
            mean.201705_tmmn,mean.201706_tmmn,
            mean.201707_tmmn,mean.201708_tmmn,mean.201709_tmmn,mean.201710_tmmn,
            mean.201711_tmmn,mean.201712_tmmn
  )

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_17_full))
columns_tmmn <- grepl("tmmn", names(malnutrition_17_full))

# Dividir las columnas seleccionadas entre 10
malnutrition_17_full[, columns_tmmx] <- malnutrition_17_full[, columns_tmmx] / 10
malnutrition_17_full[, columns_tmmn] <- malnutrition_17_full[, columns_tmmn] / 10


# 3. Aplicar los umbrales en términos simples ..................................

malnutrition_17_full_with_thresholds <- malnutrition_17_full %>%
  left_join(umbrales_region, by = "conglomerado")

malnutrition_17_thresholded <- malnutrition_17_full_with_thresholds %>%
  
  mutate(
    
    #NDVI
    NDVI_high_90 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_90)),
    NDVI_low_10 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_10)),
    NDVI_high_95 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_95)),
    NDVI_low_5 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_5)),
    NDVI_high_99 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_99)),
    NDVI_low_1 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_1)),
    
    NDVI_range = apply(
      select(., contains("NDVI")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(
      select(., c("mean.41_NDVI", "mean.42_NDVI", "mean.43_NDVI"))),
    NDVI_first_months = rowMeans(
      select(., c("mean.36_NDVI", "mean.37_NDVI", "mean.38_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    
    #PR 
    pr_high_90 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_90)),
    pr_low_10 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_10)),
    pr_high_95 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_95)),
    pr_low_5 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_5)),
    pr_high_99 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_99)),
    pr_low_1 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_1)),
    
    pr_range = apply(
      select(., contains("pr")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(
      select(., c("mean.201706_pr","mean.201707_pr", "mean.201708_pr"))),
    pr_first_months = rowMeans(
      select(., c("mean.201701_pr", "mean.201702_pr", "mean.201703_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    
    #tmmx
    tmax_high_90 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_90)),
    tmax_low_10 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_10)),
    tmax_high_95 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_95)),
    tmax_low_5 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_5)),
    tmax_high_99 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_99)),
    tmax_low_1 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_1)),
    
    tmax_range = apply(
      select(., contains("tmmx")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMAX = rowMeans(.[,which(names(malnutrition_17_full)=="mean.201701_tmmx"):
                        which(names(malnutrition_17_full)=="mean.201712_tmmx")]),
    
    
    #tmmn
    tmin_high_90 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_90)),
    tmin_low_10 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_10)),
    tmin_high_95 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_95)),
    tmin_low_5 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_5)),
    tmin_high_99 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_99)),
    tmin_low_1 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_1)),
    
    tmin_range = apply(
      select(., contains("tmmn")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMIN = rowMeans(.[,which(names(malnutrition_17_full)=="mean.201701_tmmn"):
                        which(names(malnutrition_17_full)=="mean.201712_tmmn")]))%>%
  
  transmute(HHID, area_residence, region, conglomerado, longitudx, latitudy,
            
            malnutrition,
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, 
            NDVI_first_months, NDVI_seasonal_diff,
            NDVI_high_90, NDVI_high_95, NDVI_high_99, NDVI_low_10, NDVI_low_5, 
            NDVI_low_1,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, 
            pr_seasonal_diff,
            pr_high_90, pr_high_95, pr_high_99, pr_low_10,pr_low_5, pr_low_1,
            
            tmax_high_90, tmax_high_95, tmax_high_99, tmax_low_10, tmax_low_5, 
            tmax_low_1,
            
            tmin_high_90, tmin_high_95, tmin_high_99, tmin_low_10, tmin_low_5, 
            tmin_low_1,
            
            TMAX, TMIN)


# 4. Transformación de umbrales para consecución temporal ......................

### Transformación a 0 y 1

# Obtener las columnas que contienen "NDVI"
cols_with_NDVI <- grep("NDVI", names(malnutrition_17_with_thresholds), 
                       value = TRUE)
cols_with_pr <- grep("pr$", names(malnutrition_17_with_thresholds), 
                     value = TRUE)
cols_with_tmmx <- grep("tmmx$", names(malnutrition_17_with_thresholds), 
                       value = TRUE)
cols_with_tmmn <- grep("tmmn$", names(malnutrition_17_with_thresholds), 
                       value = TRUE)


# Crear nuevas columnas con 1 y 0 basadas en los umbrales
malnutrition_17_trans <- malnutrition_17_with_thresholds %>%
  mutate(
    across(cols_with_NDVI, ~ ifelse(. > veg_high_90, 1, 0),
           .names = "veg_tr_90_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_10, 1, 0),
           .names = "veg_tr_10_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_95, 1, 0),
           .names = "veg_tr_95_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_5, 1, 0),
           .names = "veg_tr_5_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_99, 1, 0),
           .names = "veg_tr_99_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_1, 1, 0),
           .names = "veg_tr_1_{.col}"),
    
    across(cols_with_pr, ~ ifelse(. > prec_high_90, 1, 0),
           .names = "prec_tr_90_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_10, 1, 0),
           .names = "prec_tr_10_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_95, 1, 0),
           .names = "prec_tr_95_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_5, 1, 0),
           .names = "prec_tr_5_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_99, 1, 0),
           .names = "prec_tr_99_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_1, 1, 0),
           .names = "prec_tr_1_{.col}"),
    
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_90, 1, 0),
           .names = "tmax_tr_90_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_10, 1, 0),
           .names = "tmax_tr_10_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_95, 1, 0),
           .names = "tmax_tr_95_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_5, 1, 0),
           .names = "tmax_tr_5_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_99, 1, 0),
           .names = "tmax_tr_99_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_1, 1, 0),
           .names = "tmax_tr_1_{.col}"),
    
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_90, 1, 0),
           .names = "tmin_tr_90_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_10, 1, 0),
           .names = "tmin_tr_10_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_95, 1, 0),
           .names = "tmin_tr_95_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_5, 1, 0),
           .names = "tmin_tr_5_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_99, 1, 0),
           .names = "tmin_tr_99_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_1, 1, 0),
           .names = "tmin_tr_1_{.col}")
    
  )



# 5. Funciones para conteo temporal con umbrales ...............................

# Definir umbrales
umbrales <- c("90", "95", "99", "1", "5", "10")

count_consecutive_ones <- function(x) {
  consecutivos <- rle(x)
  consecutivos$values[is.na(consecutivos$values)] <- 0
  if (all(x == 0)) {
    return(0)
  } else {
    max_consecutivos <- max(consecutivos$lengths[consecutivos$values == 1])
    return(max_consecutivos)
  }
}


# Función para contar grupos consecutivos de 1
count_consecutive_groups <- function(x) {
  consecutivos <- rle(x)
  grupos <- sum(consecutivos$lengths[consecutivos$values == 1] >= 2)
  grupos
}

cols_veg_99 <- grep(paste0("veg_tr_", "99"), names(malnutrition_17_trans), 
                    value = TRUE)
cols_veg_95 <- grep(paste0("veg_tr_", "95"), names(malnutrition_17_trans), 
                    value = TRUE)
cols_veg_90 <- grep(paste0("veg_tr_", "90"), names(malnutrition_17_trans), 
                    value = TRUE)
cols_veg_1 <- grep("veg_tr_1_", names(malnutrition_17_trans), value = TRUE)
cols_veg_5 <- grep(paste0("veg_tr_", "5"), names(malnutrition_17_trans), 
                   value = TRUE)
cols_veg_10 <- grep(paste0("veg_tr_", "10"), names(malnutrition_17_trans), 
                    value = TRUE)

cols_prec_99 <- grep(paste0("prec_tr_", "99"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_prec_95 <- grep(paste0("prec_tr_", "95"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_prec_90 <- grep(paste0("prec_tr_", "90"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_prec_1 <- grep("prec_tr_1_", names(malnutrition_17_trans), value = TRUE)
cols_prec_5 <- grep(paste0("prec_tr_", "5"), names(malnutrition_17_trans), 
                    value = TRUE)
cols_prec_10 <- grep(paste0("prec_tr_", "10"), names(malnutrition_17_trans), 
                     value = TRUE)

cols_tmax_99 <- grep(paste0("tmax_tr_", "99"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_tmax_95 <- grep(paste0("tmax_tr_", "95"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_tmax_90 <- grep(paste0("tmax_tr_", "90"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_tmax_1 <- grep("tmax_tr_1_", names(malnutrition_17_trans), value = TRUE)
cols_tmax_5 <- grep(paste0("tmax_tr_", "5"), names(malnutrition_17_trans), 
                    value = TRUE)
cols_tmax_10 <- grep(paste0("tmax_tr_", "10"), names(malnutrition_17_trans), 
                     value = TRUE)

cols_tmin_99 <- grep(paste0("tmin_tr_", "99"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_tmin_95 <- grep(paste0("tmin_tr_", "95"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_tmin_90 <- grep(paste0("tmin_tr_", "90"), names(malnutrition_17_trans), 
                     value = TRUE)
cols_tmin_1 <- grep("tmin_tr_1_", names(malnutrition_17_trans), value = TRUE)
cols_tmin_5 <- grep(paste0("tmin_tr_", "5"), names(malnutrition_17_trans), 
                    value = TRUE)
cols_tmin_10 <- grep(paste0("tmin_tr_", "10"), names(malnutrition_17_trans), 
                     value = TRUE)

# Cummulative sum

malnutrition_17_new <- malnutrition_17_trans[, 80:length(names(
  malnutrition_17_trans))]

malnutrition_17_new <- malnutrition_17_new %>%
  mutate(
    consecutivas_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_ones),
    consecutivas_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_ones),
    consecutivas_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_ones),
    consecutivas_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_ones),
    consecutivas_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_ones),
    consecutivas_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_ones),
    
    consecutivas_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_ones),
    consecutivas_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_ones),
    consecutivas_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_ones),
    consecutivas_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_ones),
    consecutivas_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_ones),
    consecutivas_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_ones),
    
    consecutivas_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_ones),
    consecutivas_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_ones),
    consecutivas_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_ones),
    consecutivas_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_ones),
    consecutivas_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_ones),
    consecutivas_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_ones),
    
    consecutivas_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_ones),
    consecutivas_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_ones),
    consecutivas_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_ones),
    consecutivas_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_ones),
    consecutivas_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_ones),
    consecutivas_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_ones)
  )

# Groups

malnutrition_17_new <- malnutrition_17_new %>%
  mutate(
    grupos_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_groups),
    grupos_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_groups),
    grupos_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_groups),
    grupos_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_groups),
    grupos_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_groups),
    grupos_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_groups),
    
    grupos_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_groups),
    grupos_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_groups),
    grupos_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_groups),
    grupos_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_groups),
    grupos_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_groups),
    grupos_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_groups),
    
    grupos_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_groups),
    grupos_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_groups),
    grupos_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_groups),
    grupos_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_groups),
    grupos_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_groups),
    grupos_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_groups),
    
    grupos_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_groups),
    grupos_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_groups),
    grupos_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_groups),
    grupos_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_groups),
    grupos_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_groups),
    grupos_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_groups)
  )

malnutrition_17_final <- malnutrition_17_new[, 289:length(
  names(malnutrition_17_new))]

malnutrition_17 <- cbind(malnutrition_17_thresholded, malnutrition_17_final)

malnutrition_path <- paste("malnutrition_final17.csv", sep = "")

doc_path <- ("dataset")

write_csv(as.data.frame(malnutrition_17), path(doc_path,malnutrition_path))  

































## 2018

malnutrition_18 <- malnutrition_final %>%
  filter(year==2018) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado, 
            
            mean.48_NDVI, mean.49_NDVI, mean.50_NDVI,mean.51_NDVI, mean.52_NDVI, 
            mean.53_NDVI, 
            mean.54_NDVI, mean.55_NDVI, mean.56_NDVI,mean.57_NDVI, mean.58_NDVI, 
            mean.59_NDVI,
            
            mean.201801_pr,mean.201802_pr,mean.201803_pr,mean.201804_pr,
            mean.201805_pr,mean.201806_pr,
            mean.201807_pr,mean.201808_pr,mean.201809_pr,mean.201810_pr,
            mean.201811_pr,mean.201812_pr,
            
            mean.201801_tmmx,mean.201802_tmmx,mean.201803_tmmx,mean.201804_tmmx,
            mean.201805_tmmx,mean.201806_tmmx,
            mean.201807_tmmx,mean.201808_tmmx,mean.201809_tmmx,mean.201810_tmmx,
            mean.201811_tmmx,mean.201812_tmmx,
            
            mean.201801_tmmn,mean.201802_tmmn,mean.201803_tmmn,mean.201804_tmmn,
            mean.201805_tmmn,mean.201806_tmmn,
            mean.201807_tmmn,mean.201808_tmmn,mean.201809_tmmn,mean.201810_tmmn,
            mean.201811_tmmn,mean.201812_tmmn)


# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_18))
columns_tmmn <- grepl("tmmn", names(malnutrition_18))

# Dividir las columnas seleccionadas entre 10
malnutrition_18[, columns_tmmx] <- malnutrition_18[, columns_tmmx] / 10
malnutrition_18[, columns_tmmn] <- malnutrition_18[, columns_tmmn] / 10


# Crear dataframe de umbrales por región
umbrales_region <- data.frame(conglomerado = unique(malnutrition_18$conglomerado),
                              veg_low_10 = rep(0, 2127),
                              veg_low_5 = rep(0, 2127),
                              veg_low_1 = rep(0, 2127),
                              veg_high_90 = rep(0, 2127),
                              veg_high_95 = rep(0, 2127),
                              veg_high_99 = rep(0, 2127),
                              prec_low_10 = rep(0, 2127),
                              prec_low_5 = rep(0, 2127),
                              prec_low_1 = rep(0, 2127),
                              prec_high_90 = rep(0, 2127),
                              prec_high_95 = rep(0, 2127),
                              prec_high_99 = rep(0, 2127),
                              tmax_low_10 = rep(0, 2127),
                              tmax_low_5 = rep(0, 2127),
                              tmax_low_1 = rep(0, 2127),
                              tmax_high_90 = rep(0, 2127),
                              tmax_high_95 = rep(0, 2127),
                              tmax_high_99 = rep(0, 2127),
                              tmin_low_10 = rep(0, 2127),
                              tmin_low_5 = rep(0, 2127),
                              tmin_low_1 = rep(0, 2127),
                              tmin_high_90 = rep(0, 2127),
                              tmin_high_95 = rep(0, 2127),
                              tmin_high_99 = rep(0, 2127))

congs <- unique(malnutrition_18$conglomerado)

# Calcular los umbrales para cada región
for (i in 1:length(congs)) {
  data_filtered <- malnutrition_18 %>%
    filter(conglomerado == congs[i])
  
  # Vegetacion
  
  umbrales_region$veg_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.10, 
    na.rm = TRUE)
  umbrales_region$veg_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$veg_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.05, 
    na.rm = TRUE)
  umbrales_region$veg_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$veg_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.01, 
    na.rm = TRUE)
  umbrales_region$veg_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Precipitacion
  
  
  umbrales_region$prec_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$prec_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$prec_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$prec_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$prec_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$prec_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  
  # Temperatura maxima
  
  umbrales_region$tmax_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmax_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmax_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmax_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmax_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmax_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Temperatura minima
  
  umbrales_region$tmin_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmin_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmin_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmin_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmin_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmin_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.99, 
    na.rm =TRUE)
}


# Realizar merge con el dataframe "malnutrition_18"
malnutrition_18_with_thresholds <- malnutrition_18 %>%
  left_join(umbrales_region, by = "conglomerado")

##################################################################################

malnutrition_18_full <- malnutrition_final %>%
  filter(year==2018) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.12_NDVI, mean.13_NDVI, mean.14_NDVI,mean.15_NDVI, 
            mean.16_NDVI, mean.17_NDVI, 
            mean.18_NDVI, mean.19_NDVI, mean.20_NDVI,mean.21_NDVI, 
            mean.22_NDVI, mean.23_NDVI,
            
            mean.201501_pr,mean.201502_pr,mean.201503_pr,mean.201504_pr,
            mean.201505_pr,mean.201506_pr,
            mean.201507_pr,mean.201508_pr,mean.201509_pr,mean.201510_pr,
            mean.201511_pr,mean.201512_pr,
            
            mean.201501_tmmx,mean.201502_tmmx,mean.201503_tmmx,mean.201504_tmmx,
            mean.201505_tmmx,mean.201506_tmmx,
            mean.201507_tmmx,mean.201508_tmmx,mean.201509_tmmx,mean.201510_tmmx,
            mean.201511_tmmx,mean.201512_tmmx,
            
            mean.201501_tmmn,mean.201502_tmmn,mean.201503_tmmn,mean.201504_tmmn,
            mean.201505_tmmn,mean.201506_tmmn,
            mean.201507_tmmn,mean.201508_tmmn,mean.201509_tmmn,mean.201510_tmmn,
            mean.201511_tmmn,mean.201512_tmmn,
            
            
            
            mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, 
            mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, 
            mean.10_NDVI, mean.11_NDVI,
            
            mean.201401_pr,mean.201402_pr,mean.201403_pr,mean.201404_pr, 
            mean.201405_pr,mean.201406_pr, mean.201407_pr,mean.201408_pr,
            mean.201409_pr,mean.201410_pr,mean.201411_pr,mean.201412_pr,
            
            mean.201401_tmmx,mean.201402_tmmx,mean.201403_tmmx,mean.201404_tmmx,
            mean.201405_tmmx,mean.201406_tmmx, mean.201407_tmmx,mean.201408_tmmx,
            mean.201409_tmmx,mean.201410_tmmx,mean.201411_tmmx,mean.201412_tmmx,
            
            mean.201401_tmmn,mean.201402_tmmn,mean.201403_tmmn,mean.201404_tmmn,
            mean.201405_tmmn,mean.201406_tmmn, mean.201407_tmmn,mean.201408_tmmn,
            mean.201409_tmmn,mean.201410_tmmn,mean.201411_tmmn,mean.201412_tmmn,
            
            
            
            mean.24_NDVI, mean.25_NDVI, mean.26_NDVI,mean.27_NDVI, 
            mean.28_NDVI, mean.29_NDVI, 
            mean.30_NDVI, mean.31_NDVI, mean.32_NDVI,mean.33_NDVI, 
            mean.34_NDVI, mean.35_NDVI,
            
            mean.201601_pr,mean.201602_pr,mean.201603_pr,mean.201604_pr,
            mean.201605_pr,mean.201606_pr,
            mean.201607_pr,mean.201608_pr,mean.201609_pr,mean.201610_pr,
            mean.201611_pr,mean.201612_pr,
            
            mean.201601_tmmx,mean.201602_tmmx,mean.201603_tmmx,mean.201604_tmmx,
            mean.201605_tmmx,mean.201606_tmmx,
            mean.201607_tmmx,mean.201608_tmmx,mean.201609_tmmx,mean.201610_tmmx,
            mean.201611_tmmx,mean.201612_tmmx,
            
            mean.201601_tmmn,mean.201602_tmmn,mean.201603_tmmn,mean.201604_tmmn,
            mean.201605_tmmn,mean.201606_tmmn,
            mean.201607_tmmn,mean.201608_tmmn,mean.201609_tmmn,mean.201610_tmmn,
            mean.201611_tmmn,mean.201612_tmmn,
            
            mean.36_NDVI, mean.37_NDVI, mean.38_NDVI,mean.39_NDVI, mean.40_NDVI, 
            mean.41_NDVI, 
            mean.42_NDVI, mean.43_NDVI, mean.44_NDVI,mean.45_NDVI, mean.46_NDVI, 
            mean.47_NDVI,
            
            mean.201701_pr,mean.201702_pr,mean.201703_pr,mean.201704_pr,
            mean.201705_pr,mean.201706_pr,
            mean.201707_pr,mean.201708_pr,mean.201709_pr,mean.201710_pr,
            mean.201711_pr,mean.201712_pr,
            
            mean.201701_tmmx,mean.201702_tmmx,mean.201703_tmmx,mean.201704_tmmx,
            mean.201705_tmmx,mean.201706_tmmx,
            mean.201707_tmmx,mean.201708_tmmx,mean.201709_tmmx,mean.201710_tmmx,
            mean.201711_tmmx,mean.201712_tmmx,
            
            mean.201701_tmmn,mean.201702_tmmn,mean.201703_tmmn,mean.201704_tmmn,
            mean.201705_tmmn,mean.201706_tmmn,
            mean.201707_tmmn,mean.201708_tmmn,mean.201709_tmmn,mean.201710_tmmn,
            mean.201711_tmmn,mean.201712_tmmn,
            
            mean.48_NDVI, mean.49_NDVI, mean.50_NDVI,mean.51_NDVI, mean.52_NDVI, 
            mean.53_NDVI, 
            mean.54_NDVI, mean.55_NDVI, mean.56_NDVI,mean.57_NDVI, mean.58_NDVI, 
            mean.59_NDVI,
            
            mean.201801_pr,mean.201802_pr,mean.201803_pr,mean.201804_pr,
            mean.201805_pr,mean.201806_pr,
            mean.201807_pr,mean.201808_pr,mean.201809_pr,mean.201810_pr,
            mean.201811_pr,mean.201812_pr,
            
            mean.201801_tmmx,mean.201802_tmmx,mean.201803_tmmx,mean.201804_tmmx,
            mean.201805_tmmx,mean.201806_tmmx,
            mean.201807_tmmx,mean.201808_tmmx,mean.201809_tmmx,mean.201810_tmmx,
            mean.201811_tmmx,mean.201812_tmmx,
            
            mean.201801_tmmn,mean.201802_tmmn,mean.201803_tmmn,mean.201804_tmmn,
            mean.201805_tmmn,mean.201806_tmmn,
            mean.201807_tmmn,mean.201808_tmmn,mean.201809_tmmn,mean.201810_tmmn,
            mean.201811_tmmn,mean.201812_tmmn
  )

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_18_full))
columns_tmmn <- grepl("tmmn", names(malnutrition_18_full))

# Dividir las columnas seleccionadas entre 10
malnutrition_18_full[, columns_tmmx] <- malnutrition_18_full[, columns_tmmx] / 10
malnutrition_18_full[, columns_tmmn] <- malnutrition_18_full[, columns_tmmn] / 10


# 3. Aplicar los umbrales en términos simples ..................................

malnutrition_18_full_with_thresholds <- malnutrition_18_full %>%
  left_join(umbrales_region, by = "conglomerado")

malnutrition_18_thresholded <- malnutrition_18_full_with_thresholds %>%
  
  mutate(
    
    #NDVI
    NDVI_high_90 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_90)),
    NDVI_low_10 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_10)),
    NDVI_high_95 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_95)),
    NDVI_low_5 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_5)),
    NDVI_high_99 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_99)),
    NDVI_low_1 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_1)),
    
    NDVI_range = apply(
      select(., contains("NDVI")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(
      select(., c("mean.57_NDVI", "mean.58_NDVI", "mean.59_NDVI"))),
    NDVI_first_months = rowMeans(
      select(., c("mean.48_NDVI", "mean.49_NDVI", "mean.50_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    
    #PR 
    pr_high_90 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_90)),
    pr_low_10 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_10)),
    pr_high_95 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_95)),
    pr_low_5 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_5)),
    pr_high_99 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_99)),
    pr_low_1 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_1)),
    
    pr_range = apply(
      select(., contains("pr")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(
      select(., c("mean.201806_pr","mean.201807_pr", "mean.201808_pr"))),
    pr_first_months = rowMeans(
      select(., c("mean.201801_pr", "mean.201802_pr", "mean.201803_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    
    #tmmx
    tmax_high_90 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_90)),
    tmax_low_10 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_10)),
    tmax_high_95 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_95)),
    tmax_low_5 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_5)),
    tmax_high_99 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_99)),
    tmax_low_1 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_1)),
    
    tmax_range = apply(
      select(., contains("tmmx")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMAX = rowMeans(.[,which(names(malnutrition_18_full)=="mean.201801_tmmx"):
                        which(names(malnutrition_18_full)=="mean.201812_tmmx")]),
    
    
    #tmmn
    tmin_high_90 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_90)),
    tmin_low_10 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_10)),
    tmin_high_95 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_95)),
    tmin_low_5 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_5)),
    tmin_high_99 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_99)),
    tmin_low_1 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_1)),
    
    tmin_range = apply(
      select(., contains("tmmn")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMIN = rowMeans(.[,which(names(malnutrition_18_full)=="mean.201801_tmmn"):
                        which(names(malnutrition_18_full)=="mean.201812_tmmn")]))%>%
  
  transmute(HHID, area_residence, region, conglomerado, longitudx, latitudy,
            
            malnutrition,
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, 
            NDVI_first_months, NDVI_seasonal_diff,
            NDVI_high_90, NDVI_high_95, NDVI_high_99, NDVI_low_10, NDVI_low_5, 
            NDVI_low_1,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, 
            pr_seasonal_diff,
            pr_high_90, pr_high_95, pr_high_99, pr_low_10,pr_low_5, pr_low_1,
            
            tmax_high_90, tmax_high_95, tmax_high_99, tmax_low_10, tmax_low_5, 
            tmax_low_1,
            
            tmin_high_90, tmin_high_95, tmin_high_99, tmin_low_10, tmin_low_5, 
            tmin_low_1,
            
            TMAX, TMIN)


# 4. Transformación de umbrales para consecución temporal ......................

### Transformación a 0 y 1

# Obtener las columnas que contienen "NDVI"
cols_with_NDVI <- grep("NDVI", names(malnutrition_18_with_thresholds), 
                       value = TRUE)
cols_with_pr <- grep("pr$", names(malnutrition_18_with_thresholds), 
                     value = TRUE)
cols_with_tmmx <- grep("tmmx$", names(malnutrition_18_with_thresholds), 
                       value = TRUE)
cols_with_tmmn <- grep("tmmn$", names(malnutrition_18_with_thresholds), 
                       value = TRUE)


# Crear nuevas columnas con 1 y 0 basadas en los umbrales
malnutrition_18_trans <- malnutrition_18_with_thresholds %>%
  mutate(
    across(cols_with_NDVI, ~ ifelse(. > veg_high_90, 1, 0),
           .names = "veg_tr_90_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_10, 1, 0),
           .names = "veg_tr_10_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_95, 1, 0),
           .names = "veg_tr_95_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_5, 1, 0),
           .names = "veg_tr_5_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_99, 1, 0),
           .names = "veg_tr_99_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_1, 1, 0),
           .names = "veg_tr_1_{.col}"),
    
    across(cols_with_pr, ~ ifelse(. > prec_high_90, 1, 0),
           .names = "prec_tr_90_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_10, 1, 0),
           .names = "prec_tr_10_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_95, 1, 0),
           .names = "prec_tr_95_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_5, 1, 0),
           .names = "prec_tr_5_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_99, 1, 0),
           .names = "prec_tr_99_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_1, 1, 0),
           .names = "prec_tr_1_{.col}"),
    
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_90, 1, 0),
           .names = "tmax_tr_90_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_10, 1, 0),
           .names = "tmax_tr_10_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_95, 1, 0),
           .names = "tmax_tr_95_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_5, 1, 0),
           .names = "tmax_tr_5_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_99, 1, 0),
           .names = "tmax_tr_99_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_1, 1, 0),
           .names = "tmax_tr_1_{.col}"),
    
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_90, 1, 0),
           .names = "tmin_tr_90_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_10, 1, 0),
           .names = "tmin_tr_10_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_95, 1, 0),
           .names = "tmin_tr_95_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_5, 1, 0),
           .names = "tmin_tr_5_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_99, 1, 0),
           .names = "tmin_tr_99_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_1, 1, 0),
           .names = "tmin_tr_1_{.col}")
    
  )



# 5. Funciones para conteo temporal con umbrales ...............................

# Definir umbrales
umbrales <- c("90", "95", "99", "1", "5", "10")

count_consecutive_ones <- function(x) {
  consecutivos <- rle(x)
  consecutivos$values[is.na(consecutivos$values)] <- 0
  if (all(x == 0)) {
    return(0)
  } else {
    max_consecutivos <- max(consecutivos$lengths[consecutivos$values == 1])
    return(max_consecutivos)
  }
}


# Función para contar grupos consecutivos de 1
count_consecutive_groups <- function(x) {
  consecutivos <- rle(x)
  grupos <- sum(consecutivos$lengths[consecutivos$values == 1] >= 2)
  grupos
}

cols_veg_99 <- grep(paste0("veg_tr_", "99"), names(malnutrition_18_trans), 
                    value = TRUE)
cols_veg_95 <- grep(paste0("veg_tr_", "95"), names(malnutrition_18_trans), 
                    value = TRUE)
cols_veg_90 <- grep(paste0("veg_tr_", "90"), names(malnutrition_18_trans), 
                    value = TRUE)
cols_veg_1 <- grep("veg_tr_1_", names(malnutrition_18_trans), value = TRUE)
cols_veg_5 <- grep(paste0("veg_tr_", "5"), names(malnutrition_18_trans), 
                   value = TRUE)
cols_veg_10 <- grep(paste0("veg_tr_", "10"), names(malnutrition_18_trans), 
                    value = TRUE)

cols_prec_99 <- grep(paste0("prec_tr_", "99"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_prec_95 <- grep(paste0("prec_tr_", "95"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_prec_90 <- grep(paste0("prec_tr_", "90"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_prec_1 <- grep("prec_tr_1_", names(malnutrition_18_trans), value = TRUE)
cols_prec_5 <- grep(paste0("prec_tr_", "5"), names(malnutrition_18_trans), 
                    value = TRUE)
cols_prec_10 <- grep(paste0("prec_tr_", "10"), names(malnutrition_18_trans), 
                     value = TRUE)

cols_tmax_99 <- grep(paste0("tmax_tr_", "99"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_tmax_95 <- grep(paste0("tmax_tr_", "95"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_tmax_90 <- grep(paste0("tmax_tr_", "90"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_tmax_1 <- grep("tmax_tr_1_", names(malnutrition_18_trans), value = TRUE)
cols_tmax_5 <- grep(paste0("tmax_tr_", "5"), names(malnutrition_18_trans), 
                    value = TRUE)
cols_tmax_10 <- grep(paste0("tmax_tr_", "10"), names(malnutrition_18_trans), 
                     value = TRUE)

cols_tmin_99 <- grep(paste0("tmin_tr_", "99"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_tmin_95 <- grep(paste0("tmin_tr_", "95"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_tmin_90 <- grep(paste0("tmin_tr_", "90"), names(malnutrition_18_trans), 
                     value = TRUE)
cols_tmin_1 <- grep("tmin_tr_1_", names(malnutrition_18_trans), value = TRUE)
cols_tmin_5 <- grep(paste0("tmin_tr_", "5"), names(malnutrition_18_trans), 
                    value = TRUE)
cols_tmin_10 <- grep(paste0("tmin_tr_", "10"), names(malnutrition_18_trans), 
                     value = TRUE)

# Cummulative sum

malnutrition_18_new <- malnutrition_18_trans[, 80:length(names(
  malnutrition_18_trans))]

malnutrition_18_new <- malnutrition_18_new %>%
  mutate(
    consecutivas_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_ones),
    consecutivas_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_ones),
    consecutivas_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_ones),
    consecutivas_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_ones),
    consecutivas_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_ones),
    consecutivas_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_ones),
    
    consecutivas_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_ones),
    consecutivas_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_ones),
    consecutivas_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_ones),
    consecutivas_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_ones),
    consecutivas_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_ones),
    consecutivas_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_ones),
    
    consecutivas_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_ones),
    consecutivas_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_ones),
    consecutivas_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_ones),
    consecutivas_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_ones),
    consecutivas_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_ones),
    consecutivas_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_ones),
    
    consecutivas_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_ones),
    consecutivas_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_ones),
    consecutivas_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_ones),
    consecutivas_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_ones),
    consecutivas_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_ones),
    consecutivas_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_ones)
  )

# Groups

malnutrition_18_new <- malnutrition_18_new %>%
  mutate(
    grupos_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_groups),
    grupos_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_groups),
    grupos_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_groups),
    grupos_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_groups),
    grupos_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_groups),
    grupos_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_groups),
    
    grupos_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_groups),
    grupos_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_groups),
    grupos_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_groups),
    grupos_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_groups),
    grupos_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_groups),
    grupos_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_groups),
    
    grupos_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_groups),
    grupos_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_groups),
    grupos_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_groups),
    grupos_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_groups),
    grupos_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_groups),
    grupos_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_groups),
    
    grupos_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_groups),
    grupos_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_groups),
    grupos_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_groups),
    grupos_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_groups),
    grupos_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_groups),
    grupos_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_groups)
  )

malnutrition_18_final <- malnutrition_18_new[, 289:length(
  names(malnutrition_18_new))]

malnutrition_18 <- cbind(malnutrition_18_thresholded, malnutrition_18_final)

malnutrition_path <- paste("malnutrition_final18.csv", sep = "")

doc_path <- ("dataset")

write_csv(as.data.frame(malnutrition_18), path(doc_path,malnutrition_path)) 




## 2019


malnutrition_19 <- malnutrition_final %>%
  filter(year==2019) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.60_NDVI, mean.61_NDVI, mean.62_NDVI,mean.63_NDVI, mean.64_NDVI, 
            mean.65_NDVI, 
            mean.66_NDVI, mean.67_NDVI, mean.68_NDVI,mean.69_NDVI, mean.70_NDVI, 
            mean.71_NDVI,
            
            mean.201901_pr,mean.201902_pr,mean.201903_pr,mean.201904_pr,
            mean.201905_pr,mean.201906_pr,
            mean.201907_pr,mean.201908_pr,mean.201909_pr,mean.201910_pr,
            mean.201911_pr,mean.201912_pr,
            
            mean.201901_tmmx,mean.201902_tmmx,mean.201903_tmmx,mean.201904_tmmx,
            mean.201905_tmmx,mean.201906_tmmx,
            mean.201907_tmmx,mean.201908_tmmx,mean.201909_tmmx,mean.201910_tmmx,
            mean.201911_tmmx,mean.201912_tmmx,
            
            mean.201901_tmmn,mean.201902_tmmn,mean.201903_tmmn,mean.201904_tmmn,
            mean.201905_tmmn,mean.201906_tmmn,
            mean.201907_tmmn,mean.201908_tmmn,mean.201909_tmmn,mean.201910_tmmn,
            mean.201911_tmmn,mean.201912_tmmn)


# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_19))
columns_tmmn <- grepl("tmmn", names(malnutrition_19))

# Dividir las columnas seleccionadas entre 10
malnutrition_19[, columns_tmmx] <- malnutrition_19[, columns_tmmx] / 10
malnutrition_19[, columns_tmmn] <- malnutrition_19[, columns_tmmn] / 10


# Crear dataframe de umbrales por región
umbrales_region <- data.frame(conglomerado = unique(malnutrition_19$conglomerado),
                              veg_low_10 = rep(0, 772),
                              veg_low_5 = rep(0, 772),
                              veg_low_1 = rep(0, 772),
                              veg_high_90 = rep(0, 772),
                              veg_high_95 = rep(0, 772),
                              veg_high_99 = rep(0, 772),
                              prec_low_10 = rep(0, 772),
                              prec_low_5 = rep(0, 772),
                              prec_low_1 = rep(0, 772),
                              prec_high_90 = rep(0, 772),
                              prec_high_95 = rep(0, 772),
                              prec_high_99 = rep(0, 772),
                              tmax_low_10 = rep(0, 772),
                              tmax_low_5 = rep(0, 772),
                              tmax_low_1 = rep(0, 772),
                              tmax_high_90 = rep(0, 772),
                              tmax_high_95 = rep(0, 772),
                              tmax_high_99 = rep(0, 772),
                              tmin_low_10 = rep(0, 772),
                              tmin_low_5 = rep(0, 772),
                              tmin_low_1 = rep(0, 772),
                              tmin_high_90 = rep(0, 772),
                              tmin_high_95 = rep(0, 772),
                              tmin_high_99 = rep(0, 772))

congs <- unique(malnutrition_19$conglomerado)

# Calcular los umbrales para cada región
for (i in 1:length(congs)) {
  data_filtered <- malnutrition_19 %>%
    filter(conglomerado == congs[i])
  
  # Vegetacion
  
  umbrales_region$veg_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.10, 
    na.rm = TRUE)
  umbrales_region$veg_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$veg_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.05, 
    na.rm = TRUE)
  umbrales_region$veg_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$veg_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.01, 
    na.rm = TRUE)
  umbrales_region$veg_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d+_NDVI", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Precipitacion
  
  
  umbrales_region$prec_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$prec_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$prec_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$prec_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$prec_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$prec_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_pr", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  
  # Temperatura maxima
  
  umbrales_region$tmax_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmax_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmax_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmax_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmax_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmax_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmx", names(data_filtered))], 0.99, 
    na.rm =TRUE)
  
  # Temperatura minima
  
  umbrales_region$tmin_low_10[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.10, 
    na.rm =TRUE)
  umbrales_region$tmin_high_90[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.90, 
    na.rm =TRUE)
  umbrales_region$tmin_low_5[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.05, 
    na.rm =TRUE)
  umbrales_region$tmin_high_95[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.95, 
    na.rm =TRUE)
  umbrales_region$tmin_low_1[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.01, 
    na.rm =TRUE)
  umbrales_region$tmin_high_99[i] <- quantile(
    data_filtered[, grep("mean\\.\\d{6}_tmmn", names(data_filtered))], 0.99, 
    na.rm =TRUE)
}


# Realizar merge con el dataframe "malnutrition_19"
malnutrition_19_with_thresholds <- malnutrition_19 %>%
  left_join(umbrales_region, by = "conglomerado")

##################################################################################


malnutrition_19_full <- malnutrition_final %>%
  filter(year==2019) %>%
  #mutate(across(.cols = everything(), .f = na.locf)) %>%
  na.omit() %>%
  transmute(HHID = HHID.x, malnutrition, longitudx, latitudy, area_residence, 
            region, conglomerado,
            
            mean.12_NDVI, mean.13_NDVI, mean.14_NDVI,mean.15_NDVI, 
            mean.16_NDVI, mean.17_NDVI, 
            mean.18_NDVI, mean.19_NDVI, mean.20_NDVI,mean.21_NDVI, 
            mean.22_NDVI, mean.23_NDVI,
            
            mean.201501_pr,mean.201502_pr,mean.201503_pr,mean.201504_pr,
            mean.201505_pr,mean.201506_pr,
            mean.201507_pr,mean.201508_pr,mean.201509_pr,mean.201510_pr,
            mean.201511_pr,mean.201512_pr,
            
            mean.201501_tmmx,mean.201502_tmmx,mean.201503_tmmx,mean.201504_tmmx,
            mean.201505_tmmx,mean.201506_tmmx,
            mean.201507_tmmx,mean.201508_tmmx,mean.201509_tmmx,mean.201510_tmmx,
            mean.201511_tmmx,mean.201512_tmmx,
            
            mean.201501_tmmn,mean.201502_tmmn,mean.201503_tmmn,mean.201504_tmmn,
            mean.201505_tmmn,mean.201506_tmmn,
            mean.201507_tmmn,mean.201508_tmmn,mean.201509_tmmn,mean.201510_tmmn,
            mean.201511_tmmn,mean.201512_tmmn,
            
            
            
            mean.0_NDVI, mean.1_NDVI, mean.2_NDVI,mean.3_NDVI, mean.4_NDVI, 
            mean.5_NDVI, mean.6_NDVI, mean.7_NDVI, mean.8_NDVI,mean.9_NDVI, 
            mean.10_NDVI, mean.11_NDVI,
            
            mean.201401_pr,mean.201402_pr,mean.201403_pr,mean.201404_pr, 
            mean.201405_pr,mean.201406_pr, mean.201407_pr,mean.201408_pr,
            mean.201409_pr,mean.201410_pr,mean.201411_pr,mean.201412_pr,
            
            mean.201401_tmmx,mean.201402_tmmx,mean.201403_tmmx,mean.201404_tmmx,
            mean.201405_tmmx,mean.201406_tmmx, mean.201407_tmmx,mean.201408_tmmx,
            mean.201409_tmmx,mean.201410_tmmx,mean.201411_tmmx,mean.201412_tmmx,
            
            mean.201401_tmmn,mean.201402_tmmn,mean.201403_tmmn,mean.201404_tmmn,
            mean.201405_tmmn,mean.201406_tmmn, mean.201407_tmmn,mean.201408_tmmn,
            mean.201409_tmmn,mean.201410_tmmn,mean.201411_tmmn,mean.201412_tmmn,
            
            
            
            mean.24_NDVI, mean.25_NDVI, mean.26_NDVI,mean.27_NDVI, 
            mean.28_NDVI, mean.29_NDVI, 
            mean.30_NDVI, mean.31_NDVI, mean.32_NDVI,mean.33_NDVI, 
            mean.34_NDVI, mean.35_NDVI,
            
            mean.201601_pr,mean.201602_pr,mean.201603_pr,mean.201604_pr,
            mean.201605_pr,mean.201606_pr,
            mean.201607_pr,mean.201608_pr,mean.201609_pr,mean.201610_pr,
            mean.201611_pr,mean.201612_pr,
            
            mean.201601_tmmx,mean.201602_tmmx,mean.201603_tmmx,mean.201604_tmmx,
            mean.201605_tmmx,mean.201606_tmmx,
            mean.201607_tmmx,mean.201608_tmmx,mean.201609_tmmx,mean.201610_tmmx,
            mean.201611_tmmx,mean.201612_tmmx,
            
            mean.201601_tmmn,mean.201602_tmmn,mean.201603_tmmn,mean.201604_tmmn,
            mean.201605_tmmn,mean.201606_tmmn,
            mean.201607_tmmn,mean.201608_tmmn,mean.201609_tmmn,mean.201610_tmmn,
            mean.201611_tmmn,mean.201612_tmmn,
            
            mean.36_NDVI, mean.37_NDVI, mean.38_NDVI,mean.39_NDVI, mean.40_NDVI, 
            mean.41_NDVI, 
            mean.42_NDVI, mean.43_NDVI, mean.44_NDVI,mean.45_NDVI, mean.46_NDVI, 
            mean.47_NDVI,
            
            mean.201701_pr,mean.201702_pr,mean.201703_pr,mean.201704_pr,
            mean.201705_pr,mean.201706_pr,
            mean.201707_pr,mean.201708_pr,mean.201709_pr,mean.201710_pr,
            mean.201711_pr,mean.201712_pr,
            
            mean.201701_tmmx,mean.201702_tmmx,mean.201703_tmmx,mean.201704_tmmx,
            mean.201705_tmmx,mean.201706_tmmx,
            mean.201707_tmmx,mean.201708_tmmx,mean.201709_tmmx,mean.201710_tmmx,
            mean.201711_tmmx,mean.201712_tmmx,
            
            mean.201701_tmmn,mean.201702_tmmn,mean.201703_tmmn,mean.201704_tmmn,
            mean.201705_tmmn,mean.201706_tmmn,
            mean.201707_tmmn,mean.201708_tmmn,mean.201709_tmmn,mean.201710_tmmn,
            mean.201711_tmmn,mean.201712_tmmn,
            
            mean.48_NDVI, mean.49_NDVI, mean.50_NDVI,mean.51_NDVI, mean.52_NDVI, 
            mean.53_NDVI, 
            mean.54_NDVI, mean.55_NDVI, mean.56_NDVI,mean.57_NDVI, mean.58_NDVI, 
            mean.59_NDVI,
            
            mean.201801_pr,mean.201802_pr,mean.201803_pr,mean.201804_pr,
            mean.201805_pr,mean.201806_pr,
            mean.201807_pr,mean.201808_pr,mean.201809_pr,mean.201810_pr,
            mean.201811_pr,mean.201812_pr,
            
            mean.201801_tmmx,mean.201802_tmmx,mean.201803_tmmx,mean.201804_tmmx,
            mean.201805_tmmx,mean.201806_tmmx,
            mean.201807_tmmx,mean.201808_tmmx,mean.201809_tmmx,mean.201810_tmmx,
            mean.201811_tmmx,mean.201812_tmmx,
            
            mean.201801_tmmn,mean.201802_tmmn,mean.201803_tmmn,mean.201804_tmmn,
            mean.201805_tmmn,mean.201806_tmmn,
            mean.201807_tmmn,mean.201808_tmmn,mean.201809_tmmn,mean.201810_tmmn,
            mean.201811_tmmn,mean.201812_tmmn,
            
            mean.60_NDVI, mean.61_NDVI, mean.62_NDVI,mean.63_NDVI, mean.64_NDVI, 
            mean.65_NDVI, 
            mean.66_NDVI, mean.67_NDVI, mean.68_NDVI,mean.69_NDVI, mean.70_NDVI, 
            mean.71_NDVI,
            
            mean.201901_pr,mean.201902_pr,mean.201903_pr,mean.201904_pr,
            mean.201905_pr,mean.201906_pr,
            mean.201907_pr,mean.201908_pr,mean.201909_pr,mean.201910_pr,
            mean.201911_pr,mean.201912_pr,
            
            mean.201901_tmmx,mean.201902_tmmx,mean.201903_tmmx,mean.201904_tmmx,
            mean.201905_tmmx,mean.201906_tmmx,
            mean.201907_tmmx,mean.201908_tmmx,mean.201909_tmmx,mean.201910_tmmx,
            mean.201911_tmmx,mean.201912_tmmx,
            
            mean.201901_tmmn,mean.201902_tmmn,mean.201903_tmmn,mean.201904_tmmn,
            mean.201905_tmmn,mean.201906_tmmn,
            mean.201907_tmmn,mean.201908_tmmn,mean.201909_tmmn,mean.201910_tmmn,
            mean.201911_tmmn,mean.201912_tmmn
  )

# Filtrar columnas que contienen "tmmx" y "tmmn"
columns_tmmx <- grepl("tmmx", names(malnutrition_19_full))
columns_tmmn <- grepl("tmmn", names(malnutrition_19_full))

# Dividir las columnas seleccionadas entre 10
malnutrition_19_full[, columns_tmmx] <- malnutrition_19_full[, columns_tmmx] / 10
malnutrition_19_full[, columns_tmmn] <- malnutrition_19_full[, columns_tmmn] / 10


# 3. Aplicar los umbrales en términos simples ..................................

malnutrition_19_full_with_thresholds <- malnutrition_19_full %>%
  left_join(umbrales_region, by = "conglomerado")

malnutrition_19_thresholded <- malnutrition_19_full_with_thresholds %>%
  
  mutate(
    
    #NDVI
    NDVI_high_90 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_90)),
    NDVI_low_10 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_10)),
    NDVI_high_95 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_95)),
    NDVI_low_5 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_5)),
    NDVI_high_99 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x > veg_high_99)),
    NDVI_low_1 = rowSums(
      sapply(select(., contains("NDVI")), function(x) x < veg_low_1)),
    
    NDVI_range = apply(
      select(., contains("NDVI")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    NDVI_mean = apply(select(., contains("NDVI")), 1, mean, na.rm = TRUE),
    NDVI_sd = apply(select(., contains("NDVI")), 1, sd, na.rm = TRUE),
    NDVI_median = apply(select(., contains("NDVI")), 1, median, na.rm = TRUE),
    NDVI_IQR = apply(select(., contains("NDVI")), 1, IQR, na.rm = TRUE),
    NDVI_last_months = rowMeans(
      select(., c("mean.69_NDVI", "mean.70_NDVI", "mean.71_NDVI"))),
    NDVI_first_months = rowMeans(
      select(., c("mean.60_NDVI", "mean.61_NDVI", "mean.62_NDVI"))),
    NDVI_seasonal_diff = NDVI_last_months - NDVI_first_months,
    
    
    #PR 
    pr_high_90 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_90)),
    pr_low_10 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_10)),
    pr_high_95 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_95)),
    pr_low_5 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_5)),
    pr_high_99 = rowSums(
      sapply(select(., contains("pr")), function(x) x > prec_high_99)),
    pr_low_1 = rowSums(
      sapply(select(., contains("pr")), function(x) x < prec_low_1)),
    
    pr_range = apply(
      select(., contains("pr")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    pr_mean = apply(select(., contains("pr")), 1, mean, na.rm = TRUE),
    pr_sd = apply(select(., contains("pr")), 1, sd, na.rm = TRUE),
    pr_median = apply(select(., contains("pr")), 1, median, na.rm = TRUE),
    pr_IQR = apply(select(., contains("pr")), 1, IQR, na.rm = TRUE),
    pr_last_months = rowMeans(
      select(., c("mean.201906_pr","mean.201907_pr", "mean.201908_pr"))),
    pr_first_months = rowMeans(
      select(., c("mean.201901_pr", "mean.201902_pr", "mean.201903_pr"))),
    pr_seasonal_diff = pr_last_months - pr_first_months,
    
    
    #tmmx
    tmax_high_90 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_90)),
    tmax_low_10 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_10)),
    tmax_high_95 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_95)),
    tmax_low_5 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_5)),
    tmax_high_99 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x > tmax_high_99)),
    tmax_low_1 = rowSums(
      sapply(select(., contains("tmmx")), function(x) x < tmax_low_1)),
    
    tmax_range = apply(
      select(., contains("tmmx")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMAX = rowMeans(.[,which(names(malnutrition_19_full)=="mean.201901_tmmx"):
                        which(names(malnutrition_19_full)=="mean.201912_tmmx")]),
    
    
    #tmmn
    tmin_high_90 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_90)),
    tmin_low_10 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_10)),
    tmin_high_95 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_95)),
    tmin_low_5 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_5)),
    tmin_high_99 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x > tmin_high_99)),
    tmin_low_1 = rowSums(
      sapply(select(., contains("tmmn")), function(x) x < tmin_low_1)),
    
    tmin_range = apply(
      select(., contains("tmmn")), 1, 
      function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    TMIN = rowMeans(.[,which(names(malnutrition_19_full)=="mean.201901_tmmn"):
                        which(names(malnutrition_19_full)=="mean.201912_tmmn")]))%>%
  
  transmute(HHID, area_residence, region, conglomerado, longitudx, latitudy,
            
            malnutrition,
            
            NDVI_mean, NDVI_sd, NDVI_median, NDVI_IQR, NDVI_last_months, 
            NDVI_first_months, NDVI_seasonal_diff,
            NDVI_high_90, NDVI_high_95, NDVI_high_99, NDVI_low_10, NDVI_low_5, 
            NDVI_low_1,
            
            pr_mean, pr_sd, pr_median, pr_IQR, pr_last_months, pr_first_months, 
            pr_seasonal_diff,
            pr_high_90, pr_high_95, pr_high_99, pr_low_10,pr_low_5, pr_low_1,
            
            tmax_high_90, tmax_high_95, tmax_high_99, tmax_low_10, tmax_low_5, 
            tmax_low_1,
            
            tmin_high_90, tmin_high_95, tmin_high_99, tmin_low_10, tmin_low_5, 
            tmin_low_1,
            
            TMAX, TMIN)


# 4. Transformación de umbrales para consecución temporal ......................

### Transformación a 0 y 1

# Obtener las columnas que contienen "NDVI"
cols_with_NDVI <- grep("NDVI", names(malnutrition_19_with_thresholds), 
                       value = TRUE)
cols_with_pr <- grep("pr$", names(malnutrition_19_with_thresholds), 
                     value = TRUE)
cols_with_tmmx <- grep("tmmx$", names(malnutrition_19_with_thresholds), 
                       value = TRUE)
cols_with_tmmn <- grep("tmmn$", names(malnutrition_19_with_thresholds), 
                       value = TRUE)


# Crear nuevas columnas con 1 y 0 basadas en los umbrales
malnutrition_19_trans <- malnutrition_19_with_thresholds %>%
  mutate(
    across(cols_with_NDVI, ~ ifelse(. > veg_high_90, 1, 0),
           .names = "veg_tr_90_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_10, 1, 0),
           .names = "veg_tr_10_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_95, 1, 0),
           .names = "veg_tr_95_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_5, 1, 0),
           .names = "veg_tr_5_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. > veg_high_99, 1, 0),
           .names = "veg_tr_99_{.col}"),
    across(cols_with_NDVI, ~ ifelse(. < veg_low_1, 1, 0),
           .names = "veg_tr_1_{.col}"),
    
    across(cols_with_pr, ~ ifelse(. > prec_high_90, 1, 0),
           .names = "prec_tr_90_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_10, 1, 0),
           .names = "prec_tr_10_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_95, 1, 0),
           .names = "prec_tr_95_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_5, 1, 0),
           .names = "prec_tr_5_{.col}"),
    across(cols_with_pr, ~ ifelse(. > prec_high_99, 1, 0),
           .names = "prec_tr_99_{.col}"),
    across(cols_with_pr, ~ ifelse(. < prec_low_1, 1, 0),
           .names = "prec_tr_1_{.col}"),
    
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_90, 1, 0),
           .names = "tmax_tr_90_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_10, 1, 0),
           .names = "tmax_tr_10_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_95, 1, 0),
           .names = "tmax_tr_95_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_5, 1, 0),
           .names = "tmax_tr_5_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. > tmax_high_99, 1, 0),
           .names = "tmax_tr_99_{.col}"),
    across(cols_with_tmmx, ~ ifelse(. < tmax_low_1, 1, 0),
           .names = "tmax_tr_1_{.col}"),
    
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_90, 1, 0),
           .names = "tmin_tr_90_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_10, 1, 0),
           .names = "tmin_tr_10_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_95, 1, 0),
           .names = "tmin_tr_95_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_5, 1, 0),
           .names = "tmin_tr_5_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. > tmin_high_99, 1, 0),
           .names = "tmin_tr_99_{.col}"),
    across(cols_with_tmmn, ~ ifelse(. < tmin_low_1, 1, 0),
           .names = "tmin_tr_1_{.col}")
    
  )



# 5. Funciones para conteo temporal con umbrales ...............................

# Definir umbrales
umbrales <- c("90", "95", "99", "1", "5", "10")

count_consecutive_ones <- function(x) {
  consecutivos <- rle(x)
  consecutivos$values[is.na(consecutivos$values)] <- 0
  if (all(x == 0)) {
    return(0)
  } else {
    max_consecutivos <- max(consecutivos$lengths[consecutivos$values == 1])
    return(max_consecutivos)
  }
}


# Función para contar grupos consecutivos de 1
count_consecutive_groups <- function(x) {
  consecutivos <- rle(x)
  grupos <- sum(consecutivos$lengths[consecutivos$values == 1] >= 2)
  grupos
}

cols_veg_99 <- grep(paste0("veg_tr_", "99"), names(malnutrition_19_trans), 
                    value = TRUE)
cols_veg_95 <- grep(paste0("veg_tr_", "95"), names(malnutrition_19_trans), 
                    value = TRUE)
cols_veg_90 <- grep(paste0("veg_tr_", "90"), names(malnutrition_19_trans), 
                    value = TRUE)
cols_veg_1 <- grep("veg_tr_1_", names(malnutrition_19_trans), value = TRUE)
cols_veg_5 <- grep(paste0("veg_tr_", "5"), names(malnutrition_19_trans), 
                   value = TRUE)
cols_veg_10 <- grep(paste0("veg_tr_", "10"), names(malnutrition_19_trans), 
                    value = TRUE)

cols_prec_99 <- grep(paste0("prec_tr_", "99"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_prec_95 <- grep(paste0("prec_tr_", "95"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_prec_90 <- grep(paste0("prec_tr_", "90"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_prec_1 <- grep("prec_tr_1_", names(malnutrition_19_trans), value = TRUE)
cols_prec_5 <- grep(paste0("prec_tr_", "5"), names(malnutrition_19_trans), 
                    value = TRUE)
cols_prec_10 <- grep(paste0("prec_tr_", "10"), names(malnutrition_19_trans), 
                     value = TRUE)

cols_tmax_99 <- grep(paste0("tmax_tr_", "99"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_tmax_95 <- grep(paste0("tmax_tr_", "95"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_tmax_90 <- grep(paste0("tmax_tr_", "90"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_tmax_1 <- grep("tmax_tr_1_", names(malnutrition_19_trans), value = TRUE)
cols_tmax_5 <- grep(paste0("tmax_tr_", "5"), names(malnutrition_19_trans), 
                    value = TRUE)
cols_tmax_10 <- grep(paste0("tmax_tr_", "10"), names(malnutrition_19_trans), 
                     value = TRUE)

cols_tmin_99 <- grep(paste0("tmin_tr_", "99"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_tmin_95 <- grep(paste0("tmin_tr_", "95"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_tmin_90 <- grep(paste0("tmin_tr_", "90"), names(malnutrition_19_trans), 
                     value = TRUE)
cols_tmin_1 <- grep("tmin_tr_1_", names(malnutrition_19_trans), value = TRUE)
cols_tmin_5 <- grep(paste0("tmin_tr_", "5"), names(malnutrition_19_trans), 
                    value = TRUE)
cols_tmin_10 <- grep(paste0("tmin_tr_", "10"), names(malnutrition_19_trans), 
                     value = TRUE)

# Cummulative sum

malnutrition_19_new <- malnutrition_19_trans[, 80:length(names(
  malnutrition_19_trans))]

malnutrition_19_new <- malnutrition_19_new %>%
  mutate(
    consecutivas_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_ones),
    consecutivas_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_ones),
    consecutivas_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_ones),
    consecutivas_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_ones),
    consecutivas_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_ones),
    consecutivas_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_ones),
    
    consecutivas_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_ones),
    consecutivas_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_ones),
    consecutivas_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_ones),
    consecutivas_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_ones),
    consecutivas_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_ones),
    consecutivas_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_ones),
    
    consecutivas_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_ones),
    consecutivas_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_ones),
    consecutivas_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_ones),
    consecutivas_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_ones),
    consecutivas_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_ones),
    consecutivas_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_ones),
    
    consecutivas_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_ones),
    consecutivas_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_ones),
    consecutivas_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_ones),
    consecutivas_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_ones),
    consecutivas_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_ones),
    consecutivas_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_ones)
  )

# Groups

malnutrition_19_new <- malnutrition_19_new %>%
  mutate(
    grupos_veg_99 = apply(across(cols_veg_99), 1, count_consecutive_groups),
    grupos_veg_95 = apply(across(cols_veg_95), 1, count_consecutive_groups),
    grupos_veg_90 = apply(across(cols_veg_90), 1, count_consecutive_groups),
    grupos_veg_1 = apply(across(cols_veg_1), 1, count_consecutive_groups),
    grupos_veg_5 = apply(across(cols_veg_5), 1, count_consecutive_groups),
    grupos_veg_10 = apply(across(cols_veg_10), 1, count_consecutive_groups),
    
    grupos_prec_99 = apply(across(cols_prec_99), 1, count_consecutive_groups),
    grupos_prec_95 = apply(across(cols_prec_95), 1, count_consecutive_groups),
    grupos_prec_90 = apply(across(cols_prec_90), 1, count_consecutive_groups),
    grupos_prec_1 = apply(across(cols_prec_1), 1, count_consecutive_groups),
    grupos_prec_5 = apply(across(cols_prec_5), 1, count_consecutive_groups),
    grupos_prec_10 = apply(across(cols_prec_10), 1, count_consecutive_groups),
    
    grupos_tmax_99 = apply(across(cols_tmax_99), 1, count_consecutive_groups),
    grupos_tmax_95 = apply(across(cols_tmax_95), 1, count_consecutive_groups),
    grupos_tmax_90 = apply(across(cols_tmax_90), 1, count_consecutive_groups),
    grupos_tmax_1 = apply(across(cols_tmax_1), 1, count_consecutive_groups),
    grupos_tmax_5 = apply(across(cols_tmax_5), 1, count_consecutive_groups),
    grupos_tmax_10 = apply(across(cols_tmax_10), 1, count_consecutive_groups),
    
    grupos_tmin_99 = apply(across(cols_tmin_99), 1, count_consecutive_groups),
    grupos_tmin_95 = apply(across(cols_tmin_95), 1, count_consecutive_groups),
    grupos_tmin_90 = apply(across(cols_tmin_90), 1, count_consecutive_groups),
    grupos_tmin_1 = apply(across(cols_tmin_1), 1, count_consecutive_groups),
    grupos_tmin_5 = apply(across(cols_tmin_5), 1, count_consecutive_groups),
    grupos_tmin_10 = apply(across(cols_tmin_10), 1, count_consecutive_groups)
  )

malnutrition_19_final <- malnutrition_19_new[, 289:length(
  names(malnutrition_19_new))]

malnutrition_19 <- cbind(malnutrition_19_thresholded, malnutrition_19_final)

malnutrition_path <- paste("malnutrition_final19.csv", sep = "")

doc_path <- ("dataset")

write_csv(as.data.frame(malnutrition_19), path(doc_path,malnutrition_path))