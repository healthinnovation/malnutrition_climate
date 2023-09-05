library(tidyverse)
library(haven)
library(fs)
library(readr)
library(dplyr)
library(purrr)


# 1. Selecting required years --------------------------------------------------

years <- c("2014", "2015", "2016", "2017", "2018", "2019")

for(i in 1:6) {
  doc_path <- path("dataset")
  year_path <- path(years[i])
  final_path <- path(doc_path, year_path)

  # 1.1 Selecting loading house and child ENDES dataset ------------------------  
  child_path <- "RECH6.SAV"
  house_path <- "RECH0.SAV"
  
  variable_child_path <- path(final_path, child_path)
  variable_house_path <- path(final_path, house_path)
  
  # 1.2 Selecting desired variables --------------------------------------------  
  child <- read_sav(variable_child_path) %>%
    select(HHID, HC12, HC3, HC2, HC1, HC72, HC0, HC11) %>%
    filter(HC12 != 99998) %>%
    mutate(weight = HC2/10, height = HC3/10, age = HC1, order = HC0)
  
  # 1.3 Creating malnutrition (target) variable --------------------------------
  child <- mutate(child, malnutrition = HC12/100) %>%
    mutate(malnutrition = case_when(
      malnutrition < 89 ~ 1,
      TRUE ~ 0)) 

  # 1.4 Feature engineering for some variables in 2015 -------------------------  
  if (i == 2){
    house <- read_sav(variable_house_path) %>%
      select(HHID, LONGITUDX, LATITUDY, HV025, HV024, HV001) %>%
      mutate(longitudx = LONGITUDX, latitudy = LATITUDY) %>%
      select(-c(LONGITUDX, LATITUDY))
  }
  
  # 1.5 Feature engineering for some variables in 2017 -------------------------
  else if (i==4){
    house <- read_sav(variable_house_path) %>%
      select(HHID, long_ccpp, lat_ccpp, HV025, HV024, HV001) %>%
      mutate(longitudx = long_ccpp, latitudy = lat_ccpp) %>%
      select(-c(long_ccpp, lat_ccpp))
  }
  
  
  else{
    house <- read_sav(variable_house_path) %>%
      select(HHID, longitudx, latitudy, HV025, HV024, HV001)
  }
  
  # 1.6 Creation of the dataset (child and household variables) ----------------
  df <- 
    child %>% 
    inner_join(house, by = "HHID") %>%
    mutate(area_residence =  HV025, region = HV024, HHID = as.character(HHID), conglomerado = HV001) %>%
    select(-c( HV025, HV024, HC12, HC3, HC1, HC2,HC72, HC0, HV001))
  
  malnutrition_path <- paste("malnutrition_",years[i],".csv", sep = "")
  dataset_path <- path(final_path, malnutrition_path)
  write_csv(df, dataset_path)   
}

################################################################################

library(tidyverse)
library(haven)
library(fs)
library(readr)
library(dplyr)
library(purrr)

# 1. Selecting required year --------------------------------------------------

year <- "2019"

doc_path <- path("dataset")
year_path <- path(year)
final_path <- path(doc_path, year_path)

# 1.1 Selecting loading house and child ENDES dataset ------------------------  
child_path <- "RECH6.SAV"
house_path <- "RECH0.SAV"

variable_child_path <- path(final_path, child_path)
variable_house_path <- path(final_path, house_path)

# 1.2 Selecting desired variables --------------------------------------------  
child <- read_sav(variable_child_path) %>%
  select(HHID, HC12, HC3, HC2, HC1, HC72, HC0, HC11) %>%
  filter(HC12 != 99998) %>%
  mutate(weight = HC2/10, height = HC3/10, age = HC1, order = HC0)

# 1.3 Creating malnutrition (target) variable --------------------------------
child <- mutate(child, malnutrition = HC12/100) %>%
  mutate(malnutrition = case_when(
    malnutrition < 89 ~ 1,
    TRUE ~ 0)) 

house <- read_sav(variable_house_path) %>%
    select(HHID, longitudx, latitudy, HV025, HV024, HV001)

# 1.6 Creation of the dataset (child and household variables) ----------------
df <- 
  child %>% 
  inner_join(house, by = "HHID") %>%
  mutate(area_residence =  HV025, region = HV024, HHID = as.character(HHID), conglomerado = HV001) %>%
  select(-c( HV025, HV024, HC12, HC3, HC1, HC2,HC72, HC0, HV001))

malnutrition_path <- paste("malnutrition_", year, ".csv", sep = "")
dataset_path <- path(final_path, malnutrition_path)
write_csv(df, dataset_path)

