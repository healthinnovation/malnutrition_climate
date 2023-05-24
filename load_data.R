load_data <- function(){
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
  
  data_list <- list(
    malnutrition_14 = malnutrition_14,
    malnutrition_15 = malnutrition_15,
    malnutrition_16 = malnutrition_16,
    malnutrition_17 = malnutrition_17,
    malnutrition_18 = malnutrition_18,
    malnutrition_19 = malnutrition_19
  )
  
  return(data_list)
  
}