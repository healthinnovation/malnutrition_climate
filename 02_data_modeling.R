library(tidyverse)
library(exactextractr)
library(terra)
library(innovar)
library(sf)
library(stringr)
library(lubridate)
# 1. Loading raster layer -------------------------------------------------
data("Peru")
pp <- rast("rasters/pp_2019.tif")
tmax <- rast("rasters/temp_max_2019.tif")
tmin <- rast("rasters/temp_min_2019.tif")
ndvi <- rast("rasters/ndvi_2019.tif")
evi <- rast("rasters/evi_2019.tif")
# 2. Extracting meteorogical variables  -----------------------------------
pp_data <- exact_extract(x = pp,Peru,fun = 'mean')
pp_data$ubigeo <- Peru$ubigeo

tmx_data <- exact_extract(x = tmax,Peru,fun = 'mean')
tmx_data$ubigeo <- Peru$ubigeo

tmin_data <- exact_extract(x = tmin,Peru,fun = 'mean')
tmin_data$ubigeo <- Peru$ubigeo

ndvi_data <- exact_extract(x = ndvi,Peru,fun = 'mean')
ndvi_data$ubigeo <- Peru$ubigeo

evi_data <- exact_extract(x = evi,Peru,fun = 'mean')
evi_data$ubigeo <- Peru$ubigeo

# 3. Modeling long data format  -------------------------------------------

names(pp_data) <- seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "month") %>% 
  gsub("-",".",.) %>% sprintf("pp.%s",.) %>% append("ubigeo")

names(tmx_data) <- seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "month") %>% 
  gsub("-",".",.) %>% sprintf("tmx.%s",.) %>% append("ubigeo")

names(tmin_data) <- seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "month") %>% 
  gsub("-",".",.) %>% sprintf("tmin.%s",.)%>% append("ubigeo")

names(ndvi_data) <- seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "month") %>% 
  gsub("-",".",.) %>% sprintf("ndvi.%s",.)%>% append("ubigeo")

names(evi_data) <- seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "month") %>% 
  gsub("-",".",.) %>% sprintf("evi.%s",.)%>% append("ubigeo")


m1 <- left_join(
  pp_data,
  tmx_data,
  "ubigeo"
)

m2 <- left_join(
  m1,
  tmin_data,
  "ubigeo"
)

m3 <- left_join(
  m2,
  ndvi_data,
  "ubigeo"
)

m4 <- left_join(
  m3,
  evi_data,
  "ubigeo"
)

m5 <- left_join(
  m4,
  Peru,
  "ubigeo"
)

final_data <- m5 %>% 
  select(ubigeo,dep.code:capital,pp.2019.01.01:evi.2019.12.01,geometry) %>% 
  pivot_longer(
    cols = pp.2019.01.01:evi.2019.12.01,
    names_to = "variables",
    values_to = "value"
  ) %>% 
  select(ubigeo,dep.code:capital,variables,value,geometry) %>% 
  mutate(
    variable = str_extract(variables,pattern = "[a-z]+"),
    fecha = ymd(str_remove(variables,pattern = "[a-z]+."))
    ) %>% 
  select(ubigeo,dep.code:capital,variable,fecha,value,geometry)

write_sf(final_data,"dataset/variables_districts.gpkg")
write_csv(final_data %>% st_drop_geometry(),"dataset/variables_districts.csv")