library(tidyverse)
library(sf)
library(glue)
library(exactextractr)
library(terra)
library(innovar)
library(sf)
library(stringr)
library(lubridate)

paths <- list.dirs()[33:38]
reading_csv_with_glue <- function(x){
  index_name <- list.files(
    path = glue("{paths[x]}"),
    pattern = "*.csv"
    ) %>% 
    sprintf(glue("{paths[x]}/%s"),.)
  
  data <- read_csv(index_name) %>% 
    mutate(
      year = as.numeric(
        str_extract(
          glue(
            "{paths[x]}"),
            pattern = "[0-9]+"
            )
          )
        ) %>% 
    distinct(longitudx,latitudy,.keep_all = T) %>% 
    st_as_sf(coords = c("longitudx","latitudy"),crs = 4326) %>% 
    st_buffer(dist = 5*1000) %>% 
    st_transform(4326)
  return(data)
}

index_csv <- lapply(1:length(paths),reading_csv_with_glue)

# 1. Loading raster layer -------------------------------------------------
pp <- rast("rasters/pp.tif")
tmax <- rast("rasters/temp_max.tif")
tmin <- rast("rasters/temp_min.tif")
ndvi <- rast("rasters/ndvi.tif")
evi <- rast("rasters/evi.tif")

# 2. Extracting meteorogical variables  -----------------------------------

extract_raster_value <- function(x,raster){
  input <- exact_extract(raster,index_csv[[x]],fun = 'mean')
  input$HHID <- index_csv[[x]]$HHID
  output <- input 
  return(output)
}

lista_pp <- lapply(1:length(index_csv), extract_raster_value, raster = pp)
lista_tmax <- lapply(1:length(index_csv), extract_raster_value, raster = tmax)
lista_tmin <- lapply(1:length(index_csv), extract_raster_value, raster = tmin)
lista_ndvi <- lapply(1:length(index_csv), extract_raster_value, raster = ndvi)
lista_savi <- lapply(1:length(index_csv), extract_raster_value, raster = evi)



