library(tidyverse)
library(innovar)
library(rgee)
library(sf)
ee_Initialize()

# 1. Loading database -----------------------------------------------------
data("Peru") 
region_ee <- Peru %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  sf_as_ee()

# 2. Get climate information ----------------------------------------------
pp <- ee$ImageCollection$Dataset$IDAHO_EPSCOR_TERRACLIMATE %>% 
  ee$ImageCollection$filterDate("2019-01-01","2019-12-31") %>% 
  ee$ImageCollection$select("pr") %>% 
  ee$ImageCollection$toBands() %>% 
  ee$Image$clip(region_ee)

temp_max <- ee$ImageCollection$Dataset$IDAHO_EPSCOR_TERRACLIMATE %>% 
  ee$ImageCollection$filterDate("2019-01-01","2019-12-31") %>% 
  ee$ImageCollection$select("tmmx") %>% 
  ee$ImageCollection$toBands() %>% 
  ee$Image$clip(region_ee)

ndvi <- ee$ImageCollection$Dataset$MODIS_006_MOD13Q1 %>% 
  ee$ImageCollection$filter(ee$Filter$calendarRange(2019,2019,"month")) %>%
  ee$ImageCollection$select("NDVI") %>% 
  ee$ImageCollection$toBands() %>% 
  ee$Image$clip(region_ee) %>% 
  ee$Image$multiply(0.0001)

# 3. Downloading raster layer ---------------------------------------------

ee_as_raster(
  image = pp,
  region = region_ee,
  dsn = "rasters/pp_2019.tif",
  scale = 1000
  )

ee_as_raster(
  image = temp_max,
  region = region_ee,
  dsn = "rasters/temp_max_2019.tif",
  scale = 1000
)

ee_as_raster(
  image = ndvi,
  region = region_ee,
  dsn = "rasters/ndvi_2019.tif",
  scale = 1000
)





