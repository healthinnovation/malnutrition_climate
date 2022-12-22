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






