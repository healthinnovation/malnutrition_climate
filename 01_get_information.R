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

temp_min <- ee$ImageCollection$Dataset$IDAHO_EPSCOR_TERRACLIMATE %>% 
  ee$ImageCollection$filterDate("2019-01-01","2019-12-31") %>% 
  ee$ImageCollection$select("tmmn") %>% 
  ee$ImageCollection$toBands() %>% 
  ee$Image$clip(region_ee)


# 3. Vegetation index -----------------------------------------------------

get_vegatation_raster <- function(from, to, band, region, scale = 1000) {
  
  # Conditions about the times
  start_year <- substr(from, 1, 4) %>% as.numeric()
  end_year <- substr(to, 1, 4) %>% as.numeric()
  
  # Factors by each bands
  
  multiply_factor <- c(
    NDVI = 0.0001, EVI = 0.0001, SAVI = 1
  )
  
  # Message of error
  
  if (end_year < 1999 | start_year >= 2021) {
    print(sprintf("No exist data"))
  }
  
  # NDVI - EVI
  
  if (band == "SAVI") {
    collection <- ee$ImageCollection("MODIS/006/MOD13A1")$
      select(c("sur_refl_b01", "sur_refl_b02", "DetailedQA"))
  } else {
    collection <- ee$ImageCollection("MODIS/006/MOD13A1")$
      select(c(band, "DetailedQA"))
  }
  
  # filter quality
  bitwiseExtract <- function(value, fromBit, toBit = fromBit) {
    maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
    mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
    final <- value$rightShift(fromBit)$bitwiseAnd(mask)
    return(final)
  }
  
  if (band == "SAVI") {
    filteApply <- function(image) {
      qa <- image$select("DetailedQA")
      ndvi <- image$select(c("sur_refl_b01", "sur_refl_b02"))
      # build filter
      filter1 <- bitwiseExtract(qa, 0, 1)
      filter2 <- bitwiseExtract(qa, 15)
      filter3 <- bitwiseExtract(qa, 14)
      # build mask
      mask <- filter1$neq(2)$And(filter2$neq(1))$And(filter3$neq(1))
      # apply mas
      ndvi$updateMask(mask) %>% return()
    }
  } else {
    filteApply <- function(image) {
      qa <- image$select("DetailedQA")
      ndvi <- image$select(c(band))
      # build filter
      filter1 <- bitwiseExtract(qa, 0, 1)
      filter2 <- bitwiseExtract(qa, 15)
      filter3 <- bitwiseExtract(qa, 14)
      # build mask
      mask <- filter1$neq(2)$And(filter2$neq(1))$And(filter3$neq(1))
      # apply mas
      ndvi$updateMask(mask) %>% return()
    }
  }
  
  # savi index
  savi <- function(img) {
    index <- img$
      expression(
        "(1 + L) * float(nir - red)/ (nir + red + L)",
        list(
          "nir" = img$select("sur_refl_b02"),
          "red" = img$select("sur_refl_b01"),
          "L" = 0.5
        )
      )$rename("SAVI")
  }
  
  # date of dataset
  months <- ee$List$sequence(1, 12)
  years <- ee$List$sequence(start_year, end_year)
  
  if (band == "SAVI") {
    modis <- ee$
      ImageCollection$
      fromImages(years$map(
        ee_utils_pyfunc(function(y) {
          months$map(ee_utils_pyfunc(
            function(m) {
              collection$
                filter(ee$Filter$calendarRange(y, y, "year"))$
                filter(ee$Filter$calendarRange(m, m, "month"))$
                map(filteApply)$
                map(savi)$
                max()$
                set("year", y)$
                set("month", m)
            }
          ))
        })
      )$flatten())
  } else {
    modis <- ee$
      ImageCollection$
      fromImages(years$map(
        ee_utils_pyfunc(function(y) {
          months$map(ee_utils_pyfunc(
            function(m) {
              collection$
                filter(ee$Filter$calendarRange(y, y, "year"))$
                filter(ee$Filter$calendarRange(m, m, "month"))$
                map(filteApply)$
                max()$
                set("year", y)$
                set("month", m)
            }
          ))
        })
      )$flatten())
  }
  
  im_base <- modis$
    filter(ee$Filter$inList("month", c(1:12)))
  
  if (start_year == end_year) {
    new_base <- im_base$
      filter(
        ee$Filter$inList(
          "year",
          list(
            c(
              start_year:end_year
            )
          )
        )
      )$toBands()$
      multiply(
        multiply_factor[[band]]
      )
  } else {
    new_base <- im_base$
      filter(
        ee$Filter$inList(
          "year",
          c(
            start_year:end_year
          )
        )
      )$
      toBands()$
      multiply(
        multiply_factor[[band]]
      )
    return(newbase)
  }
}

ndvi <- get_vegatation_raster(
  from = "2019-01-01",
  to = "2019-12-31",
  band = "NDVI",
  region = region_ee,
  scale = 1000)

evi <- get_vegatation_raster(
  from = "2019-01-01",
  to = "2019-12-31",
  band = "EVI",
  region = region_ee,
  scale = 1000)

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
  image = temp_min,
  region = region_ee,
  dsn = "rasters/temp_min_2019.tif",
  scale = 1000
)

ee_as_raster(
  image = ndvi,
  region = region_ee,
  dsn = "rasters/ndvi_2019.tif",
  scale = 1000
)

ee_as_raster(
  image = evi,
  region = region_ee,
  dsn = "rasters/evi_2019.tif",
  scale = 1000
)