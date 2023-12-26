library(sf)
library(mapview)
library(spData)
library(ggplot2)
library(terra)
library(dplyr)
library(rnaturalearth)


d <- st_read(system.file("shapes/columbus.shp",
                         package = "spData"), quiet = TRUE)
ggplot(d) + geom_sf(aes(fill = INC))


d <- st_read(system.file("shape/nc.shp", package = "sf"),
             quiet = TRUE)
mapview(d, zcol = "SID74")


d <- rast(system.file("ex/elev.tif", package = "terra"))
plot(d)


###### 2) Spatial data in R

library(sf)
pathshp <- system.file("shape/nc.shp", package = "sf")
map <- st_read(pathshp, quiet = TRUE)
class(map)

plot(map[1]) # plot first attribute


library(sf)
pathshp <- system.file("shape/nc.shp", package = "sf")
map <- st_read(pathshp, quiet = TRUE)


###### CRS


# Get CRS
st_crs(map)
# Transform CRS
map2 <- st_transform(map, crs = "EPSG:4326")
# Get CRS
# st_crs(map2)

plot(map[1])

library(terra)
pathraster <- system.file("ex/elev.tif", package = "terra")
r <- rast(pathraster)

# Get CRS
# crs(r)
# Transform CRS
r2 <- terra::project(r, "EPSG:2169")
# Get CRS
# crs(r2)


####### Raster

library(terra)
pathraster <- system.file("ex/elev.tif", package = "terra")
r <- terra::rast(pathraster)

plot(r)

r <- rast(ncol = 10, nrow = 10,
          xmin = -150, xmax = -80, ymin = 20, ymax = 60)

values(r) <- 1:ncell(r)

r2 <- r * r
s <- c(r, r2)

plot(s[[2]]) # layer 2

plot(min(s))
plot(r + r + 10)
plot(round(r))
plot(r == 1)


pathshp <- system.file("ex/lux.shp", package = "terra")
v <- vect(pathshp)

plot(v)

# Longitude and latitude values
long <- c(-0.118092, 2.349014, -3.703339, 12.496366)
lat <- c(51.509865, 48.864716, 40.416729, 41.902782)
longlat <- cbind(long, lat)

# CRS
crspoints <- "+proj=longlat +datum=WGS84"

# Attributes for points
d <- data.frame(
  place = c("London", "Paris", "Madrid", "Rome"),
  value = c(200, 300, 400, 500))

# SpatVector object
pts <- vect(longlat, atts = d, crs = crspoints)

pts
plot(pts)

##### GEODATA PERU TEMPERATURE 

r <- geodata::worldclim_country(country = "Spain", var = "tavg",
                                res = 10, path = tempdir())
plot(r)


r <- mean(r)
plot(r)


##### SPAIN MAP DOWNLOAD

# Map

map <- rnaturalearth::ne_states("Spain", returnclass = "sf")
map <- map[-which(map$region == "Canary Is."), ] # delete region
ggplot(map) + geom_sf()

# Cropping
sextent <- terra::ext(map)
r <- terra::crop(r, sextent)
plot(r)

# Masking
r <- terra::mask(r, vect(map))
plot(r)


# Aggregating
r <- terra::aggregate(r, fact = 20, fun = "mean", na.rm = TRUE)
plot(r)



##### RASTER VALUES 

library(terra)
# Raster (SpatRaster)
r <- rast(system.file("ex/elev.tif", package = "terra"))
# Polygons (SpatVector)
v <- vect(system.file("ex/lux.shp", package = "terra"))

points <- crds(centroids(v))

plot(r)
plot(v, add = TRUE)
points(points) 

# data frame with the coordinates
points <- as.data.frame(points)
valuesatpoints <- extract(r, points)
cbind(points, valuesatpoints)



##### EXTRACT AVERAGE RASTER VALUES

# Extracted raster cells within each polygon
head(extract(r, v, na.rm = TRUE))

# Extracted raster cells and percentage of area
# covered within each polygon
head(extract(r, v, na.rm = TRUE, weights = TRUE))

# Average raster values by polygon
v$avg <- extract(r, v, mean, na.rm = TRUE)$elevation

# Area-weighted average raster values by polygon (weights = TRUE)
v$weightedavg <- extract(r, v, mean, na.rm = TRUE,
                         weights = TRUE)$elevation

df_v <- data.frame(cbind(v$avg, v$weightedavg))

#### PLOT


library(ggplot2)
library(tidyterra)

# Plot average raster values within polygons
ggplot(data = v) + geom_spatvector(aes(fill = avg)) +
  scale_fill_terrain_c()

# Plot area-weighted average raster values within polygons
ggplot(data = v) + geom_spatvector(aes(fill = weightedavg)) +
  scale_fill_terrain_c()


################################################################################
################################################################################
################################################################################

# Obtener el mapa de Perú
map <- ne_countries(country = "peru", returnclass = "sf")

malnutrition <- malnutrition_2019 %>%
  select(malnutrition, longitudx, latitudy)

# Convertir a objeto sf
malnutrition_sf <- st_as_sf(malnutrition, coords = c("longitudx", "latitudy"), crs = st_crs(map))


# Encuentra los puntos que están dentro del mapa de Perú
indices <- st_intersects(malnutrition_sf, map, sparse = FALSE)
malnutrition_in_peru <- malnutrition_sf[indices[,1], ]

# Gráfico con solo los puntos dentro del territorio de Perú
ggplot() +
  geom_sf(data = map) +  # Dibuja el mapa
  geom_sf(data = malnutrition_in_peru, color = 'red', size = 0.5) +  # Superpone solo los puntos dentro de Perú
  theme_minimal() +
  labs(title = "Casos de Desnutrición en Perú 2019")


# Convertir los datos de desnutrición a un formato que ggplot pueda usar para stat_density2d
malnutrition_points <- as.data.frame(st_coordinates(malnutrition_in_peru))

# Gráfico
ggplot() +
  geom_sf(data = map) +  # Dibuja el mapa
  stat_density2d(data = malnutrition_points, 
                 aes(x = X, y = Y, fill = ..level..), 
                 geom = "polygon", alpha = 0.5) +  # Agrega la densidad de los puntos
  scale_fill_viridis_c() +  # Escala de colores para la densidad
  theme_minimal() +
  labs(title = "Densidad de Casos de Desnutrición en Perú")
