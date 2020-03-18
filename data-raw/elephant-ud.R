# prepare elephant_ud example dataset
# african elephant utilization distribution
# 500 m resolution, 1 year, elephants LA11 and LA14

library(raster)
library(sf)
library(adehabitatHR)
library(lubridate)
library(tidyverse)

output_res <- 500

# load locs from gps tracking
# focus on single elephant for four study years
locs <- read_sf("data-raw/elephant-tracks.gpkg") %>%
  mutate(timestamp = ymd_hms(timestamp),
         year = year(timestamp),
         month = month(timestamp)) %>%
  filter(ind_ident %in% c("LA14", "LA11"),
         year == 2011) %>%
  group_by(ind_ident) %>%
  mutate(start = min(timestamp),
         end = max(timestamp)) %>%
  ungroup()

# calculate longitude for centering projection
lng_center <- mean(st_coordinates(locs)[, 1])
moll <- str_glue("+proj=moll +lon_0={lng_center} +x_0=0 +y_0=0 +ellps=WGS84")
# project locs
locs_moll <- st_transform(locs, crs = moll)
# make template for utilization distribution
template <- extent(locs_moll) %>%
  raster(resolution = output_res) %>%
  extend(20)

# generate annual trajectories and utilization distributions
make_trajectory <- function(x) {
  xy <- data.frame(sf::st_coordinates(x))
  adehabitatLT::as.ltraj(xy,
                         date = x$timestamp,
                         id = as.character(x$tag_ident),
                         typeII = TRUE,
                         proj4string = sp::CRS(sf::st_crs(x)$proj4string))
}
make_ud <- function(x, grid) {
  vv <- BRB.D(x,
              # max duration (s) allowed b/t successive relocations
              Tmax = 6 * 60 * 60,
              # min distance (m) allowed b/t successive relocations
              Lmin = 300)
  ud <- BRB(x,
            D = vv,
            # max duration (s) allowed b/t successive relocations
            Tmax = 6 * 60 * 60,
            # min distance (m) allowed b/t successive relocations
            Lmin = 300,
            # interpolation time
            tau = 60,
            # min smoothing parameter (m) applied to relocations
            hmin = 200,
            grid = as(grid, "SpatialPixels"),
            b = 0,
            same4all = FALSE,
            extent = 0.5)
  values <- data.frame(ud@coords, ud@data$dens)
  ud_raster <- rasterFromXYZ(values, crs = attr(x, "proj4string"))
  ud_raster / cellStats(ud_raster, sum)
}
ud_ind <- locs_moll %>%
  group_by(ind_ident) %>%
  nest() %>%
  mutate(trajectory = map(data, make_trajectory),
         ud = map(trajectory, make_ud, grid = template))
elephant_ud <- stack(ud_ind$ud)

# trim out very low values
elephant_ud <- elephant_ud * (elephant_ud > 1e-6)
elephant_ud <- trim(elephant_ud, values = c(0, NA))
elephant_ud <- elephant_ud / cellStats(elephant_ud, sum)

# save package data
elephant_ud <- stack(elephant_ud)
elephant_ud <- readAll(elephant_ud)
names(elephant_ud) <- ud_ind$ind_ident
writeRaster(elephant_ud, "data-raw/elephant-ud.tif", overwrite = TRUE)
usethis::use_data(elephant_ud, overwrite = TRUE)
