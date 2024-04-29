# prepare fisher_ud example dataset
# ny fisher utilization distribution
# 25 m resolution, 9 days in april 2011

library(raster)
library(sf)
library(adehabitatHR)
library(lubridate)
library(suncalc)
library(tidyverse)

output_res <- 25

# load locs from gps tracking
# focus on single fisher in 2011
locs <- read_sf("data-raw/fisher-data/points.shp") %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "GMT"),
         timestamp = with_tz(timestamp, tz = "America/New_York"),
         date = as_date(timestamp),
         year = year(timestamp),
         month = month(timestamp),
         hour = hour(timestamp)) %>%
  filter(ind_ident == "M5",
         year == 2011,
         yday(date) >= 97,
         yday(date) <= 105) %>%
  mutate(start = min(timestamp),
         end = max(timestamp))

# add sunrise/set, filter to noctural obs
locs_noct <- locs %>%
  st_drop_geometry() %>%
  distinct(date) %>%
  mutate(suninfo = map(date, getSunlightTimes,
                       lat = 42.82711, lon = -73.40421,
                       keep = c("sunrise", "sunset"),
                       tz = "America/New_York"),
         suninfo = map(suninfo, select, sunrise, sunset)) %>%
  unnest(cols = suninfo) %>%
  inner_join(locs, ., by = "date") %>%
  filter(timestamp <= sunrise | timestamp >= sunset)

# calculate longitude for centering projection
lng_center <- mean(st_coordinates(locs_noct)[, 1])
moll <- str_glue("+proj=moll +lon_0={lng_center} +x_0=0 +y_0=0 +ellps=WGS84")
# project locs
locs_moll <- st_transform(locs_noct, crs = moll)
# make template for utilization distribution
template <- extent(locs_moll) %>%
  raster(resolution = output_res) %>%
  extend(75)

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
              Tmax = 180 * 60,
              # min distance (m) allowed b/t successive relocations
              Lmin = 5)
  ud <- BRB(x,
            D = vv,
            # max duration (s) allowed b/t successive relocations
            Tmax = 180 * 60,
            # min distance (m) allowed b/t successive relocations
            Lmin = 5,
            # interpolation time
            tau = 10,
            # min smoothing parameter (m) applied to relocations
            hmin = 75,
            grid = as(grid, "SpatialPixels"),
            b = 0,
            same4all = FALSE,
            extent = 0.5)
  values <- data.frame(ud@coords, ud@data$dens)
  ud_raster <- rasterFromXYZ(values, crs = attr(x, "proj4string"))
  ud_raster / cellStats(ud_raster, sum)
}
ud_date <- locs_moll %>%
  group_by(date) %>%
  nest() %>%
  mutate(trajectory = map(data, make_trajectory),
         ud = map(trajectory, make_ud, grid = template))
fisher_ud <- stack(ud_date$ud)

# trim out very low values
fisher_ud <- fisher_ud * (fisher_ud > 1e-6)
fisher_ud <- trim(fisher_ud, values = c(0, NA))
fisher_ud <- fisher_ud / cellStats(fisher_ud, sum)

# save package data
fisher_ud <- stack(fisher_ud)
fisher_ud <- readAll(fisher_ud)
names(fisher_ud) <- paste0("night", seq_len(nlayers(fisher_ud)))
writeRaster(fisher_ud, "data-raw/fisher-ud.tif", overwrite = TRUE)
usethis::use_data(fisher_ud, overwrite = TRUE)
