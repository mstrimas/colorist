library(sf)
library(adehabitatHR)
library(lubridate)
library(tidyverse)

# load locs from gps tracking
# focus on single elephant for four study yers
locs <- read_sf("data-raw/elephant-tracks.gpkg") %>%
  mutate(timestamp = ymd_hms(timestamp),
         year = year(timestamp),
         month = month(timestamp)) %>%
  filter(ind_ident == "LA14",
         timestamp > "2009-08-01",
         timestamp < "2013-08-01") %>%
  mutate(study_day = difftime(timestamp, min(timestamp), units = "days"),
         study_year = case_when(study_day < 366 ~ 1,
                                study_day < 731 ~ 2,
                                study_day < 1096 ~ 3,
                                TRUE ~ 4))


# calculate longitude for centering projection
lng_center <- mean(st_coordinates(locs)[, 1])
moll <- str_c("+proj=moll +lon_0={lng_center} +x_0=0 +y_0=0 +ellps=WGS84")
# project locs
locs_moll <- st_transform(locs, crs = moll)
# make template for utilization distribution
template <- raster::extent(locs_moll) %>%
  raster::raster(resolution = 500) %>%
  raster::extend(5)

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
  vv <- adehabitatHR::BRB.D(x, Tmax = 180 * 60, Lmin = 500)
  ud <- adehabitatHR::BRB(x,
                          D = vv,
                          Tmax = 180 * 60,
                          tau = 50,
                          Lmin = 500,
                          hmin = 100,
                          grid = as(grid, "SpatialPixels"),
                          b = 0,
                          same4all = FALSE,
                          extent = 0.5)
  values <- data.frame(ud@coords, ud@data$dens)
  ud_raster <- raster::rasterFromXYZ(values, crs = attr(x, "proj4string"))
  ud_raster / raster::cellStats(ud_raster, sum)
}
ud_year <- locs_moll %>%
  group_by(study_year) %>%
  nest() %>%
  mutate(trajectory = map(data, make_trajectory),
         ud = map(trajectory, make_ud, grid = template))
elephant_ud <- raster::stack(ud_year$ud) %>%
  raster::trim(values = NA)

# trim out very low values
elephant_ud <- elephant_ud * (elephant_ud > 1e-6)
elephant_ud <- elephant_ud / raster::cellStats(elephant_ud, sum)

# save package data
names(elephant_ud) <- paste0("year", ud_year$study_year)
elephant_ud <- raster::stack(elephant_ud)
usethis::use_data(elephant_ud, overwrite = TRUE)
