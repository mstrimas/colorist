library(colorist)
library(sf)

buildings <- read_sf("data-raw/rensselaer-buildings.gpkg") %>%
  st_transform(crs = st_crs(fisher_ud)) %>%
  st_crop(st_bbox(fisher_ud))
saveRDS(buildings, "data-raw/buildings.rds")
usethis::use_data(buildings, overwrite = TRUE)
