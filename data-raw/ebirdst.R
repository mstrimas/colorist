library(raster)
library(ebirdst)

# download and load field sparrow s&t data
dl_path <- ebirdst_download("Field Sparrow")
map_pars <- load_fac_map_parameters(dl_path)
occ <- load_raster("occurrence", dl_path)

# aggregate
gdal_aggregate <- function(f_in, f_out, fact, crs, ext) {
  # output resolution
  tr <- paste(fact * raster::res(raster::raster(f_in)), collapse = " ")
  tr <- paste("-tr", tr)
  # output crs
  if (missing(crs)) {
    proj <- ""
  } else {
    proj <- sprintf("-t_srs '%s'", crs)
  }
  # output extent
  if (missing(ext)) {
    e <- ""
  } else {
    e <- paste("-te", ext@xmin, ext@ymin, ext@xmax, ext@ymax)
  }
  cmd <- sprintf("gdalwarp %s -r average %s %s -co 'COMPRESS=LZW' '%s' '%s'",
                 tr, proj, e, f_in, f_out)
  system(cmd)
  raster::stack(f_out)
}
f <- "data-raw/fiespa_lr_2018_occurrence_median.tif"
unlink(f)
fiespa_occ <- gdal_aggregate(occ[[1]]@file@name, f,
                             fact = 9,
                             crs = map_pars$custom_projection,
                             ext = map_pars$fa_extent)
names(fiespa_occ) <- names(occ)

# save package data
usethis::use_data(fiespa_occ, overwrite = TRUE)
