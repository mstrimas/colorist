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
f <- "data-raw/fiespa_lr_wk_2018_occurrence_median.tif"
unlink(f)
# tight extent
e <- extent(c(xmin = -1482551, xmax = 1843348,
              ymin = -1456169, ymax = 1405830))
fiespa_occ_wk <- gdal_aggregate(occ[[1]]@file@name, f,
                                fact = 5,
                                crs = map_pars$custom_projection,
                                ext = e)
names(fiespa_occ_wk) <- names(occ)

# monthly mean
f <- "data-raw/fiespa_lr_mth_2018_occurrence_median.tif"
mths <- lubridate::month(parse_raster_dates(fiespa_occ_wk))
fiespa_occ_mth <- stackApply(fiespa_occ_wk, indices = mths, fun = mean)
names(fiespa_occ_mth) <- tolower(month.abb)
fiespa_occ_mth <- stack(fiespa_occ_mth)

# save package data
fiespa_occ <- readAll(fiespa_occ_mth)
usethis::use_data(fiespa_occ, overwrite = TRUE)


