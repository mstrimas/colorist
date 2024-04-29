# prepare fiesp_occ example dataset
# ebirdst occurrence data for field sparrow
# 15 km, monthly resolution

library(raster)
library(ebirdst)

# download and load field sparrow s&t data
ebirdst_download_status("fiespa", pattern = "occurrence_median_3km",
                        download_occurrence = TRUE)
map_pars <- load_fac_map_parameters("fiespa")
occ <- load_raster("fiespa", product = "occurrence") |>
  stack()

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
f_wk <- "data-raw/fiespa_occurrence_median_27km_2022.tif"
unlink(f_wk)
# tight extent
e <- extent(c(xmin = -1782551, xmax = 1443348,
              ymin = -1156169, ymax = 1505830))
fiespa_occ_wk <- gdal_aggregate(occ[[1]]@file@name, f_wk,
                                fact = 9,
                                crs = map_pars$custom_projection,
                                ext = e)
names(fiespa_occ_wk) <- names(occ)

# monthly mean
f_mth <- "data-raw/fiespa-occ.tif"
mths <- lubridate::month(as.Date(names(fiespa_occ_wk), format = "X%Y.%m.%d"))
fiespa_occ_mth <- stackApply(fiespa_occ_wk, indices = mths, fun = mean,
                             filename = f_mth, overwrite = TRUE)
names(fiespa_occ_mth) <- tolower(month.abb)
fiespa_occ_mth <- stack(fiespa_occ_mth)

# save package data
unlink(f_wk)
fiespa_occ <- readAll(fiespa_occ_mth)
usethis::use_data(fiespa_occ, overwrite = TRUE)
