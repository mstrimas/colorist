#' Genreate a stack of HCL color wheels
#'
#' @param x RasterStack or integer giving the number of intervals.
#'
#' @return A data frame with the following columns:
#'   - `seasonality`: the seasonality, which is mapped to chroma
#'   - `interval`: the time interval, which is mapped to hue
#'   - `color`: the hex color associated with the given seasonality and interval
#' @export
#' @examples
#' data(elephant_ud)
#' pal <- make_hcl_palette(elephant_ud)
#' head(pal)
make_hcl_palette <- function(x) {
  UseMethod("make_hcl_palette")
}

make_hcl_palette.integer <- function(x) {
  if (x < 2) {
    stop("At least two intervals are required to generate a palette.")
  }
  # start the palette at blue
  start <- round(0.67 * x, 0)
  interval <- c(start:1, x:(start + 1))
  wheel <- data.frame(seasonality = rep(0:100, each = x),
                      interval = rep(interval, times = 101),
                      color = NA_character_,
                      stringsAsFactors = FALSE)
  # generate palettes for each chroma value
  for (i in 0:100) {
    idx <- seq(i * length(interval) + 1, (i + 1) * length(interval), by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(x,
                                                     c = i,
                                                     l = 45,
                                                     start = 0,
                                                     end = 360 * ((x - 1)  / x),
                                                     fixup = TRUE)
  }
  return(wheel[order(-wheel[["seasonality"]], wheel[["interval"]]), ])
}

#' @export
make_hcl_palette.numeric <- function(x) {
  make_hcl_palette(as.integer(x))
}

#' @export
make_hcl_palette.Raster<- function(x) {
  make_hcl_palette(raster::nlayers(x))
}
