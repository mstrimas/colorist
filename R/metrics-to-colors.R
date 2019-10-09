#' Assign cell colors and alpha levels based on annual cycle metrics
#'
#' @param x RasterStack of seasonility metrics from [annual_cycle_metrics()]
#' @param palette data frame containing an HCL color palette as output from
#'   [make_hcl_palette()]
#'
#' @return A RasterStack with four layers, specifying the RGB color and alpha
#'   level, all on the 0-1 scale:
#'   - `r`: red channel
#'   - `g`: green channel
#'   - `b`: blue channel
#'   - `alpha`: alpha level
#' @export
#' @examples
#' data(elephant_ud)
#' # generate annual cyle metrics and a color palette based on sample data
#' r <- annual_cycle_metrics(elephant_ud)
#' pal <- make_hcl_palette(elephant_ud)
#' # assign colors
#' rgb <- metrics_to_colors(r, pal)
#' raster::plotRGB(rgb, scale = 1)
metrics_to_colors <- function(x, palette) {
  stopifnot(inherits(x, "Raster"))
  stopifnot(all(c("max", "peak_interval", "seasonality") %in% names(x)))
  stopifnot(inherits(palette, "data.frame"))
  stopifnot(all(c("seasonality", "interval", "color") %in% names(palette)))

  # raster template
  r <- raster::raster(x)

  # assign color based on peak_interval and seasonality
  x_df <- raster::as.data.frame(x[[c("peak_interval", "seasonality", "max")]])
  names(x_df) <- c("interval", "seasonality", "max")
  x_df[["cell"]] <- seq.int(nrow(x_df))
  x_df <- na.omit(x_df)
  x_df[["seasonality"]] <- round(100 * x_df[["seasonality"]], digits = 0)
  x_df <- merge(x_df, palette, by = c("interval", "seasonality"),
                all.x = TRUE, all.y = FALSE)
  # convert hex color to rgb
  srgb <- colorspace::hex2RGB(x_df[["color"]])@coords

  # alpha level based on max value
  x_df[["alpha"]] <- x_df[["max"]] / max(x_df[["max"]], na.rm = TRUE)

  # assign to raster stack
  s <- raster::stack(set_cell_values(r, srgb[, "R"], x_df[["cell"]]),
                     set_cell_values(r, srgb[, "G"], x_df[["cell"]]),
                     set_cell_values(r, srgb[, "B"], x_df[["cell"]]),
                     set_cell_values(r, x_df[["alpha"]], x_df[["cell"]]))
  names(s) <- c("r", "g", "b", "alpha")
  return(s)
}

set_cell_values <- function(x, values, cells) {
  x <- raster::raster(x)
  x[cells] <- values
  return(x)
}
