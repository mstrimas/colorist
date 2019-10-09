#' Calculate annual cycle metrics for a raster time series
#'
#' @param x RasterStack with each layer corresponding to a time interval
#'
#' @return A RasterStack with four layers:
#'   - `max`: the maximum pixel value across all time intervals
#'   - `peak_interval`: the interval number associated with the maximum
#'   - `seasonality`: the seasonality or unevenness
#'   - `n_intervals`: the number of intervals with a non-NA pixel value
#' @export
#' @examples
#' data(elephant_ud)
#' r <- annual_cycle_metrics(elephant_ud)
annual_cycle_metrics <- function(x) {
  calc_unevenness <- function(x) {
    sum_x <- (length(x) - 1) * mean(x)
    if (sum_x == 0) {
      return(NA)
    }
    residuals <- abs(x - mean(x, na.rm = TRUE))
    0.5 * sum(residuals) / sum_x
  }
  calc_intervals <- function(x) {
    length(na.omit(x))
  }
  r_n <- raster::calc(elephant_ud, calc_intervals)
  r_max <- raster::calc(elephant_ud, max, na.rm = TRUE)
  r_peak <- raster::calc(elephant_ud, which.max)
  r_uneven <- raster::calc(elephant_ud, calc_unevenness)
  r <- raster::stack(r_max, r_peak, r_uneven, r_n)
  r <- raster::mask(r, r_max, maskvalue = 0)
  names(r) <- c("max", "peak_interval", "seasonality", "n_intervals")
  return(r)
}
