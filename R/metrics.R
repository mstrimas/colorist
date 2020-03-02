#' Transform a raster stack to intensity values
#'
#' @param x RasterStack with each layer typically corresponding to the
#'   distribution of an individual or species at a given time interval.
#'
#' @return A RasterStack resulting from converting the input stack to intensity
#'   values by dividing each layer by the maximum pixel value in that layer.
#'   These layer maxima are stored as the `"maximum"` attribute.
#'
#' @family metrics
#' @export
#' @examples
#' # load elephant data
#' data("elephant_ud")
#' r <- metrics_pull(elephant_ud)
#' print(r)
#' # maximum values for each layer stored as an attribute
#' attr(r, "maximum")
metrics_pull <- function(x) {
  stopifnot(class(x) %in% c("RasterStack", "RasterBrick"))

  # scale layers by their maximum
  maximum <- raster::cellStats(x, "max")
  if (any(is.na(maximum)) && !all(is.finite(maximum))) {
    stop("All layers must have at least one non-NA value.")
  }
  r <- x / maximum

  # set attributes
  attr(r, "metric") <- "pull"
  attr(r, "maximum") <- maximum

  return(r)
}

#' Distill a raster stack into a set of metrics for each cells
#'
#' @param x RasterStack with each layer typically corresponding to the
#'   distribution of an individual or species at a given time interval.
#'
#' @return A RasterStack with four layers:
#'   - `intensity`: the maximum pixel intensity across all layers
#'   - `intensity_mean`: the mean pixel intensity across all layers
#'   - `layer`: the layer number associated with the maximum intensity.
#'   - `specificity`: the amount of variation between layers for each cell, this
#'   is a measure of seasonality if the layers represents different times of
#'   year.
#'   - `n_layers`: the number of intervals with a non-NA pixel value
#'
#'   Here intensity is the result of dividing all cell values by the maximum
#'   cell value across all layers. This maximum value is stored as the
#'   `"maximum"` attribute.
#'
#' @family metrics
#' @export
#' @examples
#' # load elephant data
#' data("elephant_ud")
#' r <- metrics_distill(elephant_ud)
#' print(r)
#' # maximum value across all layers stored as an attribute
#' attr(r, "maximum")
metrics_distill <- function(x) {
  stopifnot(class(x) %in% c("RasterStack", "RasterBrick"))

  # scale layers by overall maximum
  maximum <- raster::cellStats(x, "max")
  if (any(is.na(maximum)) && !all(is.finite(maximum))) {
    stop("All layers must have at least one non-NA value.")
  }
  maximum <- max(maximum, na.rm = TRUE)
  intensity <- x / maximum

  # calculate metrics
  which_max <- function(x, ...) {
    max_idx <- which.max(x)
    max_idx <- ifelse(length(max_idx) == 0, NA, max_idx)
    return(max_idx)
  }
  suppressWarnings({
    r_n <- raster::calc(intensity, length_nona)
    r_max <- raster::calc(intensity, max, na.rm = TRUE)
    r_mean <- raster::calc(intensity, mean, na.rm = TRUE)
    r_peak <- raster::calc(intensity, which_max)
    r_specificity <- raster::calc(intensity, specificity)
  })
  r <- raster::stack(r_max, r_mean, r_peak, r_specificity, r_n)
  names(r) <- c("intensity", "intensity_mean",
                "layer", "specificity", "n_layers")

  # set attributes
  attr(r, "metric") <- "distill"
  attr(r, "maximum") <- maximum

  return(r)
}

length_nona <- function(x) {
  length(stats::na.omit(x))
}

specificity <- function(x) {
  sum_x <- (length(x) - 1) * mean(x, na.rm = TRUE)
  if (!is.finite(sum_x) || sum_x == 0) {
    return(NA)
  }
  residuals <- abs(x - mean(x, na.rm = TRUE))
  round(50 * sum(residuals, na.rm = TRUE) / sum_x)
}
