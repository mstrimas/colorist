#' Transform raster stack values to intensity values
#'
#' @description This function transforms raster stack values that describe
#'   individual distributions or species distributions into standardized
#'   intensity values. All the distributional information in the original raster
#'   stack is preserved for visualization.
#'
#' @param x RasterStack of distributions. Layers typically contain information
#'   about the distribution of a single individual or species at multiple points
#'   in time. Alternatively, layers may contain information about the
#'   distributions of multiple individuals or species within a single time
#'   period. Other conceptualizations are possible.
#'
#' @return A RasterStack containing intensity values. Intensity values are
#'   calculated by dividing cell values in every layer by the maximum cell value
#'   in the entire stack, thus ensuring intensities are comparable across
#'   layers.
#'
#'   The maximum cell value in the stack is stored as the `"maximum"` attribute.
#'
#' @family metrics
#' @export
#' @examples
#' # load elephant data
#' data("elephant_ud")
#' r <- metrics_pull(elephant_ud)
#' print(r)
#' # maximum value for the stack stored as an attribute
#' attr(r, "maximum")
metrics_pull <- function(x) {
  stopifnot(class(x) %in% c("RasterStack", "RasterBrick"))

  # scale layers by the maximum value in the stack
  maximum <- raster::cellStats(x, "max")
  if (any(is.na(maximum)) && !all(is.finite(maximum))) {
    stop("All layers must have at least one non-NA value.")
  }
  r <- x / max(maximum)

  # set attributes
  attr(r, "metric") <- "pull"
  attr(r, "maximum") <- max(maximum)

  return(r)
}

#' Distill a raster stack into a set of distribution metrics
#'
#' @description This function is used to summarize several distributional
#'   features of interest across a series of distributions. Distributional
#'   information in the original raster stack is "distilled" for subsequent
#'   visualization.
#'
#' @param x RasterStack of distributions. Layers typically contain information
#'   about the distribution of a single individual or species at multiple points
#'   in time. Alternatively, layers may contain information about the
#'   distributions of multiple individuals or species within a single time
#'   period. Other conceptualizations are possible.
#'
#' @details Specificity values range from 0 to 100. Values of 0 indicate
#'   intensity values are identical in all layers. Values of 100 indicate
#'   intensity values are restricted to a single layer. Interpretation of
#'   specificity values depends on the layers provided. If layers describe the
#'   distribution of a species at different times of the year, specificity can
#'   be interpreted as a measure of seasonality (i.e., 0 = stable year-round
#'   occurrence in a cell, 100 = highly seasonal occurrence). If layers describe
#'   space use by multiple individuals, specificity can be interpreted as a
#'   measure of exclusivity (i.e., 0 = equal use of a cell by all individuals,
#'   100 = exclusive use by one individual).
#'
#'   The number of layers with non-NA values is recorded to aid interpretation
#'   of distributions. Ideally, n_layers values are identical in every cell,
#'   indicating that users have knowledge of distributions over the same area in
#'   every layer of their raster stack. When n_layers values are unequal, it
#'   indicates that users have unequal knowledge of distributions in their
#'   raster stack. Distributions are more likely to be misrepresented and
#'   misinterpreted if cells do not contain intensity values in every layer.
#'
#' @return A RasterStack with four layers:
#'   - `intensity`: the maximum intensity value across all layers.
#'   - `layer_id`: an integer identifying layer containing the maximum intensity
#'   value.
#'   - `specificity`: the degree to which intensity values are unevenly
#'   distributed across layers (see Details).
#'   - `n_layers`: the number of layers with non-NA values (see Details).
#'
#'   The maximum cell value in the stack is stored as the `"maximum"` attribute.
#'   The link between the `layer_id` and the layer names from the underlying
#'   raster is stored as a data frame in the `layer_names` attribute.
#'
#' @family metrics
#' @export
#' @examples
#' # load elephant data
#' data("elephant_ud")
#'
#' # distill
#' r <- metrics_distill(elephant_ud)
#' print(r)
#'
#' # maximum value across all layers stored as an attribute
#' attr(r, "maximum")
#' # link between layer id and name stored as an attribute
#' attr(r, "layer_names")
metrics_distill <- function(x) {
  stopifnot(class(x) %in% c("RasterStack", "RasterBrick"))

  # scale layers by overall maximum
  maximum <- raster::cellStats(x, "max", na.rm = TRUE)
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
    #r_mean <- raster::calc(intensity, mean, na.rm = TRUE)
    r_peak <- raster::calc(intensity, which_max)
    r_specificity <- raster::calc(intensity, specificity)
  })
  r <- raster::stack(r_max, r_peak, r_specificity, r_n)
  names(r) <- c("intensity", "layer_id", "specificity", "n_layers")

  # set attributes
  attr(r, "metric") <- "distill"
  attr(r, "maximum") <- maximum
  print(x)
  attr(r, "layer_names") <- data.frame(layer_id = seq_len(raster::nlayers(x)),
                                       layer_name = names(x),
                                       stringsAsFactors = FALSE)
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
