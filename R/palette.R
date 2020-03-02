# palette_timecycle ----

#' Make HCL palette for a cyclical sequence of distributions
#'
#' @param x RasterStack or integer giving the number of layers to build the
#'   color palette for.
#'
#' @return A data frame with the following columns:
#'   - `specificity`: amount of variation between layers; mapped to chroma.
#'   - `layer`: layer with the maximum cell value; mapped to hue.
#'   - `color`: color associated with the given specificity and peak layer.
#' @family palette
#' @seealso [palette_timeline] for linear sequences of distributions and
#'   [palette_groups] for distributions of distinct groups.
#' @export
#' @examples
#' # load field sparrow data
#' data(fiespa_occ)
#'
#' # generate hcl color palette
#' pal <- palette_timecycle(fiespa_occ)
#' head(pal)
#'
#' # visualize the palette in HCL space with colorspace::hclplot
#' library(colorspace)
#' hclplot(pal[pal$specificity == 100, ]$color)
palette_timecycle <- function(x) {
  UseMethod("palette_timecycle")
}

#' @export
palette_timecycle.integer <- function(x) {
  if (x < 2) {
    stop("At least two intervals are required to generate a palette.")
  }
  # start the palette at blue
  start <- round(0.67 * x, 0)
  layer <- c(start:1, x:(start + 1))
  wheel <- data.frame(specificity = rep(0:100, each = x),
                      layer = rep(layer, times = 101),
                      color = NA_character_,
                      stringsAsFactors = FALSE)
  # generate palettes for each chroma value
  for (i in 0:100) {
    idx <- seq(i * x + 1, (i + 1) * x, by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(x,
                                                     c = i,
                                                     l = 45,
                                                     start = 0,
                                                     end = 360 * ((x - 1)  / x),
                                                     fixup = TRUE)
  }
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer"]]), ]
  row.names(wheel) <- NULL
  class(wheel) <- c("palette_timecycle", "data.frame")
  return(wheel)
}

#' @export
palette_timecycle.numeric <- function(x) {
  palette_timecycle(as.integer(x))
}

#' @export
palette_timecycle.Raster<- function(x) {
  palette_timecycle(raster::nlayers(x))
}


# palette_timeline ----

#' Make HCL palette for a linear sequence of distributions
#'
#' @param x RasterStack or integer giving the number of layers to build the
#'   color palette for.
#' @param start_hue integer between -360 and 360 representing the starting hue
#'   in the color wheel. The ending hue will be `start_hue` + 180. For
#'   further details, consult the documentation for [colorspace::rainbow_hcl].
#'   Recommended values are -130, giving a blue-pink-yellow palette, and 50,
#'   giving a yellow-green-blue palette.
#'
#' @return A data frame with the following columns:
#'   - `specificity`: amount of variation between layers; mapped to chroma.
#'   - `layer`: layer with the maximum cell value; mapped to hue.
#'   - `color`: color associated with the given specificity and peak layer.
#' @family palette
#' @seealso [palette_timecycle] for cyclical sequences of distributions and
#'   [palette_groups] for distributions of distinct groups.
#' @export
#' @examples
#' # load elephant data
#' data(elephant_ud)
#'
#' # generate hcl color palette
#' pal_a <- palette_timeline(elephant_ud)
#' head(pal_a)
#'
#' # try a different starting hue
#' pal_b <- palette_timeline(elephant_ud, start = 50)
#'
#' # visualize the palette in HCL space  with colorspace::hclplot
#' library(colorspace)
#' hclplot(pal_a[pal_a$specificity == 100, ]$color)
#' hclplot(pal_b[pal_b$specificity == 100, ]$color)
palette_timeline <- function(x, start_hue = -130) {
  UseMethod("palette_timeline")
}

#' @export
palette_timeline.integer <- function(x, start_hue = -130) {
  if (x < 2) {
    stop("At least two intervals are required to generate a palette.")
  }
  stopifnot(length(start_hue) == 1, start_hue >= -360, start_hue <= 360)

  # set up color wheel
  wheel <- expand.grid(specificity = 0:100,
                       layer = seq_len(x),
                       color = NA_character_,
                       stringsAsFactors = FALSE)
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer"]]), ]
  row.names(wheel) <- NULL

  # generate palettes for each chroma value
  for (i in 0:100) {
    idx <- seq(i * x + 1, (i + 1) * x, by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(x,
                                                     c = i,
                                                     l = 45,
                                                     start = start_hue,
                                                     end = start_hue + 180,
                                                     fixup = TRUE)
  }
  class(wheel) <- c("palette_timecycle", "data.frame")
  return(wheel)
}

#' @export
palette_timeline.numeric <- function(x, start_hue = -130) {
  palette_timeline(as.integer(x), start_hue)
}

#' @export
palette_timeline.Raster<- function(x, start_hue = -130) {
  palette_timeline(raster::nlayers(x), start_hue)
}


# palette_groups ----

#' Make HCL palette for the distributions of distinct groups
#'
#' @param x RasterStack or integer giving the number of layers to build the
#'   color palette for.
#'
#' @return A data frame with the following columns:
#'   - `specificity`: seasonality; mapped to chroma.
#'   - `layer`: layer number for each group; mapped to hue.
#'   - `color`: color associated with the given specificity and peak layer.
#' @family palette
#' @seealso [palette_timecycle] for cyclical sequences of distributions and
#'   [palette_timeline] for linear sequences of distributions.
#' @export
#' @examples
#' # load elephant data
#' # treat layers as groups of elephants, not years for a single elephant
#' data(elephant_ud)
#'
#' # generate hcl color palette
#' pal <- palette_groups(elephant_ud)
#' head(pal)
#'
#' # visualize the palette in HCL space  with colorspace::hclplot
#' library(colorspace)
#' hclplot(pal[pal$specificity == 100, ]$color)
palette_groups <- function(x) {
  UseMethod("palette_groups")
}

#' @export
palette_groups.integer <- function(x) {
  if (x < 2) {
    stop("At least two groups are required to generate a palette.")
  } else if (x > 8) {
    stop(paste("Too many layers for palette_group.",
               "Try palette_timecycle or palette_timeline."))
  }

  # set up color wheel
  layer <- c(6, 1, 3, 8, 5, 2, 7, 4)
  n <- length(layer)
  wheel <- data.frame(specificity = rep(0:100, each = n),
                      layer = rep(layer, times = 101),
                      color = NA_character_,
                      stringsAsFactors = FALSE)

  # generate palettes for each chroma value
  for (i in 0:100) {
    idx <- seq(i * n + 1, (i + 1) * n, by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(n,
                                                     c = i,
                                                     l = 45,
                                                     start = 240,
                                                     end = 240 - 360 * (7/8),
                                                     fixup = TRUE)
  }

  # drop irrelevant layers
  wheel <- wheel[wheel$layer %in% seq.int(x), ]
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer"]]), ]
  row.names(wheel) <- NULL
  class(wheel) <- c("palette_groups", "data.frame")
  return(wheel)
}

#' @export
palette_groups.numeric <- function(x) {
  palette_groups(as.integer(x))
}

#' @export
palette_groups.Raster<- function(x) {
  palette_groups(raster::nlayers(x))
}
