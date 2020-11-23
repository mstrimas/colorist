# palette_timecycle ----

#' Make an HCL palette for visualizing a cyclical sequence of distributions
#'
#' @description This function generates an HCL palette for visualizing
#'   a cyclical sequence of distributions (e.g., a series of distributions
#'   describing species occurrence in each of 52 weeks of the annual
#'   cycle or a series of utilization distributions describing typical
#'   space use by an individual animal in each hour of a 24-hour daily cycle).
#'
#' @param x RasterStack or integer describing the number of layers for which
#'   colors need to be generated.
#' @param start_hue integer between -360 and 360 representing the starting hue
#'   in an HCL color wheel. For further details, consult the documentation for
#'   [colorspace::rainbow_hcl]. The default value of 240 will start the palette
#'   at "blue".
#' @param clockwise logical indicating which direction to move around color
#'   wheel. The default `clockwise = TRUE` will yield a
#'   "blue-green-yellow-pink-blue" palette when `start_hue = 240`, while
#'   `clockwise = FALSE` will yield a "blue-pink-yellow-green-blue" palette.
#'
#' @return A data frame with three columns:
#'   - `layer_id`: integer identifying the layer containing the maximum
#'   intensity value; mapped to hue.
#'   - `specificity`: the degree to which intensity values are unevenly
#'   distributed across layers; mapped to chroma.
#'   - `color`: the hexadecimal color associated with the given layer and
#'   specificity values.
#'
#' @family palette
#' @seealso [palette_timeline] for linear sequences of distributions and
#'   [palette_set] for unordered sets of distributions.
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
palette_timecycle <- function(x, start_hue = 240, clockwise = TRUE) {
  UseMethod("palette_timecycle")
}

#' @export
palette_timecycle.integer <- function(x, start_hue = 240, clockwise = TRUE) {
  if (x < 2) {
    stop("At least two intervals are required to generate a palette.")
  }
  stopifnot(is.numeric(start_hue), length(start_hue) == 1,
            start_hue >= -360, start_hue <= 360)
  stopifnot(is.logical(clockwise), length(clockwise) == 1)
  if (isTRUE(clockwise)) {
    end_hue <- start_hue - 360 * ((x - 1)  / x)
  } else {
    end_hue <- start_hue + 360 * ((x - 1)  / x)
  }

  # set up color wheel
  wheel <- data.frame(specificity = rep(0:100, each = x),
                      layer_id = rep(seq_len(x), times = 101),
                      color = NA_character_,
                      stringsAsFactors = FALSE)
  # generate palettes for each chroma value
  for (i in 0:100) {
    idx <- seq(i * x + 1, (i + 1) * x, by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(x,
                                                     c = i,
                                                     l = 45,
                                                     start = start_hue,
                                                     end = end_hue,
                                                     fixup = TRUE)
  }
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer_id"]]), ]
  row.names(wheel) <- NULL
  class(wheel) <- c("palette_timecycle", "data.frame")
  return(wheel)
}

#' @export
palette_timecycle.numeric <- function(x, start_hue = 240, clockwise = TRUE) {
  palette_timecycle(as.integer(x), start_hue, clockwise)
}

#' @export
palette_timecycle.Raster<- function(x, start_hue = 240, clockwise = TRUE) {
  palette_timecycle(raster::nlayers(x), start_hue, clockwise)
}


# palette_timeline ----

#' Make an HCL palette for visualizing a linear sequence of distributions
#'
#' @description This function generates an HCL palette for visualizing a linear
#'   sequence of distributions (e.g., a series of utilization
#'   distributions describing space use by an individual animal across each of
#'   20 consecutive days or a series of species distributions describing
#'   projected responses to global warming in 0.5 C increments).
#'
#' @param x RasterStack or integer describing the number of layers for which
#'   colors need to be generated.
#' @param start_hue integer between -360 and 360 representing the starting hue
#'   in the color wheel. For further details, consult the documentation for
#'   [colorspace::rainbow_hcl]. Recommended values are -130 ("blue-pink-yellow"
#'   palette) and 50 ("yellow-green-blue" palette).
#' @param clockwise logical indicating which direction to move around an HCL
#'   color wheel. When `clockwise = FALSE` the ending hue will be `start_hue +
#'   180`. When `clockwise = TRUE` the ending hue will be `start_hue - 180`. The
#'   default value `clockwise = FALSE` will yield a "blue-pink-yellow" palette
#'   when `start_hue = -130`, while `clockwise = TRUE` will yield a
#'   "blue-green-yellow" palette.
#'
#' @return A data frame with three columns:
#'   - `layer_id`: integer identifying the layer containing the maximum
#'   intensity value; mapped to hue.
#'   - `specificity`: the degree to which intensity values are unevenly
#'   distributed across layers; mapped to chroma.
#'   - `color`: the hexadecimal color associated with the given layer and
#'   specificity values.
#' @family palette
#' @seealso [palette_timecycle] for cyclical sequences of distributions and
#'   [palette_set] for unordered sets of distributions.
#' @export
#' @examples
#' # load fisher data
#' data(fisher_ud)
#'
#' # generate hcl color palette
#' pal_a <- palette_timeline(fisher_ud)
#' head(pal_a)
#'
#' # use a clockwise palette
#' pal_b <- palette_timeline(fisher_ud, clockwise = TRUE)
#'
#' # try a different starting hue
#' pal_c <- palette_timeline(fisher_ud, start = 50)
#'
#' # visualize the palette in HCL space  with colorspace::hclplot
#' library(colorspace)
#' hclplot(pal_a[pal_a$specificity == 100, ]$color)
#' hclplot(pal_b[pal_b$specificity == 100, ]$color)
#' hclplot(pal_c[pal_c$specificity == 100, ]$color)
palette_timeline <- function(x, start_hue = -130, clockwise = FALSE) {
  UseMethod("palette_timeline")
}

#' @export
palette_timeline.integer <- function(x, start_hue = -130, clockwise = FALSE) {
  if (x < 2) {
    stop("At least two layers are required to generate a palette.")
  }
  stopifnot(is.numeric(start_hue), length(start_hue) == 1,
            start_hue >= -360, start_hue <= 360)
  stopifnot(is.logical(clockwise), length(clockwise) == 1)
  if (isTRUE(clockwise)) {
    end_hue <- start_hue - 180
  } else {
    end_hue <- start_hue + 180
  }

  # set up color wheel
  wheel <- expand.grid(specificity = 0:100,
                       layer_id = seq_len(x),
                       color = NA_character_,
                       stringsAsFactors = FALSE)
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer_id"]]), ]
  row.names(wheel) <- NULL

  # generate palettes for each chroma value
  for (i in 0:100) {
    idx <- seq(i * x + 1, (i + 1) * x, by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(x,
                                                     c = i,
                                                     l = 45,
                                                     start = start_hue,
                                                     end = end_hue,
                                                     fixup = TRUE)
  }
  class(wheel) <- c("palette_timeline", "data.frame")
  return(wheel)
}

#' @export
palette_timeline.numeric <- function(x, start_hue = -130, clockwise = FALSE) {
  palette_timeline(as.integer(x), start_hue, clockwise)
}

#' @export
palette_timeline.Raster<- function(x, start_hue = -130, clockwise = FALSE) {
  palette_timeline(raster::nlayers(x), start_hue, clockwise)
}


# palette_set ----

#' Make an HCL palette for visualizing an unordered set of distributions
#'
#' @description This function generates an HCL palette for visualizing a small
#'   set of distributions (i.e., eight or fewer) that are not ordered in a
#'   linear or cyclical sequence (e.g., a set of utilization distributions
#'   describing space use by five separate individuals in the same population or
#'   a set of four species distributions that depend on similar food resources).
#'
#' @param x RasterStack or integer describing the number of layers for which
#'   colors need to be generated.
#' @param custom_hues vector of integers between -360 and 360 representing
#'   hues in the color wheel. For further details, consult the documentation
#'   for [colorspace::rainbow_hcl]. The length of the vector must equal the
#'   number of layers described by `x`. Hues are assigned to layers in order.
#'
#' @return A data frame with three columns:
#'   - `layer_id`: integer identifying the layer containing the maximum
#'   intensity value; mapped to hue.
#'   - `specificity`: the degree to which intensity values are unevenly
#'   distributed across layers; mapped to chroma.
#'   - `color`: the hexadecimal color associated with the given layer and
#'   specificity values.
#'
#' @family palette
#' @seealso [palette_timecycle] for cyclical sequences of distributions and
#'   [palette_timeline] for linear sequences of distributions.
#' @export
#' @examples
#' # load elephant data
#' data(elephant_ud)
#'
#' # generate hcl color palette
#' pal <- palette_set(elephant_ud)
#' head(pal)
#'
#' # visualize the palette in HCL space with colorspace::hclplot
#' library(colorspace)
#' hclplot(pal[pal$specificity == 100, ]$color)
palette_set <- function(x, custom_hues) {
  UseMethod("palette_set")
}

#' @export
palette_set.integer <- function(x, custom_hues) {
  if (x < 2) {
    stop("At least two layers are required to generate a palette.")
  } else if (x > 8) {
    stop(paste("Too many layers for palette_set.",
               "Try palette_timecycle or palette_timeline."))
  }

  if (missing(custom_hues)) {
    # set up color wheel
    layer <- c(6, 1, 3, 8, 5, 2, 7, 4)
    n <- length(layer)
    wheel <- data.frame(specificity = rep(0:100, each = n),
                        layer_id = rep(layer, times = 101),
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
  } else {
    if (x != length(custom_hues)) {
      stop("The number of layers in x and number of hues described in
           custom_hues must be equal.")
    }
    stopifnot(is.vector(custom_hues),
              is.numeric(custom_hues),
              custom_hues >= -360,
              custom_hues <= 360)

    # set up color wheel
    layer <- 1:x
    n <- length(layer)
    wheel <- data.frame(specificity = rep(0:100, each = n),
                        layer_id = rep(layer, times = 101),
                        color = NA_character_,
                        stringsAsFactors = FALSE)

    # generate palettes for each chroma value
    for (j in 1:n) {
      for (i in 0:100) {
        idx <- n * i + j
        wheel[["color"]][idx] <- colorspace::rainbow_hcl(1,
                                                       c = i,
                                                       l = 45,
                                                       start = custom_hues[j],
                                                       end = custom_hues[j],
                                                       fixup = TRUE)
      }
    }
  }

  # drop irrelevant layers
  wheel <- wheel[wheel$layer_id %in% seq.int(x), ]
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer_id"]]), ]
  row.names(wheel) <- NULL
  class(wheel) <- c("palette_set", "data.frame")
  return(wheel)

}

#' @export
palette_set.numeric <- function(x, custom_hues) {
  palette_set(as.integer(x), custom_hues)
}

#' @export
palette_set.Raster<- function(x, custom_hues) {
  palette_set(raster::nlayers(x), custom_hues)
}
