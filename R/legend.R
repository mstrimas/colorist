# legend_timecycle ----

#' Make HCL legend for a cyclical sequence of distributions
#'
#' @param x RasterStack or integer giving the number of layers to build the
#'   HCL legend for.
#' @param specificity logical indicating whether to show a single color wheel or
#'   three wheels demonstrating the palette for intensity values of 0, 50, and
#'   100.
#' @param return_df logical indicating whether to return the legend as a
#'   `ggplot2` object or return a data frame containing the necessary data to
#'   build the legend.
#'
#' @return A ggplot2 plot object of the legend. Alternatively,
#'   `return_df = TRUE` will return a data frame containing a data frame
#'   containing the data needed to build the legend. The data frame columns are:
#'   - `specificity`: amount of variation between layers; mapped to chroma.
#'   - `layer`: layer with the maximum cell value; mapped to hue.
#'   - `color`: color associated with the given specificity and peak layer.
#'   - `intensity`: maximum cell value across layers divided by the maximum
#'   value across all layers and cells; mapped to alpha level.
#'
#' @family palette
#' @seealso [legend_timeline] for linear sequences of distributions.
#' @export
#' @examples
#' # load field sparrow data
#' data(fiespa_occ)
#'
#' # generate hcl legend
#' legend_timecycle(fiespa_occ)
legend_timecycle <- function(x, specificity = TRUE, return_df = FALSE) {
  UseMethod("legend_timecycle")
}

#' @export
legend_timecycle.integer <- function(x, specificity = TRUE, return_df = FALSE) {
  if (x < 2) {
    stop("At least two intervals are required to generate a palette.")
  }
  stopifnot(is.logical(specificity), length(specificity) == 1)
  stopifnot(is.logical(return_df), length(return_df) == 1)
  # start the palette at blue
  start <- round(0.67 * x, 0)
  layer <- c(start:1, x:(start + 1))
  specs <- c(0, 50, 100)
  wheel <- data.frame(specificity = rep(specs, each = x),
                      layer = rep(layer, times = 3),
                      color = NA_character_,
                      stringsAsFactors = FALSE)
  # generate palettes for each chroma value
  for (i in seq_along(specs)) {
    idx <- seq((i - 1) * x + 1, i * x, by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(x,
                                                     c = specs[i],
                                                     l = 45,
                                                     start = 0,
                                                     end = 360 * ((x - 1)  / x),
                                                     fixup = TRUE)
  }

  # add intensity
  wheel <- merge(wheel, data.frame(intensity = seq(0, 1, 0.05)))
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer"]],
                       wheel[["intensity"]]), ]
  class(wheel) <- c("legend_timecycle", "data.frame")

  # return data without plotting if requested
  if (isTRUE(return_df)) {
    return(wheel)
  }

  # whether to show 3 or 1 wheels
  if (!isTRUE(specificity)) {
    wheel <- wheel[wheel$specificity == 100, ]
  }

  # make named vector of colors
  tile_colors <- sort(unique(wheel$color))
  names(tile_colors) <- tile_colors

  # make labels for legend
  labs <- c("low", "moderate", "high")
  names(labs) <- c(0, 50, 100)

  # describe ggplot
  p <- ggplot2::ggplot(data = wheel) +
    ggplot2::aes_(x = ~ layer, y = ~ intensity, fill = ~ color,
                  alpha = ~ intensity) +
    ggplot2::geom_tile(size = 0) +
    ggplot2::facet_wrap(~ specificity, nrow = 1,
               labeller = ggplot2::labeller(specificity = labs)) +
    ggplot2::scale_fill_manual(values = tile_colors) +
    ggplot2::scale_alpha_continuous(range = c(0, 1)) +
    ggplot2::coord_polar(theta = "x", start = 0) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11, hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0, "lines"),
      plot.background = ggplot2::element_rect(fill = "white"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()) +
    ggplot2::guides(fill = FALSE, alpha = FALSE) +
    ggplot2::ggtitle("Specificity") +
    ggplot2::xlab("Interval") +
    ggplot2::ylab("Maximum\nintensity")

  return(p)
}

#' @export
legend_timecycle.numeric <- function(x, specificity = TRUE, return_df = FALSE) {
  legend_timecycle(as.integer(x), specificity, return_df)
}

#' @export
legend_timecycle.Raster <- function(x, specificity = TRUE, return_df = FALSE) {
  legend_timecycle(raster::nlayers(x), specificity, return_df)
}


# legend_timeline ----

#' Make HCL legend for a linear sequence of distributions
#'
#' @param x RasterStack or integer giving the number of layers to build the
#'   HCL legend for.
#' @param start_hue integer between -360 and 360 representing the starting hue
#'   in the color wheel. The ending hue will be `start_hue` + 180. For
#'   further details, consult the documentation for [colorspace::rainbow_hcl].
#'   Recommended values are -130, giving a blue-pink-yellow palette, and 50,
#'   giving a yellow-green-blue palette.
#' @param specificity logical indicating whether to show a single color wheel or
#'   three wheels demonstrating the palette for intensity values of 0, 50, and
#'   100.
#' @param time_labels character vector with two elements to be used as labels
#'   for the start and end points of the time axis (i.e. x-axis) in the legend.
#' @param return_df logical indicating whether to return the legend as a
#'   `ggplot2` object or return a data frame containing the necessary data to
#'   build the legend.
#'
#' @return A ggplot2 plot object of the legend. Alternatively,
#'   `return_df = TRUE` will return a data frame containing a data frame
#'   containing the data needed to build the legend. The data frame columns are:
#'   - `specificity`: amount of variation between layers; mapped to chroma.
#'   - `layer`: layer with the maximum cell value; mapped to hue.
#'   - `color`: color associated with the given specificity and peak layer.
#'   - `intensity`: maximum cell value across layers divided by the maximum
#'   value across all layers and cells; mapped to alpha level.
#'
#' @family palette
#' @seealso [palette_timecycle] for cyclical sequences of distributions.
#' @export
#' @examples
#' # load field sparrow data
#' data(fiespa_occ)
#'
#' # generate hcl legend
#' legend_timeline(fiespa_occ)
legend_timeline <- function(x, start_hue = -130, specificity = TRUE,
                            time_labels = NULL, return_df = FALSE) {
  UseMethod("legend_timeline")
}

#' @export
legend_timeline.integer <- function(x, start_hue = -130, specificity = TRUE,
                                    time_labels = NULL, return_df = FALSE) {
  if (x < 2) {
    stop("At least two intervals are required to generate a palette.")
  }
  stopifnot(length(start_hue) == 1, start_hue >= -360, start_hue <= 360)
  stopifnot(is.logical(specificity), length(specificity) == 1)
  stopifnot(is.character(time_labels), length(time_labels) == 2)
  stopifnot(is.logical(return_df), length(return_df) == 1)

  # start the palette at blue
  specs <- c(0, 50, 100)
  wheel <- expand.grid(specificity = specs,
                       layer = seq_len(x),
                       color = NA_character_,
                       stringsAsFactors = FALSE)
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer"]]), ]
  row.names(wheel) <- NULL

  # generate palettes for each chroma value
  for (i in seq_along(specs)) {
    idx <- seq((i - 1) * x + 1, i * x, by = 1)
    wheel[["color"]][idx] <- colorspace::rainbow_hcl(x,
                                                     c = specs[i],
                                                     l = 45,
                                                     start = start_hue,
                                                     end = start_hue + 180,
                                                     fixup = TRUE)
  }

  # add intensity
  wheel <- merge(wheel, data.frame(intensity = seq(0, 1, 0.05)))
  wheel <- wheel[order(wheel[["specificity"]], wheel[["layer"]],
                       wheel[["intensity"]]), ]
  class(wheel) <- c("legend_timeline", "data.frame")

  # return data without plotting if requested
  if (isTRUE(return_df)) {
    return(wheel)
  }

  # whether to show 3 or 1 wheels
  if (!isTRUE(specificity)) {
    wheel <- wheel[wheel$specificity == 100, ]
  }

  # make named vector of colors
  tile_colors <- sort(unique(wheel$color))
  names(tile_colors) <- tile_colors

  # make labels for legend
  wheel$specificity <- factor(wheel$specificity, levels = c(100, 50, 0))
  labs <- c("Low specificity", "Moderate specificity", "High specificity")
  names(labs) <- c(0, 50, 100)

  # describe ggplot
  p <- ggplot2::ggplot(data = wheel) +
    ggplot2::aes_(x = ~ layer, y = ~ intensity, fill = ~ color,
                  alpha = ~ intensity) +
    ggplot2::geom_tile(size = 0) +
    ggplot2::facet_wrap(~ specificity, ncol = 1,
                        labeller = ggplot2::labeller(specificity = labs)) +
    scale_x_continuous(breaks = c(1, x), labels = time_labels) +
    ggplot2::scale_fill_manual(values = tile_colors) +
    ggplot2::scale_alpha_continuous(range = c(0, 1)) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.spacing = ggplot2::unit(0, "lines"),
          plot.background = ggplot2::element_rect(fill = "white"),
          plot.title = ggplot2::element_text(size = 11, hjust = 0.5),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          aspect.ratio = 0.4) +
    ggplot2::guides(fill = FALSE, alpha = FALSE) +
    ggplot2::xlab("Interval") +
    ggplot2::ylab("Maximum\nintensity")

  return(p)
}

#' @export
legend_timeline.numeric <- function(x, start_hue = -130, specificity = TRUE,
                                    time_labels = NULL, return_df = FALSE) {
  legend_timeline(as.integer(x), start_hue, specificity, time_labels,
                  return_df)
}

#' @export
legend_timeline.Raster <- function(x, start_hue = -130, specificity = TRUE,
                                   time_labels = NULL, return_df = FALSE) {
  legend_timeline(raster::nlayers(x), start_hue, specificity, time_labels,
                  return_df)
}
