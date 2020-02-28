#' Visualize distributions as a single map
#'
#' @param x RasterStack of distributions processed by [metrics_pull()] or
#'   [metrics_distill()].
#' @param palette data frame containing an HCL color palette.
#' @param layer integer or character corresponding to the index or name of the
#'   single layer to map. This argument is ignored if [metrics_distill()] was
#'   used.
#' @param lambda integer controlling the offset in the Box-Cox transformation of
#'   intensities to alpha levels via the [scales::modulus_trans()] function. Use
#'   0 for Box-Cox type 1 or any non-negative number for Box-Cox type 2.
#'
#' @return A ggplot2 plot object.
#' @export
#' @examples
#' data("elephant_ud")
#'
#' # prepare metrics
#' r <- metrics_distill(elephant_ud)
#'
#' # generate palette
#' pal <- palette_timeline(elephant_ud)
#'
#' # produce map
#' map_single(r, pal)
map_single <- function(x, palette, layer, lambda = 0) {
  stopifnot(inherits(x, c("RasterStack", "RasterBrick")))
  stopifnot(inherits(palette, "data.frame"),
            inherits(palette, c("palette_timeline",
                                "palette_timecycle",
                                "palette_groups")),
            c("specificity", "peak_layer", "color") %in% names(palette))
  stopifnot(length(lambda) == 1, is.numeric(lambda), lambda >= 0)

  # convert raster to data frame, pull vs. distill
  if (isTRUE(attr(x, "metric") == "pull")) {
    good_layer <- TRUE
    if (is_integer(layer)) {
      if (layer <= 0 || layer >= raster::nlayers(x)) {
        good_layer <- FALSE
      }
    } else if (!layer %in% names(x)) {
      if (layer <= 0 || layer >= raster::nlayers(x)) {
        good_layer <- FALSE
      } else {
        layer <- which(layer == names(x))
      }
    } else {
      good_layer <- FALSE
    }
    if (!isTRUE(good_layer)) {
      stop(paste("Invalid layer specification:", layer))
    }
    r <- raster::as.data.frame(x[[layer]], xy = TRUE)
    names(r)[3] <- "intensity"
    r$specificity <- 100
    r$peak_layer <- layer
    r$cell_number <- seq.int(nrow(r))
  } else if (isTRUE(attr(x, "metric") == "distill")) {
    l <- c("intensity", "specificity", "peak_layer")
    if (!all(l %in% names(x))) {
      stop(paste("Input raster missing layers:", setdiff(l, names(x))))
    }
    r <- raster::as.data.frame(x[[l]], xy = TRUE)
    r$cell_number <- seq.int(nrow(r))
  } else {
    stop(paset0("No metric function called on the input raster.",
                "Try using metric_pull() or metric_distill()."))
  }
  r <- r[stats::complete.cases(r), ]

  # join palette
  r_pal <- merge(r, palette, by = c("specificity", "peak_layer"), sort = FALSE)
  # order rows
  r_pal <- r_pal[, c("x", "y", "cell_number",
                     "intensity", "specificity", "peak_layer",
                     "color")]
  # remove zeros
  r_pal <- r_pal[r_pal$intensity > 0, ]

  # make vector of colors for geom_tile
  map_colors <- r_pal$color
  names(map_colors) <- r_pal$cell_number

  # generate plot
  m <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = r_pal,
                       ggplot2::aes_(x = ~ x, y = ~y,
                                     fill = ~ factor(cell_number),
                                     alpha = ~ intensity)) +
    ggplot2::scale_fill_manual(values = map_colors) +
    ggplot2::scale_color_manual(values = map_colors) +
    ggplot2::scale_alpha_continuous(trans = scales::modulus_trans(lambda),
                                    range = c(0, 1)) +
    ggplot2::guides(fill = FALSE, alpha = FALSE) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                   plot.background = ggplot2::element_rect(fill = "white"),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_rect(fill = NA,
                                                        color = "black"),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::coord_sf()

  return(m)
}

#' Visualize distributions as a single map
#'
#' @param x RasterStack of distributions processed by [metrics_pull()].
#' @param palette data frame containing an HCL color palette.
#' @param ncol integer specifying the number of columns in the grid of plots.
#' @param lambda integer controlling the offset in the Box-Cox transformation of
#'   intensities to alpha levels via the [scales::modulus_trans()] function. Use
#'   0 for Box-Cox type 1 or any non-negative number for Box-Cox type 2.
#' @param labels character vector of layer labels for each plot. The default is
#'   to not show labels.
#'
#' @return A ggplot2 plot object.
#' @export
#' @examples
#' data("elephant_ud")
#'
#' # prepare data
#' r <- metrics_pull(elephant_ud)
#'
#' # generate palette
#' pal <- palette_timeline(elephant_ud)
#'
#' # produce map
#' map_multiples(r, pal)
map_multiples <- function(x, palette, ncol, lambda = 0, labels = NULL) {
  stopifnot(inherits(x, c("RasterStack", "RasterBrick")))
  stopifnot(inherits(palette, "data.frame"),
            inherits(palette, c("palette_timeline",
                                "palette_timecycle",
                                "palette_groups")),
            c("specificity", "peak_layer", "color") %in% names(palette))
  stopifnot(length(lambda) == 1, is.numeric(lambda), lambda >= 0)
  if (missing(ncol)) {
    ncol <- round(sqrt(raster::nlayers(x)))
  } else {
    stopifnot(length(ncol), is_integer(ncol), ncol > 0,
              ncol <= raster::nlayers(x))
  }
  if (isTRUE(attr(x, "metric") == "distill")) {
    stop(paset0("map_multiples() does not work with metric_distill().",
                "Try using metric_pull()."))
  } else if (!isTRUE(attr(x, "metric") == "pull")) {
    stop(paset0("No metric function called on the input raster.",
                "Try using metric_pull()."))
  }

  # convert raster to data frame
  r <- raster::as.data.frame(x, xy = TRUE)
  names(r) <- c("x", "y", seq.int(raster::nlayers(x)))
  r <- tidyr::pivot_longer(r, cols = seq(3, raster::nlayers(x) + 2),
                           names_to = "peak_layer",
                           values_to = "intensity")
  r$specificity <- 100
  r$cell_number <- seq.int(nrow(r))
  r <- r[stats::complete.cases(r), ]
  r$layer_cell <- paste(r$peak_layer, r$cell_number, sep = "-")

  # join palette
  r_pal <- merge(r, palette, by = c("specificity", "peak_layer"), sort = FALSE)
  # order rows
  r_pal <- r_pal[, c("x", "y", "cell_number", "layer_cell",
                     "intensity", "specificity", "peak_layer",
                     "color")]
  # remove zeros
  r_pal <- r_pal[r_pal$intensity > 0, ]

  # make vector of colors for geom_tile
  map_colors <- r_pal$color
  names(map_colors) <- r_pal$layer_cell

  if (is.null(labels)) {
    # suppress labels
    labels <- rep("", times = raster::nlayers(x))
    names(labels) <- seq.int(raster::nlayers(x))
  } else {
    if (length(labels) != raster::nlayers(x)) {
      stop("Length 0f labels must be equal to the number of layers in x.")
    }
    names(labels) <- seq.int(raster::nlayers(x))
  }

  # generate multipanel plot
  m <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = r_pal,
                       ggplot2::aes_(x = ~ x, y = ~y,
                                     fill = ~ factor(layer_cell),
                                     alpha = ~ intensity)) +
    ggplot2::scale_fill_manual(values = map_colors) +
    #ggplot2::scale_color_manual(values = map_colors) +
    ggplot2::scale_alpha_continuous(trans = scales::modulus_trans(lambda),
                                    range = c(0, 1)) +
    ggplot2::facet_wrap(~ peak_layer, ncol = ncol,
                        labeller = labeller(peak_layer = labels)) +
    ggplot2::guides(fill = FALSE, alpha = FALSE) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                   plot.background = ggplot2::element_rect(fill = "white"),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = element_rect(fill = NA, color = "white"),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::coord_sf()

  return(m)
}

is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}
