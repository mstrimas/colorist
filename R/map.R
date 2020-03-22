#' Visualize distributions in a single map
#'
#' @description This function enables visualization of distributional
#'   information in a single map by combining distribution metrics and an HCL
#'   color palette.
#'
#' @param x RasterStack of distributions processed by [metrics_pull()] or
#'   [metrics_distill()].
#' @param palette data frame containing an HCL color palette generated using
#'   [palette_timecycle()], [palette_timeline()], or [palette_set()].
#' @param layer integer (or character) corresponding to the layer ID (or name)
#'   of layer. A single distribution from within `x` is mapped when the `layer`
#'   argument is specified. The `layer` argument is ignored if
#'   [metrics_distill()] was used to generate `x`.
#' @param lambda number that allows visual tuning of intensity values via the
#'   [scales::modulus_trans()] function (see Details). Negative numbers decrease
#'   apparent skew of intensity values. Positive numbers increase apparent skew
#'   of intensity values.
#' @param return_df logical specifying whether the function should return a
#'   ggplot2 plot object (FALSE) or a data frame containing the raster data and
#'   associated cell colors.
#'
#' @details The lambda parameter allows for visual tuning of highly skewed
#'   distribution data. It is not uncommon for distributions to contain highly
#'   skewed intensity values because individuals spend a vast majority of their
#'   time within a relatively small area or because populations are relatively
#'   dense during some seasons and relatively dispersed during others. This can
#'   make visualizing distributions a challenge. The lambda parameter transforms
#'   intensity values via the [scales::modulus_trans()] function, allowing users
#'   to adjust the relative visual weight of high and low intensity values.
#'
#' @return A ggplot2 plot object of the map. Alternatively, with `return_df =
#'   TRUE` the function returns a data frame containing the raster data in
#'   data frame format along with the associated cell colors. The data frame
#'   columns are:
#'   - `x`,`y`: coordinates of raster cell centers.
#'   - `cell_number`: integer indicating the cell number within the raster.
#'   - `intensity`: maximum cell value across layers divided by the maximum
#'   value across all layers and cells; mapped to alpha level.
#'   - `specificity`: the degree to which intensity values are unevenly
#'   distributed across layers; mapped to chroma.
#'   - `layer_id`: integer identifying the layer containing the maximum
#'   intensity value; mapped to hue.
#'   - `color`: the hexadecimal color associated with the given layer and
#'   specificity values.
#'
#' @family map
#' @export
#' @examples
#' # load elephant data
#' data("elephant_ud")
#'
#' # prepare metrics
#' r <- metrics_distill(elephant_ud)
#'
#' # generate palette
#' pal <- palette_set(elephant_ud)
#'
#' # produce map
#' # set lambda to make areas that were used less intensively more conspicuous
#' map_single(r, pal, lambda = -5)
#'
map_single <- function(x, palette, layer, lambda = 0, return_df = FALSE) {
  stopifnot(inherits(x, c("RasterStack", "RasterBrick")))
  stopifnot(inherits(palette, "data.frame"),
            inherits(palette, c("palette_timeline",
                                "palette_timecycle",
                                "palette_set")),
            c("specificity", "layer_id", "color") %in% names(palette))
  stopifnot(length(lambda) == 1, is.numeric(lambda))

  # convert raster to data frame, pull vs. distill
  if (isTRUE(attr(x, "metric") == "pull")) {
    good_layer <- TRUE
    if (is_integer(layer)) {
      if (layer <= 0 || layer >= raster::nlayers(x)) {
        good_layer <- FALSE
      }
    } else if (is.character(layer)) {
      if (layer %in% names(x)) {
        layer <- which(layer == names(x))
      } else {
        good_layer <- FALSE
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
    r$layer_id <- layer
    r$cell_number <- seq.int(nrow(r))
  } else if (isTRUE(attr(x, "metric") == "distill")) {
    l <- c("intensity", "specificity", "layer_id")
    if (!all(l %in% names(x))) {
      stop(paste("Input raster missing layers:", setdiff(l, names(x))))
    }
    r <- raster::as.data.frame(x[[l]], xy = TRUE)
    r$cell_number <- seq.int(nrow(r))
  } else {
    stop(paste0("No metrics function called on the input raster.",
                "Try using metrics_pull() or metrics_distill()."))
  }
  r <- r[stats::complete.cases(r), ]

  # join palette
  r_pal <- merge(r, palette, by = c("specificity", "layer_id"), sort = FALSE)
  # order rows
  r_pal <- r_pal[, c("x", "y", "cell_number",
                     "intensity", "specificity", "layer_id",
                     "color")]
  # remove zeros
  r_pal <- r_pal[r_pal$intensity > 0, ]

  # return the underlying data frame if requested
  if (isTRUE(return_df)) {
    return(r_pal)
  }

  # make vector of colors for geom_tile
  map_colors <- r_pal$color
  names(map_colors) <- r_pal$cell_number

  # generate plot
  m <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = r_pal,
                       ggplot2::aes_(x = ~ x, y = ~ y,
                                     fill = ~ factor(cell_number),
                                     alpha = ~ intensity)) +
    ggplot2::scale_fill_manual(values = map_colors) +
    ggplot2::scale_color_manual(values = map_colors) +
    ggplot2::scale_alpha_continuous(trans = scales::modulus_trans(lambda + 1),
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
    ggplot2::coord_equal()

  return(m)
}


#' Visualize multiple distributions in a series of maps
#'
#' @description This function enables visualization of distributional
#'   information in a series of small multiples by combining distribution
#'   metrics and an HCL color palette.
#'
#' @param x RasterStack of distributions processed by [metrics_pull()].
#' @param palette data frame containing an HCL color palette generated using
#'   [palette_timecycle()], [palette_timeline()], or [palette_set()].
#' @param ncol integer specifying the number of columns in the grid of plots.
#' @param lambda number that allows visual tuning of intensity values via the
#'   [scales::modulus_trans()] function (see Details). Negative numbers decrease
#'   apparent skew of intensity values. Positive numbers increase apparent skew
#'   of intensity values.
#' @param labels character vector of layer labels for each plot. The default is
#'   to not show labels.
#' @param return_df logical specifying whether the function should return a
#'   ggplot2 plot object (FALSE) or a data frame containing the raster data and
#'   associated cell colors.
#'
#' @return A ggplot2 plot object of the map. Alternatively, `return_df = TRUE`
#'   will return a data frame containing the raster data in data frame format
#'   along with the associated cell colors. The data frame columns are:
#'   - `x`,`y`: coordinates of raster cell centers.
#'   - `cell_number`: integer indicating the cell number.
#'   - `layer_cell`: a unique ID for the cell within the layer in the format
#'   `"layer-cell_number"`.
#'   - `intensity`: maximum cell value across layers divided by the maximum
#'   value across all layers and cells; mapped to alpha level.
#'   - `specificity`: the degree to which intensity values are unevenly
#'   distributed across layers; mapped to chroma.
#'   - `layer_id`: the identity of the raster layer from which an intensity
#'   value was pulled; mapped to hue.
#'   - `color`: the hexadecimal color associated with the given layer and
#'   specificity values.
#'
#' @details The lambda parameter allows for visual tuning of highly skewed
#'   distribution data. It is not uncommon for distributions to contain highly
#'   skewed intensity values because individuals spend a vast majority of their
#'   time within a relatively small area or because populations are relatively
#'   dense during some seasons and relatively dispersed during others. This can
#'   make visualizing distributions a challenge. The lambda parameter transforms
#'   intensity values via the [scales::modulus_trans()] function, allowing users
#'   to adjust the relative visual weight of high and low intensity values.
#'
#' @family map
#' @export
#' @examples
#' # load fisher data
#' data("fisher_ud")
#'
#' # prepare data
#' r <- metrics_pull(fisher_ud)
#'
#' # generate palette
#' pal <- palette_timeline(fisher_ud)
#'
#' # produce maps
#' # set lambda to make areas that were used less intensively more conspicuous
#' map_multiples(r, pal, lambda = -5, labels = names(r))
map_multiples <- function(x, palette, ncol, lambda = 0, labels = NULL,
                          return_df = FALSE) {
  stopifnot(inherits(x, c("RasterStack", "RasterBrick")))
  stopifnot(inherits(palette, "data.frame"),
            inherits(palette, c("palette_timeline",
                                "palette_timecycle",
                                "palette_set")),
            c("specificity", "layer_id", "color") %in% names(palette))
  stopifnot(length(lambda) == 1, is.numeric(lambda))
  if (missing(ncol)) {
    ncol <- round(sqrt(raster::nlayers(x)))
  } else {
    stopifnot(length(ncol) == 1, is_integer(ncol), ncol > 0,
              ncol <= raster::nlayers(x))
  }
  if (isTRUE(attr(x, "metric") == "distill")) {
    stop(paste0("map_multiples() does not work with metrics_distill().",
                "Try using metrics_pull()."))
  } else if (!isTRUE(attr(x, "metric") == "pull")) {
    stop(paste0("No metrics function called on the input raster.",
                "Try using metrics_pull()."))
  }

  # convert raster to data frame
  r <- raster::as.data.frame(x, xy = TRUE)
  names(r) <- c("x", "y", seq.int(raster::nlayers(x)))
  r <- tidyr::pivot_longer(r, cols = seq(3, raster::nlayers(x) + 2),
                           names_to = "layer_id",
                           values_to = "intensity")
  r$layer_id <- as.integer(r$layer_id)
  r$specificity <- 100
  r$cell_number <- seq.int(nrow(r))
  r <- r[stats::complete.cases(r), ]
  r$layer_cell <- paste(r$layer_id, r$cell_number, sep = "-")

  # join palette
  r_pal <- merge(r, palette, by = c("specificity", "layer_id"), sort = FALSE)
  # order rows
  r_pal <- r_pal[, c("x", "y", "cell_number", "layer_cell",
                     "intensity", "specificity", "layer_id",
                     "color")]
  # remove zeros
  r_pal <- r_pal[r_pal$intensity > 0, ]

  # return the underlying data frame if requested
  if (isTRUE(return_df)) {
    return(r_pal)
  }

  # make vector of colors for geom_tile
  map_colors <- r_pal$color
  names(map_colors) <- r_pal$layer_cell

  if (is.null(labels)) {
    # suppress labels
    labels <- rep("", times = raster::nlayers(x))
    names(labels) <- seq.int(raster::nlayers(x))
  } else {
    if (length(labels) != raster::nlayers(x)) {
      stop("Length of labels must be equal to the number of layers in x.")
    }
    names(labels) <- seq.int(raster::nlayers(x))
  }

  # generate multipanel plot
  m <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = r_pal,
                       ggplot2::aes_(x = ~ x, y = ~ y,
                                     fill = ~ factor(layer_cell),
                                     alpha = ~ intensity)) +
    ggplot2::scale_fill_manual(values = map_colors) +
    ggplot2::scale_alpha_continuous(trans = scales::modulus_trans(lambda + 1),
                                    range = c(0, 1)) +
    ggplot2::facet_wrap(~ layer_id, ncol = ncol,
                        labeller = ggplot2::labeller(layer_id = labels)) +
    ggplot2::guides(fill = FALSE, alpha = FALSE) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                   plot.background = ggplot2::element_rect(fill = "white"),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_rect(fill = NA,
                                                        color = "white"),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::coord_equal()

  return(m)
}


is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}
