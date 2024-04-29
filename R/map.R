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
#' @param lambda_i number that allows visual tuning of intensity values via the
#'   [scales::modulus_trans()] function (see Details). Negative numbers increase
#'   the opacity of cells with low intensity values. Positive numbers decrease
#'   the opacity of cells with low intensity values.
#' @param lambda_s number that allows visual tuning of specificity values via
#'   the [scales::modulus_trans()] function (see Details). Negative numbers
#'   increase the chroma of cells with low specificity values. Positive numbers
#'   decrease the chroma of cells with low specificity values.
#' @param return_type character specifying whether the function should return a
#'   `ggplot2` plot object ("plot"), `RasterStack` ("stack"), or data frame
#'   ("df"). The default is to return a `ggplot2` object.
#'
#' @details The `lambda_i` parameter allows for visual tuning of intensity
#'   values with unusual distributions. For example, distributions often
#'   contain highly skewed intensity values because individuals spend a vast
#'   majority of their time within a relatively small area or because
#'   populations are relatively dense during some seasons and relatively
#'   dispersed during others. This can make visualizing distributions a
#'   challenge. The `lambda_i` parameter transforms intensity values via the
#'   [scales::modulus_trans()] function, allowing users to adjust the relative
#'   visual weight of high and low intensity values.
#'
#'   The `lambda_s` parameter allows for visual tuning of specificity
#'   values via the [scales::modulus_trans()] function. Adjustment of
#'   `lambda_s` affects the distribution of chroma values across areas of
#'   relatively low and high specificity, thus modifying information available
#'   to viewers. USE WITH CAUTION!
#'
#' @return By default, or when `return_type = "plot"`, the function returns a
#'   map that is a `ggplot2` plot object.
#'
#'   When `return_type = "stack"`, the function returns a `RasterStack`
#'   containing five layers that enable RGBa visualization of a map using other
#'   R packages or external GIS software:
#'   - `R`: red, integer values (0-255).
#'   - `G`: green, integer values (0-255).
#'   - `B`: blue, integer values (0-255).
#'   - `alpha`: opacity, numeric values (0-255).
#'   - `n_layers`: number of layers in `x` with non-NA values.
#'
#'   When `return_type = "df"`, the function returns a data frame containing
#'   seven columns:
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
#' # produce map, adjusting lambda_i to make areas that were used less
#' # intensively more conspicuous
#' map_single(r, pal, lambda_i = -5)
#'
#' # return RasterStack containing RGBa values
#' m <- map_single(r, pal, lambda_i = -5, return_type = "stack")
#'
#' # visualize RGBa values
#' library(raster)
#' plotRGB(m, 1, 2, 3, alpha = as.vector(m[[4]]))
map_single <- function(x, palette, layer, lambda_i = 0, lambda_s = 0,
                       return_type = c("plot", "stack", "df")) {
  stopifnot(inherits(x, c("RasterStack", "RasterBrick")))
  stopifnot(inherits(palette, "data.frame"),
            inherits(palette, c("palette_timeline",
                                "palette_timecycle",
                                "palette_set")),
            c("specificity", "layer_id", "color") %in% names(palette))
  stopifnot(length(lambda_i) == 1, is.numeric(lambda_i))
  stopifnot(length(lambda_s) == 1, is.numeric(lambda_s))
  return_type <- match.arg(return_type)

  # convert raster to data frame, pull vs. distill
  if (isTRUE(attr(x, "metric") == "pull")) {
    good_layer <- TRUE
    if (is_integer(layer)) {
      if (layer <= 0 || layer > raster::nlayers(x)) {
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

  # apply lambda_s if appropriate
  if (lambda_s != 0) {
    rspec <- modulus(1, lambda_s + 1)
    lspec <- modulus(r$specificity / 100, lambda_s + 1) / rspec
    r$specificity <- 100 * round(lspec, 2)
  }

  r$layer_id <- as.integer(r$layer_id)
  r$specificity <- as.integer(r$specificity)

  # join palette
  r_pal <- merge(r, palette, by = c("specificity", "layer_id"), sort = FALSE)
  # order rows
  r_pal <- r_pal[, c("x", "y", "cell_number",
                     "intensity", "specificity", "layer_id",
                     "color")]
  # remove zeros
  r_pal <- r_pal[r_pal$intensity > 0, ]

  # return the underlying data frame if requested
  if (return_type == "df") {
    return(r_pal)
  } else if (return_type == "stack") {

    # convert hex to rgb and fill empty stack with data
    rgb <- do.call(cbind, lapply(r_pal$color, grDevices::col2rgb))

    # create template using input raster
    template <- x[[1]]
    template[] <- 0
    rgban <- raster::stack(template, template, template, template, template)

    # adjust lambda_i if appropriate, typically done by scales within ggplot
    lint <- r_pal$intensity

    if (lambda_i != 0) {
      rint <- modulus(1, lambda_i + 1)
      lint <- modulus(r_pal$intensity, lambda_i + 1) / rint
    }

    # fill stack with values
    rgban[[1]][r_pal$cell_number] <- rgb[seq(1, 3 * nrow(r_pal), 3)]
    rgban[[2]][r_pal$cell_number] <- rgb[seq(2, 3 * nrow(r_pal), 3)]
    rgban[[3]][r_pal$cell_number] <- rgb[seq(3, 3 * nrow(r_pal), 3)]
    rgban[[4]][r_pal$cell_number] <- lint * 255
    rgban[[5]] <- x[[4]]

    names(rgban) <- c("R", "G", "B", "alpha", "n_layers")

    return(rgban)

  } else {

    # make vector of colors for geom_tile
    map_colors <- r_pal$color
    names(map_colors) <- r_pal$cell_number

    # generate plot
    m <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = r_pal,
                         ggplot2::aes(x = .data$x, y = .data$y,
                                      fill = factor(.data$cell_number),
                                      alpha = .data$intensity)) +
      ggplot2::scale_fill_manual(values = map_colors) +
      ggplot2::scale_alpha_continuous(trans = scales::modulus_trans(lambda_i + 1),
                                      range = c(0, 1)) +
      ggplot2::guides(fill = "none", alpha = "none") +
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
#' @param lambda_i number that allows visual tuning of intensity values via the
#'   [scales::modulus_trans()] function (see Details). Negative numbers increase
#'   the opacity of cells with low intensity values. Positive numbers decrease
#'   the opacity of cells with low intensity values.
#' @param labels character vector of layer labels for each plot. The default is
#'   to not show labels.
#' @param return_type character specifying whether the function should return a
#'   `ggplot2` plot object ("plot") or data frame ("df"). The default is to
#'   return a `ggplot2` object.
#'
#' @return By default, or when `return_type = "plot"`, the function returns a
#'   map that is a `ggplot2` plot object.
#'
#'   When `return_type = "df"`, the function returns a data frame containing
#'   eight columns:
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
#' @details The `lambda_i` parameter allows for visual tuning of intensity
#'   values with unusual distributions. For example, distributions often
#'   contain highly skewed intensity values because individuals spend a vast
#'   majority of their time within a relatively small area or because
#'   populations are relatively dense during some seasons and relatively
#'   dispersed during others. This can make visualizing distributions a
#'   challenge. The `lambda_i` parameter transforms intensity values via the
#'   [scales::modulus_trans()] function, allowing users to adjust the relative
#'   visual weight of high and low intensity values.
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
#' # produce maps, adjusting lambda_i to make areas that were used less
#' # intensively more conspicuous
#' map_multiples(r, pal, lambda_i = -5, labels = paste("night", 1:9))
map_multiples <- function(x, palette, ncol, lambda_i = 0, labels = NULL,
                          return_type = c("plot", "df")) {
  stopifnot(inherits(x, c("RasterStack", "RasterBrick")))
  stopifnot(inherits(palette, "data.frame"),
            inherits(palette, c("palette_timeline",
                                "palette_timecycle",
                                "palette_set")),
            c("specificity", "layer_id", "color") %in% names(palette))
  stopifnot(length(lambda_i) == 1, is.numeric(lambda_i))
  return_type <- match.arg(return_type)

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
  if (return_type == "df") {
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
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    fill = factor(.data$layer_cell),
                                    alpha = .data$intensity)) +
    ggplot2::scale_fill_manual(values = map_colors) +
    ggplot2::scale_alpha_continuous(trans = scales::modulus_trans(lambda_i + 1),
                                    range = c(0, 1)) +
    ggplot2::facet_wrap(~ layer_id, ncol = ncol,
                        labeller = ggplot2::labeller(layer_id = labels)) +
    ggplot2::guides(fill = "none", alpha = "none") +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                   plot.background = ggplot2::element_rect(fill = "white"),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_rect(fill = NA,
                                                        color = "white"),
                   panel.spacing = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::coord_equal(expand = F)

  return(m)
}


is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))
}

modulus <- function(y, lambda) {
  if (lambda != 0) {
    y_t <- sign(y) * ((abs(y) + 1) ^ lambda - 1) / lambda
  } else {
    y_t = sign(y) * log(abs(y) + 1)
  }
  return(y_t)
}
