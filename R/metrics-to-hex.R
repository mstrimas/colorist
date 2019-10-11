#' Assign cell colors and alpha levels based on annual cycle metrics
#'
#' @param x RasterStack of seasonility metrics from [annual_cycle_metrics()]
#' @param palette data frame containing an HCL color palette as output from
#'   [make_hcl_palette()]
#'
#' @return A data frame with four columns, specifying cell center (`x` and `y`),
#'   the color (`color`), and the alpha level (`alpha`).
#' @export
#' @examples
#' data(elephant_ud)
#' # generate annual cyle metrics and a color palette based on sample data
#' r <- annual_cycle_metrics(elephant_ud)
#' pal <- make_hcl_palette(elephant_ud)
#' # assign colors
#' cell_colors <- metrics_to_hex(r, pal)
#' head(cell_colors)
metrics_to_hex <- function(x, palette) {
  stopifnot(inherits(x, "Raster"))
  stopifnot(all(c("max", "peak_interval", "seasonality") %in% names(x)))
  stopifnot(inherits(palette, "data.frame"))
  stopifnot(all(c("seasonality", "interval", "color") %in% names(palette)))

  # rename for joining
  names(palette)[names(palette) == "seasonality"] <- "seasonality100"
  names(palette)[names(palette) == "interval"] <- "peak_interval"

  # assign color based on peak_interval and seasonality
  x_df <- raster::as.data.frame(x, xy = TRUE)
  x_df[["cell"]] <- seq.int(nrow(x_df))
  x_df <- na.omit(x_df)
  x_df[["seasonality100"]] <- round(100 * x_df[["seasonality"]], digits = 0)
  x_df <- merge(x_df, palette, by = c("peak_interval", "seasonality100"),
                all.x = TRUE, all.y = FALSE)

  # alpha level based on max value
  x_df[["alpha"]] <- x_df[["max"]] / max(x_df[["max"]], na.rm = TRUE)

  # clean up and return
  x_df[["seasonality100"]] <- NULL
  return(x_df[, c("cell", "x", "y",
                  "max", "peak_interval", "seasonality",
                  "n_intervals",
                  "color", "alpha")])
}
