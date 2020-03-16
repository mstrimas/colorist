#' eBird Status & Trends Field Sparrow occurrence probability
#'
#' A [RasterStack][raster::RasterStack-class] of the expected probability of
#' occurrence of Field Sparrow from the [eBird Status &
#' Trends](https://ebird.org/science/status-and-trends) project. Each of the 12
#' layers in the stack represent the estimated occurrence for a given month of
#' the year over a regular grid of points covering the full range of the
#' species. To reduce file size, these data have been aggregated from the native 2.96 km spatial
#' resolution and weekly temporal resolution to monthly, 14.8 km resolution.
#'
#' For further details on these data, and to access the data for more species,
#' consult the [documentation for the `ebirdst`
#' package](https://cornelllabofornithology.github.io/ebirdst).
#'
#' @source <https://ebird.org/science/status-and-trends>
#' @references Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, O. Robinson,
#'   S. Ligocki, B. Petersen, C. Wood, I. Davies, B. Sullivan, M. Iliff, S.
#'   Kelling. 2020. eBird Status and Trends, Data Version: 2018; Released: 2020.
#'   Cornell Lab of Ornithology, Ithaca, New York.
#'   <https://doi.org/10.2173/ebirdst.2018>
"fiespa_occ"

#' African Elephant utilization distribution
#'
#' A [RasterStack][raster::RasterStack-class] of the [utilization
#' distribution](https://en.wikipedia.org/wiki/Utilization_distribution) for a
#' single African Elephant in Etosha National Park. Cell values represent the
#' probability density that the individual elephant was found at a given
#' location and the four layers represent four successive years of data. This
#' distribution was derived from tracking data using the `adehabitatHR` package.
#' derived from tracking data.
#'
#' @source <https://www.datarepository.movebank.org/handle/10255/move.812>
#' @references
#' Tsalyuk M., W. Kilian, B. Reineking, W. Marcus. 2018. Temporal variation in
#' resource selection of African elephants follows long term variability in
#' resource availability. Ecological Monographs.
#' <https://doi.org/10.1002/ecm.1348>
#'
#' W. Kilian, W.M. Getz, R. Zidon, M. Tsalyuk. 2018. Data from: Temporal
#' variation in resource selection of African elephants follows long term
#' variability in resource availability. Movebank Data Repository.
#' <https://doi.org/10.5441/001/1.3nj3qj45>
"elephant_ud"
