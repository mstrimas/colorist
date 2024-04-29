#' eBird Status & Trends Field Sparrow occurrence probability
#'
#' A [RasterStack][raster::RasterStack-class] of the expected probability of
#' occurrence of Field Sparrow from the [eBird Status &
#' Trends](https://ebird.org/science/status-and-trends) project. Each of the 12
#' layers in the stack represent the estimated occurrence for a given month of
#' the year over a regular grid of points covering the full range of the
#' species. To reduce file size, these data have been aggregated from the native
#' 3 km spatial resolution and weekly temporal resolution to monthly, 27 km
#' resolution.
#'
#' For further details on these data, and to access the data for more species,
#' consult the [documentation for the `ebirdst`
#' package](https://ebird.github.io/ebirdst/).
#'
#' @source <https://ebird.org/science/status-and-trends>
#' @references Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, S. Ligocki,
#'   O. Robinson, W. Hochachka, L. Jaromczyk, C. Crowley, K. Dunham,
#'   A. Stillman, I. Davies, A. Rodewald, V. Ruiz-Gutierrez, C. Wood. 2023.
#'   eBird Status and Trends, Data Version: 2022; Released: 2023.
#'   Cornell Lab of Ornithology, Ithaca, New York.
#'   <doi:10.2173/ebirdst.2022>
"fiespa_occ"


#' African Elephant utilization distributions
#'
#' A [RasterStack][raster::RasterStack-class] of [utilization
#' distributions](https://en.wikipedia.org/wiki/Utilization_distribution) for two
#' individual African Elephants in Etosha National Park in 2011. Cell values
#' represent the probability density that an elephant was found at a given
#' location within the year and the two layers contain data for the two
#' individual elephants. Utilization distributions were generated using the
#' `adehabitatHR` package from GPS tracking data. W. Kilian, W.M. Getz, R.
#' Zidon, and M. Tsalyuk graciously provided permission to use their data for
#' visualization purposes.
#'
#' @source <https://datarepository.movebank.org/handle/10255/move.812>
#' @references
#' Tsalyuk, M., W. Kilian, B. Reineking, W. Marcus. 2018. Temporal variation in
#' resource selection of African elephants follows long term variability in
#' resource availability. Ecological Monographs.
#' <doi:10.1002/ecm.1348>
#'
#' Kilian, W., W.M. Getz, R. Zidon, M. Tsalyuk. 2018. Data from: Temporal
#' variation in resource selection of African elephants follows long term
#' variability in resource availability. Movebank Data Repository.
#' <doi:10.5441/001/1.3nj3qj45>
"elephant_ud"


#' Fisher utilization distributions
#'
#' A [RasterStack][raster::RasterStack-class] of [utilization
#' distributions](https://en.wikipedia.org/wiki/Utilization_distribution) for a
#' single Fisher in New York state over the course of nine nights in April 2011.
#' Cell values represent the probability density that the individual was found
#' at a given location between sunset and sunrise and the nine layers represent
#' nine nights of data. Utilization distributions were generated using the
#' `adehabitatHR` package from GPS tracking data.
#'
#' @source <https://datarepository.movebank.org/handle/10255/move.330>
#' @references
#' LaPoint, S., P. Gallery, M. Wikelski, R. Kays. 2013. Animal behavior,
#' cost-based corridor models, and real corridors. Landscape Ecology 28(8):
#' 1615–1630. <doi:10.1007/s10980-013-9910-0>
#'
#' LaPoint, S., P. Gallery, M. Wikelski, R. Kays. 2013. Data from: Animal behavior,
#' cost-based corridor models, and real corridors. Movebank Data Repository.
#' <doi:10.5441/001/1.2tp2j43g>
"fisher_ud"
