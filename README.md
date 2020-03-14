
# colorist

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mstrimas/colorist?branch=master&svg=true)](https://ci.appveyor.com/project/mstrimas/colorist)
[![Travis build
status](https://travis-ci.org/mstrimas/colorist.svg?branch=master)](https://travis-ci.org/mstrimas/colorist)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

Maps are essential tools for communicating information about wildlife
distributions in space and time. However, mapping these dynamic,
space-time distributions in a static map is challenging because **…
(Justin can you add something here? What’s the fundamental challenge
that this package is trying to address)**. `colorist` is designed to
address these challenges, providing a novel approach coloring and
visualizing wildlife distributions in space-time using raster data. In
addition to enabling display of sequential change in distributions
through the use of small multiples and animations, colorist provides
functions for extracting several features of interest from a sequence of
distributions and for visualizing those features using HCL
(hue-chroma-luminance) color palettes. The resulting maps allow for
“fair” visual comparison of intensity values (e.g., occurrence,
abundance, or density) across space and time and can be used to address
questions about where, when, and how consistently a species, group, or
individual is likely to be found.

## Installation

You can install the development version of colorist from
[GitHub](https://github.com/mstrimas/colorist) with:

``` r
# install.packages("remotes")
remotes::install_github("mstrimas/colorist")
```

## Background

**Justin: ** can you fill this in with a single paragraph giving some of
the necessary theoretical background, i.e. a bit about HCL color space
and how the metrics are mapped. We’ll also have a vignette where you can
add more information, this should just be the basics.

## Usage

`colorist` works on raster data stored as `RasterStack` objects
representing the distribution of either an individual animal or a
species. Each layer of the stack will typically represent the
distribution for a given point in time (e.g., layers may represent years
or seasons) or the distribution for different individuals. The basic
workflow for `colorist` is as follows:

1.  **Metrics:**
2.  **Color palette:**
3.  **Map:**
4.  **Legend:**

We can demonstrate this workflow using an example dataset of weekly
Field Sparrow occurrence probability estimates from [eBird Status &
Trends](https://ebird.org/science/status-and-trends).

``` r
library(colorist)

# load example data, field sparrow occurrence probability
data("fiespa_occ")

# calculate distribution metrics
r <- metrics_distill(fiespa_occ)
#> class      : RasterStack 
#> dimensions : 107, 177, 18939, 52  (nrow, ncol, ncell, nlayers)
#> resolution : 26665.26, 26665.28  (x, y)
#> extent     : -2682551, 2037200, -1447355, 1405830  (xmin, xmax, ymin, ymax)
#> crs        : +proj=laea +lat_0=38.7476367322638 +lon_0=-90.2379515912106 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
#> names      : X2018.01.04, X2018.01.11, X2018.01.18, X2018.01.25, X2018.02.01, X2018.02.08, X2018.02.15, X2018.02.22, X2018.03.01, X2018.03.08, X2018.03.15, X2018.03.22, X2018.03.29, X2018.04.05, X2018.04.12, ...

# generate hcl color palette
pal <- palette_timecycle(fiespa_occ)

# map
map_single(r, pal)
```

<img src="man/figures/README-elephant-1.png" width="100%" />

``` r

# legend
legend_timecycle(pal)
```

<img src="man/figures/README-elephant-2.png" width="100%" />
