
# colorist

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Package description

## Installation

You can install the development version of colorist from
[GitHub](https://CRAN.R-project.org) with:

``` r
install.packages("colorist")
```

``` r
# install.packages("remotes")
remotes::install_github("mstrimas/colorist")
```

## Usage

``` r
library(raster)
#> Loading required package: sp
library(colorist)

# load example data, elephant utlization distribution
data(elephant_ud)

# generate annual cyle metrics and a color palette
r <- annual_cycle_metrics(elephant_ud)
pal <- make_hcl_palette(elephant_ud)

# assign colors
rgb <- metrics_to_colors(r, pal)
plotRGB(rgb, scale = 1)
```

<img src="man/figures/README-usage-1.png" width="100%" />
