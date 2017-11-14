
<!-- README.md is generated from README.Rmd. Please edit that file -->
ipumsr
======

[![Project Status:Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ipumsr)](http://cran.r-project.org/web/packages/ipumsr) [![Travis-CI Build Status](https://travis-ci.org/mnpopcenter/ipumsr.svg?branch=master)](https://travis-ci.org/mnpopcenter/ipumsr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mnpopcenter/ipumsr?branch=master&svg=true)](https://ci.appveyor.com/project/mnpopcenter/ipumsr) [![Coverage Status](https://codecov.io/gh/mnpopcenter/ipumsr/master.svg)](https://codecov.io/github/mnpopcenter/ipumsr?branch=master)

The ipumsr package helps import IPUMS extracts from the [IPUMS website](https://www.ipums.org) into R. We hope to post a more stable version on CRAN soon.

The ipumsr package is now on CRAN can be installed by running the following command:

``` r
install.packages("ipumsr")
```

Or, you can install the development version using the following commands:

``` r
if (!require(devtools)) install.packages("devtools")

devtools::install_github("mnpopcenter/ipumsr")
```

Vignettes
---------

There are several vignettes included in the package:

-   **ipums** - Provides general information about using the ipumsr package
-   **value-labels** - Provides guidance for using the value labels provided by IPUMS
-   **ipums-geography** - Provides guidance for using R as GIS tool with IPUMS data
-   **ipums-cps** - An example of using CPS data with the ipumsr package
-   **ipums-nghis** - An example of using NHGIS data with the ipumsr package

You can access them with the `vignette()` command (eg `vignette("value-labels")`).

If you are installing from github and want the vignettes, you'll need to run the following commands first:

``` r
devtools::install_github("mnpopcenter/ipumsr/ipumsexamples")
devtools::install_github("mnpopcenter/ipumsr", build_vignettes = TRUE)
```

Examples
--------

``` r
suppressPackageStartupMessages({
  library(ipumsr)
  library(haven)
  library(ggplot2) # ggplot2 version > 2.2.1 (development version as of 8/15/2017)
  library(dplyr)
  library(sf)
})
```

### CPS - Hierarchical Data

Relies on user downloading the .xml DDI file and the .dat/.dat.gz file (doesn't need to be unzipped).

``` r
# Use example file included with package:
cps_hier_file <- ipums_example("cps_00010.xml")
ddi <- read_ipums_ddi(cps_hier_file)
data <- read_ipums_micro(ddi)
#> Users of IPUMS-CPS data must agree to abide by the conditions of use. A user's
#> license is valid for one year and may be renewed. Users must agree to the
#> following conditions:
#> 
#> (1) No fees may be charged for use or distribution of the data. All persons are
#> granted a limited license to use these data, but you may not charge a fee for
#> the data if you distribute it to others.
#> 
#> (2) Cite IPUMS appropriately. For information on proper citation, refer to the
#> citation requirement section of this DDI document.
#> 
#> (3) Tell us about any work you do using the IPUMS. Publications, research
#> reports, or presentations making use of IPUMS-CPS should be added to our
#> Bibliography. Continued funding for the IPUMS depends on our ability to show our
#> sponsor agencies that researchers are using the data for productive purposes.
#> 
#> (4) Use it for GOOD -- never for EVIL.
#> 
#> Publications and research reports based on the IPUMS-CPS database must cite it
#> appropriately. The citation should include the following:
#> 
#> Sarah Flood, Miriam King, Steven Ruggles, and J. Robert Warren. Integrated
#> Public Use Microdata Series, Current Population Survey: Version 5.0 [dataset].
#> Minneapolis, MN: University of Minnesota, 2017. https://doi.org/10.18128/
#> D030.V5.0
#> 
#> The licensing agreement for use of IPUMS-CPS data requires that users supply
#> us with the title and full citation for any publications, research reports, or
#> educational materials making use of the data or documentation. Please add your
#> citation to the IPUMS bibliography: http://bibliography.ipums.org/
#> 
#> Reading data...
#> Parsing data...

# Variable description for the month variable
cat(ipums_var_desc(ddi, MONTH))
#> MONTH indicates the calendar month of the CPS interview.

# Hierarachical data loaded as a data frame


# Value labels loaded as haven::labelled class
# Convert to factors with `as_factor`
table(as_factor(data$MONTH, levels = "both"))
#> 
#>   [1] January  [2] February     [3] March     [4] April       [5] May 
#>             0             0          3385             0             0 
#>      [6] June      [7] July    [8] August [9] September  [10] October 
#>             0             0             0             0             0 
#> [11] November [12] December 
#>             0             0

# Can also load as a list by rectype
data <- read_ipums_micro_list(ddi, verbose = FALSE)
# Household data
data$HOUSEHOLD
#> # A tibble: 3,385 x 6
#>      RECTYPE  YEAR SERIAL HWTSUPP  STATEFIP     MONTH
#>    <chr+lbl> <dbl>  <dbl>   <dbl> <int+lbl> <int+lbl>
#>  1         H  1962     80 1475.59        55         3
#>  2         H  1962     82 1597.61        27         3
#>  3         H  1962     83 1706.65        27         3
#>  4         H  1962     84 1790.25        27         3
#>  5         H  1962    107 4355.40        19         3
#>  6         H  1962    108 1479.05        19         3
#>  7         H  1962    122 3602.75        27         3
#>  8         H  1962    124 4104.41        55         3
#>  9         H  1962    125 2182.17        55         3
#> 10         H  1962    126 1826.38        55         3
#> # ... with 3,375 more rows
# Person data
data$PERSON
#> # A tibble: 7,668 x 6
#>      RECTYPE  YEAR SERIAL PERNUM  WTSUPP    INCTOT
#>    <chr+lbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl+lbl>
#>  1         P  1962     80      1 1475.59      4883
#>  2         P  1962     80      2 1470.72      5800
#>  3         P  1962     80      3 1578.75  99999998
#>  4         P  1962     82      1 1597.61     14015
#>  5         P  1962     83      1 1706.65     16552
#>  6         P  1962     84      1 1790.25      6375
#>  7         P  1962    107      1 4355.40  99999999
#>  8         P  1962    107      2 1385.81         0
#>  9         P  1962    107      3 1629.10       600
#> 10         P  1962    107      4 1432.24  99999999
#> # ... with 7,658 more rows
```

### CPS - Rectangular Data

Relies on user downloading the .xml DDI file and the .dat/.dat.gz file (doesn't need to be unzipped).

``` r
# Use example file included with package
cps_rect_file <- ipums_example("cps_00006.xml")
data <- read_ipums_micro(cps_rect_file, verbose = FALSE)

# While working interactively, can get convenient display of variable information
# in RStudio's viewer
ipums_view(data)
```

### NHGIS

Relies on user downloading the csv file (with or without header row) and shape files (doesn't need to be unzipped).

Note that to save space when including this data on CRAN, the shape file has been reduced to 1% of the points in the polygon of the PMSA. The original shape file can be found in the `ipumsexamples` package.

``` r
data <- read_nhgis_sf(
  ipums_example("nhgis0008_csv.zip"),
  shape_file = ipums_example("nhgis0008_shape_small.zip"),
  verbose = FALSE
)

ipums_var_info(data, starts_with("D6Z"))
#> # A tibble: 8 x 4
#>   var_name          var_label                   var_desc       val_labels
#>      <chr>              <chr>                      <chr>           <list>
#> 1   D6Z001 1989 to March 1990 Year Structure Built (D6Z) <tibble [0 x 2]>
#> 2   D6Z002       1985 to 1988 Year Structure Built (D6Z) <tibble [0 x 2]>
#> 3   D6Z003       1980 to 1984 Year Structure Built (D6Z) <tibble [0 x 2]>
#> 4   D6Z004       1970 to 1979 Year Structure Built (D6Z) <tibble [0 x 2]>
#> 5   D6Z005       1960 to 1969 Year Structure Built (D6Z) <tibble [0 x 2]>
#> 6   D6Z006       1950 to 1959 Year Structure Built (D6Z) <tibble [0 x 2]>
#> 7   D6Z007       1940 to 1949 Year Structure Built (D6Z) <tibble [0 x 2]>
#> 8   D6Z008    1939 or earlier Year Structure Built (D6Z) <tibble [0 x 2]>

data <- data %>%
  mutate(
    pct_before_1950 = (D6Z007 + D6Z008) / 
           (D6Z001 + D6Z002 + D6Z003 + D6Z004 + D6Z005 + D6Z006 + D6Z007 + D6Z008)
  )

# Note the function `geom_sf()` is currently only in the development version, 
# so you may need to update ggplot2 to run using 
#   devtools::install_github("tidyverse/ggplot2")
if ("geom_sf" %in% getNamespaceExports("ggplot2")) {
  ggplot(data = data) + 
    geom_sf(aes(fill = pct_before_1950)) + 
    labs(
      title = "Percent of homes built before 1950", 
      subtitle = "By Primary Metropolitan Statistical Area in 1990 Census", 
      caption = "Simplified PMSA boundaries (1% of polygon points retained)"
    )
}
```

![](man/figures/README-nhgis_map-1.png)

### Terrapop

There is experimental support for for loading terrapop data, but examples are too large to include in the package.

``` r
# Raster data
data <- ipumsr:::read_terra_raster(
  "2552_bundle.zip",
  "CROPLAND2000ZM2013.tiff",
  verbose = FALSE
)

# Area data
data <- ipumsr:::read_terra_area(
  "2553_bundle.zip",
  verbose = FALSE
)

# Microdata
data <- ipumsr:::read_terra_micro(
  "2554_bundle.zip",
  verbose = FALSE
)
```
