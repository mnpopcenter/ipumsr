
<!-- README.md is generated from README.Rmd. Please edit that file -->
The ipumsimport package helps import IPUMS data into R using the fixed width files and the YML variable structure. Currently it only supports single rec type surveys, but it has worked with BRFSS, NSDUH and YRBSS data.

It can be installed by:

``` r
# install.packages("devtools")
devtools::install_local("Z:/personal/gfellis/ipumsimport")
```

There is an example BRFSS file in the package documentation. You can load it by:

``` r
library(ipumsimport)
# Unzip the example data
temp_dir <- tempdir()
unzip(system.file("brfss1984a.zip", package = "ipumsimport"), exdir = temp_dir)

brfss1984a <- read_ipums_output(
  paste0(temp_dir, "/brfss1984a_brfss.yml"), 
  paste0(temp_dir, "/brfss1984a_brfss.dat"),
  c(SEX, AGE, SMOKEV)
)

brfss1984a
#> # A tibble: 12,258 × 3
#>          AGE       SEX    SMOKEV
#>    <dbl+lbl> <dbl+lbl> <dbl+lbl>
#> 1         39         2         1
#> 2         44         2         1
#> 3         22         1         2
#> 4         64         2         1
#> 5         33         1         1
#> 6         35         1         2
#> 7         72         1         2
#> 8         71         2         1
#> 9         50         2         1
#> 10        47         1         1
#> # ... with 12,248 more rows
```

More details are in the ipumsimport vignette.

``` r
vignette("ipumsimport")
```
