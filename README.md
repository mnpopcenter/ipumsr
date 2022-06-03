
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ipumsr <img src="man/figures/logo.png" align="right" height="149" width="128.5"/>

<!-- badges: start -->

[![Project
Status:Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ipumsr)](https://CRAN.R-project.org/package=ipumsr)
[![R build
status](https://github.com/ipums/ipumsr/workflows/R-CMD-check/badge.svg)](https://github.com/ipums/ipumsr/actions)
[![Codecov test
coverage](https://codecov.io/gh/ipums/ipumsr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ipums/ipumsr?branch=master)
<!-- badges: end -->

The ipumsr package helps import IPUMS extracts from the [IPUMS
website](https://www.ipums.org) into R. IPUMS provides census and survey
data from around the world integrated across time and space. IPUMS
integration and documentation makes it easy to study change, conduct
comparative research, merge information across data types, and analyze
individuals within family and community context. Data and services are
available free of charge.

The ipumsr package can be installed by running the following command:

``` r
install.packages("ipumsr")
```

Or, you can install the development version using the following
commands:

``` r
if (!require(devtools)) install.packages("devtools")

devtools::install_github("ipums/ipumsr")
```

## Learning More

The vignettes are a great place to learn more about ipumsr and IPUMS
data:

-   [See the **ipums** vignette for a general
    introduction](https://tech.popdata.org/ipumsr/articles/ipums.html)

-   For a more detailed look at some of the features, see these
    vignettes:

    -   [**value-labels**](https://tech.popdata.org/ipumsr/articles/value-labels.html)
        -   Provides guidance for using the value labels provided by
            IPUMS
    -   [**ipums-geography**](https://tech.popdata.org/ipumsr/articles/ipums-geography.html)
        -   Provides guidance for using R as GIS tool with IPUMS data
    -   [**ipums-bigdata**](https://tech.popdata.org/ipumsr/articles/ipums-bigdata.html)
        -   How to handle large IPUMS data extracts and examples of
            using the chunked versions of microdata reading functions.
    -   [**ipums-api**](https://tech.popdata.org/ipumsr/articles/ipums-api.html)
        -   How to use the IPUMS microdata extract API to define and
            submit extract requests, check extract status, and download
            extract files

-   Or to see examples of how to work through data from particular
    projects, see these vignettes:

    -   [**ipums-cps**](https://tech.popdata.org/ipumsr/articles/ipums-cps.html)
        -   An example of using CPS data with the ipumsr package
    -   [**ipums-nhgis**](https://tech.popdata.org/ipumsr/articles/ipums-nhgis.html)
        -   An example of using NHGIS data with the ipumsr package
    -   [**ipums-terra**](https://tech.popdata.org/ipumsr/articles/ipums-terra.html)
        -   An example of using IPUMS Terra data with the ipumsr package
    -   [The IPUMS website](https://www.ipums.org/support/exercises)
        -   For more project specific exercises

You can access them from R with the `vignette()` command (eg
`vignette("value-labels")`).

If you are installing from github and want the vignettes, you’ll need to
run the following commands first:

``` r
devtools::install_github("ipums/ipumsr/ipumsexamples")
devtools::install_github("ipums/ipumsr", build_vignettes = TRUE)
```

## Development

We greatly appreciate bug reports, suggestions or pull requests. They
can be submitted via github, on our [user
forum](https://forum.ipums.org) or by email to <ipums@umn.edu>

Before contributing, please be sure to read the [Contributing
Guidelines](https://github.com/ipums/ipumsr/blob/master/CONTRIBUTING.md)
and the [Code of
Conduct](https://github.com/ipums/ipumsr/blob/master/CONDUCT.md).
