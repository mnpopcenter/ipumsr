
<!-- README.md is generated from README.Rmd. Please edit that file -->
ipumsimport
===========

The ipumsimport package helps import IPUMS extracts from the [website](https://www.ipums.org) into R. Currently it only works with microdata projects and has been tested with both rectangular and hierarchical data for CPS and IPUMS-I.

It can be installed by running the following commands:

``` r
if (!require(devtools)) install.packages("devtools")

if (Sys.info()$sysname == "Windows") {
  devtools::install_local("Z:/personal/gfellis/ipumsimport")
} else {
  devtools::install_local("/pkg/ipums/personal/gfellis/ipumsimport")
}
```

Examples
--------

``` r
library(ipumsimport)
library(mpctools)
library(haven)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

### CPS - Hierarchical Data

Relies on user downloading the .xml DDI file and the .dat/.dat.gz file (doesn't need to be unzipped).

``` r
data <- ip_read_data(mpc_root("personal/gfellis/ipumsimport_examples/cps_hier/cps_00004.xml"))
#> Users of IPUMS-CPS data must agree to abide by the conditions of use. A user's license is valid for one year and may be renewed.  Users must agree to the following conditions:
#> 
#> (1) No fees may be charged for use or distribution of the data.  All persons are granted a limited license to use these data, but you may not charge a fee for the data if you distribute it to others.
#> 
#> (2) Cite IPUMS appropriately.  For information on proper citation,  refer to the citation requirement section of this DDI document.
#> 
#> (3) Tell us about any work you do using the IPUMS.  Publications, research  reports, or presentations making use of IPUMS-CPS should be added to our  Bibliography. Continued funding for the IPUMS depends on our ability to  show our sponsor agencies that researchers are using the data for productive  purposes.
#> 
#> (4) Use it for GOOD -- never for EVIL.
#> 
#> Publications and research reports based on the IPUMS-CPS database must cite it appropriately. The citation should include the following:
#> 
#> Sarah Flood, Miriam King, Steven Ruggles, and J. Robert Warren. Integrated Public Use Microdata Series, Current Population Survey: Version 5.0 [dataset]. Minneapolis, MN: University of Minnesota, 2017. https://doi.org/10.18128/D030.V5.0
#> 
#> The licensing agreement for use of IPUMS-CPS data requires that users supply us with the title and full citation for any publications, research reports, or educational materials making use of the data or documentation. Please add your citation to the IPUMS bibliography: http://bibliography.ipums.org/
#> 
#> Reading data...
#> Parsing data...

cat(attr(data$TCIG100, "label_long"))
#> TCIG100 identifies individuals who had ever smoked 100 cigarettes in their lifetime.
#> 
#> This variable is part of the Tobacco Use Supplement [URL omitted from DDI.].
table(as_factor(data$TCIG100, levels = "both"))
#> 
#>           [1] No          [2] Yes     [96] Refused  [97] Don't know 
#>            53070            25674               87              288 
#> [98] No Response         [99] NIU 
#>                3            54471
```

### CPS - Rectangular Data

Relies on user downloading the .xml DDI file and the .dat/.dat.gz file (doesn't need to be unzipped).

``` r
data <- ip_read_data(mpc_root("personal/gfellis/ipumsimport_examples/cps_rect/cps_00003.xml"), verbose = FALSE)

cat(attr(data$TCIG100, "label_long"))
#> TCIG100 identifies individuals who had ever smoked 100 cigarettes in their lifetime.
#> 
#> This variable is part of the Tobacco Use Supplement [URL omitted from DDI.].
table(as_factor(data$TCIG100, levels = "both"))
#> 
#>           [1] No          [2] Yes     [96] Refused  [97] Don't know 
#>            53070            25674               87              288 
#> [98] No Response         [99] NIU 
#>                3            54471
```

### NHGIS

Relies on user downloading the csv file (with header row) and shape files.

``` r
data <- ip_read_nhgis(
  mpc_root("personal/gfellis/ipumsimport_examples/nhgis/nhgis0005_csv.zip"),
  mpc_root("personal/gfellis/ipumsimport_examples/nhgis/nhgis0005_shape.zip"),
  "state",
  "state",
  verbose = FALSE
)

data <- data %>%
  mutate(pct_slave = (ABO003 + ABO005) / (ABO001 + ABO002 + ABO003 + ABO004 + ABO005 + ABO006))

ggplot(data = data) + 
  geom_sf(aes(fill = pct_slave))
```

![](README-unnamed-chunk-6-1.png)

### Terrapop - Raster Data

Relies on zip file from extract

``` r
data <- ip_read_terra_raster(
  mpc_root("personal/gfellis/ipumsimport_examples/terra_raster/2552_bundle.zip"),
  "CROPLAND2000ZM2013"
)
#> Use of IPUMS Terra data is subject to conditions, including that publications and research which employ IPUMS Terra data should cite itappropiately. Please see www.terrapop.org for more information.

raster::plot(data)
```

![](README-unnamed-chunk-7-1.png)

### Terrapop - Area level data

Relies on zip file from extract (with boundary files for maps)

``` r
data <- ip_read_terra_area(
  mpc_root("personal/gfellis/ipumsimport_examples/terra_area/2553_bundle.zip")
)
#> An adapted version of the dataset, harmonized for international comparability, is available from IPUMS Terra (http://data.terrapop.org/) under the following conditions:
#> 
#>   
#> IPUMS Terra distributes integrated microdata of individuals and households only by agreement of collaborating national statistical offices and under the strictest of confidence. Before data may be distributed to an individual researcher, an electronic license agreement must be signed and approved.  To gain access to the data, a researcher must agree to the following:
#> 
#> (1) Implement security measures to prevent unauthorized access to census microdata. Under IPUMS Terra agreements with collaborating agencies, redistribution of the data to third parties is prohibited.
#> 
#> (2) Use the microdata for the exclusive purposes of scholarly research and education. Researchers must explicitly agree to not use microdata acquired for any commercial or income-generating venture.
#> 
#> (3) Maintain the confidentiality of persons, households, and other entities. Any attempt to ascertain the identity of persons or households from the microdata is prohibited. Alleging that a person or household has been identified is also prohibited.
#> 
#> (4) Report all publications based on these data to IPUMS Terra, which will in turn pass the information on to the relevant national statistical agencies.
#> 
#> Once a project is approved, a password is issued and data may be acquired through the Internet. Penalties for violating the license include: revocation of the license, recall of all microdata acquired, filing of a motion of censure to the appropriate professional organizations, and civil prosecution under the relevant national or international statutes.
#> 
#> These safeguards mirror the principles from the Joint ECE/Eurostat Work Session on Statistical Data Confidentiality. Employees of the Minnesota Population Center who work with the census microdata to produce the harmonized database also sign agreements to respect the confidentiality of the data.
#> 
#> 
#> Minnesota Population Center. Terra Populus [Machine-readable database]. Minneapolis: University of Minnesota, 2011.
#> 
#> Researchers should also acknowledge the statistical agency that originally produced the data.
#> 
#> 
#> The licensing agreement for use of IPUMS Terra data requires that users supply IPUMS Terra with the title and full citation for any publications, research reports, or educational materials making use of the data or documentation.
#> 
#> Copies of such materials are also gratefully received at terrapop@umn.edu.
#> 
#> Printed matter should be sent to:
#> IPUMS Terra
#> Minnesota Population Center
#> University of Minnesota
#> 50 Willey Hall
#> 225 19th Avenue South
#> Minneapolis, MN 55455

ggplot(data) +
  geom_sf(aes(fill = EDUCTERTIARY_GEO1_BR_BR2010A))
```

![](README-unnamed-chunk-8-1.png)

### Terrapop - Microlevel data

Relies on zip file from extract (with boundary files for maps) This file is huge, so won't run on a local machine, but I need IT's help getting some dependcies set-up to run this on the mpcstats servers.
