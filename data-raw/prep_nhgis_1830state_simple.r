# Make a simplified version of the NHGIS extract so it is easier to
# include in the package. It appears the shape files from R
# don't exactly match the ones from our extract engine
# (missing .shp.xml, sbx, and sbn files), but hopefully
# this is good enough.

# Preparation requires the rmapshaper package
# (available from CRAN, but not included in package dependencies)
library(rmapshaper)

# Relies on being at project root directory
# (And changes it to get around `zip`'s behavior of
# including directory structure in zip)
old_wd <- getwd()

temp_dir <- file.path(tempdir(), "small_extract")
dir.create(temp_dir)
temp_dir_inner <- file.path(temp_dir, "shapes")
dir.create(temp_dir_inner)
temp_dir_out <- file.path(temp_dir, "nhgis0008_shape")
dir.create(temp_dir_out)

shp_data <- ipumsr::read_ipums_sf(file.path("data-raw/nhgis0008_shape.zip"))
shp_data <- rmapshaper::ms_simplify(shp_data, keep = 0.01)

shp_data <- as(shp_data, "Spatial")
# Ignore warnings because of gdal bug
# https://github.com/r-spatial/sf/issues/306
# Couldn't suppress them with supressWarnings/suppressMessages nor purrr::quietly
rgdal::writeOGR(
  obj = shp_data,
  dsn = temp_dir_inner,
  layer = "US_pmsa_1990",
  driver = "ESRI Shapefile"
)

# Files are in a weird double zip format
setwd(temp_dir_inner)
files <- dir(temp_dir_inner)
zip(
  file.path(temp_dir_out, "nhgis0008_shapefile_tl2000_us_pmsa_1990.zip"),
  files
)

setwd(temp_dir)
files <- file.path("nhgis0008_shape", dir("nhgis0008_shape/", recursive = TRUE))
zip(
  file.path(old_wd, "inst/extdata/nhgis0008_shape_small.zip"),
  files
)

setwd(old_wd)
