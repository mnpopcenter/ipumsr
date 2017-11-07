# Make a simplified version of the NHGIS extract so it is easier to
# include in the package. It appears the shape files from R
# don't exactly match the ones from our extract engine
# (missing .shp.xml, sbx, and sbn files), but hopefully
# this is good enough.

# This code is for the census tract files included in nhgis0024_shape.zip

# Preparation requires the rmapshaper package
# (available from CRAN, but not included in package dependencies)
library(rmapshaper)

# Relies on being at project root directory
# (And changes it to get around `zip`'s behavior of
# including directory structure in zip)
old_wd <- getwd()

temp_dir <- file.path(tempdir(), "small_extract")
dir.create(temp_dir)

temp_dir_out <- file.path(temp_dir, "nhgis0024_shape")
dir.create(temp_dir_out)

layers <- list(
  CT = list(
    inner_name = "nhgis0024_shapefile_tl2010_090_block_2010",
    shp_layer_name = "CT_block_2010"
  ),
  RI = list(
    inner_name = "nhgis0024_shapefile_tl2010_440_block_2010",
    shp_layer_name = "RI_block_2010"
  )
)

purrr::walk(layers, function(layer) {
  setwd(old_wd)
  temp_dir_inner <- file.path(temp_dir, layer$inner_name)
  dir.create(temp_dir_inner)

  shp_data <- ipumsr::read_ipums_sf(
    file.path("data-raw/nhgis0024_shape.zip"),
    shape_layer = dplyr::matches(layer$inner_name)
  )
  shp_data <- rmapshaper::ms_simplify(shp_data, keep = 0.01)

  shp_data <- as(shp_data, "Spatial")
  # Ignore warnings because of gdal bug
  # https://github.com/r-spatial/sf/issues/306
  # Couldn't suppress them with supressWarnings/suppressMessages nor purrr::quietly
  rgdal::writeOGR(
    obj = shp_data,
    dsn = temp_dir_inner,
    layer = layer$shp_layer_name,
    driver = "ESRI Shapefile"
  )

  # Files are in a weird double zip format
  setwd(temp_dir_inner)
  files <- dir(temp_dir_inner)
  zip(
    file.path(temp_dir_out, layer$inner_name),
    files
  )
})

setwd(temp_dir)
files <- file.path("nhgis0024_shape", dir("nhgis0024_shape/", recursive = TRUE))
zip(
  file.path(old_wd, "ipumsexamples/inst/extdata/nhgis0024_shape_small.zip"),
  files
)

setwd(old_wd)
