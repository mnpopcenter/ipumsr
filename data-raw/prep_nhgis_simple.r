# Make a simplified version of the NHGIS extract so it is easier to
# include in the package. It appears the shape files from R
# don't exactly match the ones from our extract engine
# (missing .shp.xml, sbx, and sbn files), but hopefully
# this is good enough.

# Relies on being at project root directory
# (And changes it to get around `zip`'s behavior of
# including directory structure in zip)
old_wd <- getwd()

make_square_around_centroid <- function(geo, dist) {
  out <- purrr::map(geo, function(x) {
    centroid <- (sf::st_centroid(x))
    points <- list(matrix(c(
      centroid + c(dist, dist), centroid + c(dist, -dist),
      centroid + c(-dist, -dist), centroid + c(-dist, dist),
      centroid + c(dist, dist)
    ), ncol = 2))

    sf::st_multipolygon(list(points))
  })
  attributes(out) <- attributes(geo)
  out
}

temp_dir <- file.path(tempdir(), "small_extract")
dir.create(temp_dir)
temp_dir_inner <- file.path(temp_dir, "shapes")
dir.create(temp_dir_inner)
temp_dir_out <- file.path(temp_dir, "nhgis0008_shape")
dir.create(temp_dir_out)

shp_data <- ripums::read_ipums_sf(file.path("data-raw/nhgis0008_shape.zip"))
shp_data$geometry <- make_square_around_centroid(shp_data$geometry, 20000)

shp_data <- as(shp_data, "Spatial")
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
