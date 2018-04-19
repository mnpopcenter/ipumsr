# ipumsr 0.2.0
* IPUMS Terra is now officially supported! Read raster, area or microdata extracts
  using functions `read_terra_raster()`, `read_terra_raster_list()`, 
  `read_terra_area()`, `read_terra_area_sf()`, and `read_terra_micro()`

* Add support for keyvar in DDI, which will (eventually) help link data across
  record types in hierarchical extracts. To be effective, this requires more 
  support on the ipums.org website, which is hopefully coming soon (#25 - thanks 
  @mpadge!)

* Improved main vignette instructions for Safari users (#27)

* Fix for selecting columns from csv extracts (#26 - thanks forum user JCambon_OIS!)

* Fixes for the `ipums_list_*()` family of functions.

# ipumsr 0.1.1

* Fixed a bug in ipums_shape_*_join functions when using integer ID columns. (#16)

* Allow for unzipped folders because Safari on macOS unzips folders by default (#17)

* lbl_relabel behavior is improved so that labels aren't assigned sequentially (#21)
