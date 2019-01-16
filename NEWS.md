# ipumsr 0.3.0.9000
* Fixed a bug when trying to set variable attributes but not value labels (#34).

# ipumsr 0.3.0
* Lots of improvements for users who wish to use "big data" sized IPUMS extracts. See 
  the vignette using command `vignette("ipums-bigdata", package = "ipusmr")` for
  the full details. 
  
  * There are now chunked versions of the microdata reading functions 
    which let you perform functions on subsets of the data as you read
    it in (`read_ipums_micro_chunked()` & `ipumsr::read_ipums_micro_list_chunked()`)
    
  * There is a new function `ipums_collect()` which combined `dplyr::collect()` with
    `set_ipums_attributes()` to add value and variable labels to data collected from
    a database.
    
  * When reading gzipped files, ipumsr no longer has to store the full text in memory.
  
* Added pillar printing for labelled classes in tibbles. This means that the 
  label will print the labels alongside the values when printed in a tibble 
  (in a subtle grey color when the terminal supports it). To turn this feature off,
  use command `options("ipumsr.show_pillar_labels" = FALSE).
  
* The approach to reading hierarchical data files is much faster.

* Arguments to `read_ipums_sp()` are now in the same order as `read_ipums_sf()`

* `read_ipums_sf()` and `read_ipums_sp()` gain 2 new arguments `vars` which 
  allows you to pick a subset of variables, and `add_layer_var` which lets 
  you add a variable indicating which layer it came from.

* You can now use your inside voice for variable names with the new argument
  `lower_vars` for `read_ipums_ddi()` and `read_ipums_micro()` family of functions
  so that the variable names are lower case.

* ipumsr is compatible with versions of haven newer than 2.0 (while maintaining 
  compatibility with earlier versions). (#31)

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
