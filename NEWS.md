# ipumsr 0.4.5

* Fixed bug causing a read error for some labeled string variables (#61, thanks 
  @chengchou).
  
* ipumsr now always uses the `haven::labelled()` function to create `labelled`
  vectors, in order to maintain compatibility with developments in the haven and 
  vctrs packages (thanks @gergness!).

# ipumsr 0.4.4

* Modify `lbl_define()` test to reflect changes to haven's `labelled` class 
  definition.

# ipumsr 0.4.3

* Add `lbl_define()` function to enable the use of `lbl_relabel()` syntax when  
  creating a new labelled vector from an unlabelled one (#51, thanks 
  @chengchou).
  
* Remove pillar printing from ipumsr, getting rid of pesky warning (#47).

* Improved documentation for `lower_vars` argument (#56, thanks @hrecht).

# ipumsr 0.4.2

* Incorporate bug fix in knitr 1.23 that affected encoding in NHGIS vignette.

# ipumsr 0.4.1
* Remove stringr & tidyr dependencies so installation is a little easier (#41).

* Fix bug in pillar printing of haven's `labelled` objects (#43)
 
# ipumsr 0.4.0
* Add `read_ipums_micro_yield()` and `read_ipums_micro_list_yield()` that 
  read data in 'yields', a concept similar to 'chunks', but with a little
  more flexibility. See the big data vignette 
  (`vignette("ipums-bigdata", package = "ipumsr")`) for more details.

* Fixed a bug when trying to set variable attributes but not value labels (#34).

* Fixed a bug where implicit decimals would be double counted for csv files.

* Argument `rectype_convert` has been removed because it no longer did anything.

* Fixed a typo in vignette "ipums-geography" (#37, @jacobkap).

* Creates a pkgdown site (#38, @jacobkap).

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
