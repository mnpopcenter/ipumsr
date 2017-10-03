## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
#     A: Fifteen (15)

## ------------------------------------------------------------------------
#     A: Population that is urban, particular ages, deaf and dumb, blind, and foreign born
#        not naturalized.

## ------------------------------------------------------------------------
#     A: Six (6)

## ------------------------------------------------------------------------
#     A: Nation, State, & County

## ------------------------------------------------------------------------
#     A: Persons

## ------------------------------------------------------------------------
#     A: It includes the counts of "white" persons, in addition to "colored" persons

## ------------------------------------------------------------------------
#     A: Percentage of total population in slavery, or ratio of slave:free population

## ------------------------------------------------------------------------
library(ripums)
library(sf)

## ---- echo = FALSE-------------------------------------------------------
nhgis_csv_file <- system.file("extdata", "nhgis0010_csv.zip", package = "ripumsexamples")
nhgis_shp_file <- system.file("extdata", "nhgis0010_shape.zip", package = "ripumsexamples")

## ---- eval = FALSE-------------------------------------------------------
#  # Change these filepaths to the filepaths of your downloaded extract
#  nhgis_csv_file <- "C:/Users/My Name/My Documents/nhgis0001_csv.zip"
#  nhgis_shp_file <- "C:/Users/My Name/My Documents/nhgis0001_shape.zip"

## ------------------------------------------------------------------------
nhgis_ddi <- read_ipums_codebook(nhgis_csv_file) # Contains metadata, nice to have as separate object
nhgis <- read_nhgis_sf(
  data_file = nhgis_csv_file,
  shape_file = nhgis_shp_file
)

## ------------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)

## ------------------------------------------------------------------------
length(table(nhgis$STATE))

#     A:  Twenty-Eight (28)

## ------------------------------------------------------------------------
table(nhgis$STATE)
#     A: In 1830, there were not any other states yet! Every decennial census is a 
#        historical snapshot, and NHGIS provides census counts just as they were 
#        originally reported without "filling in" any information for newer areas.

## ------------------------------------------------------------------------
nhgis <- nhgis %>%
  mutate(total_pop = ABO001 + ABO002 + ABO003 + ABO004 + ABO005 + ABO006)

nhgis %>%
  as.data.frame() %>%
  select(STATE, total_pop) %>%
  arrange(desc(total_pop)) %>%
  slice(1:5)

#     A: New  York

## ------------------------------------------------------------------------
nhgis <- nhgis %>%
  mutate(slave_pop = ABO003 + ABO004)

nhgis %>%
  as.data.frame() %>%
  select(STATE, slave_pop) %>%
  arrange(desc(slave_pop)) %>%
  slice(1:5)

#     A: Virginia 

## ------------------------------------------------------------------------
nhgis <- nhgis %>%
  mutate(pct_slave = slave_pop / total_pop)

nhgis %>%
  as.data.frame() %>%
  select(STATE, pct_slave) %>%
  filter(pct_slave %in% c(min(pct_slave, na.rm = TRUE), max(pct_slave, na.rm = TRUE)))

#     A: South Carolina (54.27%) and Vermont (0.00%)

## ------------------------------------------------------------------------
nhgis %>%
  as.data.frame() %>%
  filter(pct_slave > 0.5) %>%
  select(STATE, slave_pop, total_pop, pct_slave)

nhgis %>%
  as.data.frame() %>%
  filter(STATE %in% c("New York", "New Jersey")) %>%
  select(STATE, slave_pop, total_pop, pct_slave) 

#     A: Possibilities: Did you know some states had more slaves than free persons? Did
#        you know that some “free states” were home to substantial numbers of slaves?

## ------------------------------------------------------------------------
cat(ipums_file_info(nhgis_ddi)$conditions)

#     A: Minnesota Population Center. National Historical Geographic Information
#        System: Version 11.0 [Database]. Minneapolis: University of Minnesota. 2016.
#        http://doi.org/10.18128/D050.V11.0.

## ------------------------------------------------------------------------
#     A: (You can also send questions you may have about the site. We’re happy to help!)
#     nhgis@umn.edu

## ---- fig.height = 4, fig.width = 6--------------------------------------
# Note the function `geom_sf()` is a very new function, so you may need to update
# ggplot2 to run.
library(ggplot2)
if ("geom_sf" %in% getNamespaceExports("ggplot2")) {
  ggplot(data = nhgis, aes(fill = pct_slave)) +
    geom_sf() + 
    scale_fill_continuous("", labels = scales::percent) + 
    labs(
      title = "Percent of Population that was Enslaved by State",
      subtitle = "1830 Census",
      caption = paste0("Source: ", ipums_file_info(nhgis_ddi)$ipums_project)
    )
}

