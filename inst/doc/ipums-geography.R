## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Don't give instructions about downloading data for yourself for geography data
# because some of the files (like the small shape file) aren't available.
ripumsexamples_file <- system.file("extdata", "nhgis0010_csv.zip", package = "ripumsexamples")

if (!file.exists(ripumsexamples_file)) {
  message(paste0(
    "Could not find data and so could not run vignette.\n\n",
    "The data is available on github - you can install it using the following ",
    "commands: \n",
    "  if (!require(devtools)) install.packages('devtools')\n",
    "  devtools::install_github('mnpopcenter/ripums/ripumsexamples')\n",
    "After installation, the data should be available for this vignette.\n\n"
  ))
  knitr::opts_chunk$set(eval = FALSE)
}

if (!suppressPackageStartupMessages(require(sf))) {
  message("Could not find sf package and so could not run vignette.")
  knitr::opts_chunk$set(eval = FALSE)
}

## ---- eval = FALSE-------------------------------------------------------
#  if (!require(devtools)) install.packages(devtools)
#  devtools::install_github("mnpopcenter/ripums/ripumsexamples")

## ------------------------------------------------------------------------
# This function helps find the data from the ripumsexamples package for us
ex_file <- function(x) {
  system.file("extdata", x, package = "ripumsexamples")
}

## ------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ripums)
  library(sf)
  library(dplyr)
  library(ggplot2)
})

# Load data
ipumsi_ddi_file <- ex_file("ipumsi_00011.xml")
ipumsi_ddi <- read_ipums_ddi(ipumsi_ddi_file)
ipumsi_data <- read_ipums_micro(ipumsi_ddi_file, verbose = FALSE)

# Load shape files
colombia_shape <- read_ipums_sf(ex_file("geo1_co1964_2005.zip"), verbose = FALSE)
ecuador_shape <- read_ipums_sf(ex_file("geo1_ec2010.zip"), verbose = FALSE)
peru_shape <- read_ipums_sf(ex_file("geo1_pe2007.zip"), verbose = FALSE)

## ------------------------------------------------------------------------------------
# Convert labelled values to factors where useful (and zap elsewhere)
# See the value-labels vignette for more on this process.
fuel_labels <- ipums_val_labels(ipumsi_data$FUELCOOK)
fuel_missing_lbls <- c(
  "NIU (not in universe)", "Multiple fuels", "Other combinations", "Other", "Unknown/missing"
)
fuel_solid_vals <- c(50:56, 61, 73, 74, 75)

ipumsi_data <- ipumsi_data %>%
  mutate_at(vars(COUNTRY, SAMPLE, AGE2), ~as_factor(lbl_clean(.))) %>%
  # We will get labels from shape file for geographic variables
  mutate_at(vars(starts_with("GEO")), zap_labels) %>% 
  mutate(
    SOLIDFUEL = FUELCOOK %>% 
      lbl_na_if(~.lbl %in% fuel_missing_lbls) %>%
      lbl_relabel(
        lbl(0, "Non-solid Fuel") ~ !.val %in% fuel_solid_vals,
        lbl(1, "Solid Fuel") ~ .val %in% fuel_solid_vals
      ) %>%
      as_factor(),
    FUELCOOK = as_factor(FUELCOOK)
  )

## ------------------------------------------------------------------------------------
ipumsi_summary <- ipumsi_data %>% 
  mutate(GEOLEV1 = case_when(
    COUNTRY == "Colombia" ~ GEOLEV1,
    COUNTRY == "Ecuador" ~ GEO1_EC2010,
    COUNTRY == "Peru" ~ GEO1_PE2007
  )) %>%
  group_by(YEAR, COUNTRY, GEOLEV1) %>%
  summarize(pct_solid = mean(SOLIDFUEL == "Solid Fuel", na.rm = TRUE))

## ------------------------------------------------------------------------------------
# Currently each shape file has different variable names
names(colombia_shape)
names(ecuador_shape)
names(peru_shape)

# Keep CNTRY_NAME (because the year-specific geographies are not unique across
# countries, so we need to merge using it), ADMIN_NAME (to get the name of 
# geography), and rename GEOLEVEL1, IPUM2010 and IPUM2007 to the same variable
# name (and geography, which contains the shape)
colombia_shape <- colombia_shape %>%
  select(CNTRY_NAME, ADMIN_NAME, GEOJOIN = GEOLEVEL1)
ecuador_shape <- ecuador_shape %>%
  select(CNTRY_NAME, ADMIN_NAME, GEOJOIN = IPUM2010)
peru_shape <- peru_shape %>%
  select(CNTRY_NAME, ADMIN_NAME, GEOJOIN = IPUM2007)

# Now we can rbind them together
all_shapes <- rbind(colombia_shape, ecuador_shape, peru_shape)

## ------------------------------------------------------------------------------------
ipumsi <- ipums_shape_inner_join(
  ipumsi_summary, 
  all_shapes,
  by = c("COUNTRY" = "CNTRY_NAME", "GEOLEV1" = "GEOJOIN")
)

## ------------------------------------------------------------------------------------
join_failures(ipumsi)

## ---- fig.height = 4, fig.width = 7--------------------------------------------------
# Note the function `geom_sf()` is a very new function, so you may need to update
# ggplot2 to run.
ipumsi <- ipumsi %>%
  mutate(census_round = cut(YEAR, c(1984, 1992, 2004, 2014), c("1985", "1993", "2005-2010")))

if ("geom_sf" %in% getNamespaceExports("ggplot2")) {
  ggplot(data = ipumsi, aes(fill = pct_solid)) +
    geom_sf() + 
    facet_wrap(~census_round) + 
    scale_fill_continuous("", labels = scales::percent) + 
    labs(
      title = "Percent of Children 0-5 Who Live in a Home That Cooks Using Solid Fuel",
      subtitle = "Colombia (1985, 1993, 2005), Ecuador (2010) and Peru (2007) Census Data",
      caption = paste0("Source: ", ipums_file_info(ipumsi_ddi)$ipums_project)
    )
}

## ------------------------------------------------------------------------------------
nhgis_ddi <- read_ipums_codebook(ex_file("nhgis0024_csv.zip")) 
nhgis <- read_nhgis_sf(
  data_file = ex_file("nhgis0024_csv.zip"),
  shape_file = ex_file("nhgis0024_shape_small.zip"),
  verbose = FALSE
)

## ---- fig.height = 4, fig.width = 7--------------------------------------------------
# The median age is 0 for unpopulated counties, set them to NA
nhgis <- nhgis %>%
  mutate(H77001 = ifelse(H77001 == 0, NA, H77001))

# For map filter to Hartford County, CT and Providence County, RI
nhgis_subset <- nhgis %>%
  filter(COUNTY %in% c("Hartford County", "Providence County")) %>%
  mutate(place_name = paste0(COUNTY, ", ", STATE))

if ("geom_sf" %in% getNamespaceExports("ggplot2")) {
  ggplot(data = nhgis_subset, aes(fill = H77001)) +
    geom_sf(linetype = "blank") + 
    scale_fill_continuous("") + 
    facet_wrap(~place_name, scales = "free") + 
    labs(
      title = "Median Age of Population By Census Block",
      subtitle = "2010 Census",
      caption = paste0(
        "Source: ", ipums_file_info(nhgis_ddi)$ipums_project, "\n",
        "Simplified Census Block boundaries (1% of points retained)"
      )
    )
}

