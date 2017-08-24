## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE-------------------------------------------------------
#  vignette("ipums-cps", package = "ripums")
#  vignette("ipums-nhgis", package = "ripums")

## ---- eval = FALSE-------------------------------------------------------
#  if (!require(devtools)) install.packages("devtools")
#  devtools::install_github("mnpopcenter/ripumstest")

## ------------------------------------------------------------------------
library(ripums)
library(dplyr, warn.conflicts = FALSE)

# Note that you can pass in the loaded DDI into the `read_ipums_micro()`
cps_ddi <- read_ddi(ripums_example("cps_00011.xml"))
cps_data <- read_ipums_micro(cps_ddi, verbose = FALSE)

# Show which variables have labels
cps_data %>%
  select_if(is.labelled)

# Notice how the tibble print function shows the dbl+lbl class on top

# Investigate labels
ipums_val_labels(cps_data$FOODSTMP)

# Convert the labels to factors exactly
cps_data <- cps_data %>%
  mutate(HEALTH_factor = as_factor(HEALTH))

table(cps_data$HEALTH_factor, useNA = "always")

## ---- error = TRUE-------------------------------------------------------
# Manipulating the labelled value before as_factor 
# often leads to losing the information...
# Say we want to set HEALTH == 1 to missing
cps_data <- cps_data %>%
  mutate(HEALTH_factor2 = as_factor(ifelse(HEALTH == 1, NA, HEALTH)))

## ------------------------------------------------------------------------
# Currently the best solution is to convert to factor first, then manipulate using
# factor. We hope to improve this.
cps_data <- cps_data %>%
  mutate(HEALTH_factor2 = as_factor(HEALTH)) %>%
  mutate(HEALTH_factor2 = ifelse(HEALTH_factor2 == "Poor", NA, HEALTH_factor2))

table(cps_data$HEALTH_factor2, useNA = "always")

## ------------------------------------------------------------------------
library(ripums)
library(dplyr, warn.conflicts = FALSE)

# Note that you can pass in the loaded DDI into the `read_ipums_micro()`
cps_ddi <- read_ddi(ripums_example("cps_00011.xml"))
cps_data <- read_ipums_micro(cps_ddi, verbose = FALSE)

# Currently file level metadata is on cps_data
ipums_file_info(cps_data)$ipums_project

# But after a mutate, it is lost
cps_data <- cps_data %>%
  mutate(MONTH = haven::as_factor(MONTH))
ipums_file_info(cps_data)$ipums_project

# So you can use the DDI
ipums_file_info(cps_ddi)$ipums_project

## ------------------------------------------------------------------------
library(ripums)
library(dplyr, warn.conflicts = FALSE)

# The vars argument for `read_ipums_micro` uses this syntax
# So these are all equivalent
cf <-  ripums_example("cps_00011.xml")
read_ipums_micro(cf, vars = c("YEAR", "AGE"), verbose = FALSE) %>%
  names()

read_ipums_micro(cf, vars = c(YEAR, AGE), verbose = FALSE)%>%
  names()

read_ipums_micro(cf, vars = c(one_of("YEAR"), starts_with("AG")), verbose = FALSE) %>%
  names()

# `data_layer` and `shape_layer` arguments to `read_nhgis()` and terra functions
# also use it.
# (Sometimes extracts have multiple files, though all examples only have one)
nf <- ripums_example("nhgis0008_csv.zip")
ipums_list_files(nf)

ipums_list_files(nf, data_layer = "nhgis0008_csv/nhgis0008_ds135_1990_pmsa.csv")

ipums_list_files(nf, data_layer = contains("ds135"))

## ------------------------------------------------------------------------
library(ripums)
library(dplyr, warn.conflicts = FALSE)

# Long data
cps <- read_ipums_micro(
  ripums_example("cps_00010.xml"),
  data_structure = "long",
  verbose = FALSE
)

cps

# List data
cps <- read_ipums_micro(
  ripums_example("cps_00010.xml"),
  data_structure = "list",
  verbose = FALSE
)

cps$P

cps$H

