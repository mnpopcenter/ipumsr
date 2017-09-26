## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(ripums)
ddi <- read_ipums_ddi(ripums_example("cps_00015.xml"))
cps <- read_ipums_micro(ddi, verbose = FALSE)

cps

## ------------------------------------------------------------------------
is.labelled(cps$STATEFIP)
sapply(cps, is.labelled)

## ------------------------------------------------------------------------
# Printing the variable directly (or a subset)
head(cps$MONTH)

# Just get the labels
ipums_val_labels(cps$MONTH)

# or if you're working interactively you can use ipums_view
# ipums_view(ddi)

## ------------------------------------------------------------------------
head(cps$AGE)

cps$AGE_FACTOR <- as_factor(cps$AGE)
head(cps$AGE_FACTOR)

## ------------------------------------------------------------------------
mean(cps$AGE)

# mean(cps$AGE_FACTOR) # error because data is a factor not numeric

mean(as.numeric(cps$AGE_FACTOR)) # A common mistake

# The "more" correct way, but NA because of the text labels
mean(as.numeric(as.character(cps$AGE_FACTOR)))

## ------------------------------------------------------------------------
ipums_val_labels(cps$HEALTH)

HEALTH2 <- ifelse(cps$HEALTH > 3, 3, cps$HEALTH)
ipums_val_labels(HEALTH2)

## ------------------------------------------------------------------------
ipums_val_labels(cps$HEALTH)
cps$HEALTH <- as_factor(cps$HEALTH)

## ------------------------------------------------------------------------
cps$ASECFLAG <- as_factor(cps$ASECFLAG)
cps$MONTH <- as_factor(cps$MONTH)

## ------------------------------------------------------------------------
cps$AGE <- zap_labels(cps$AGE)

## ------------------------------------------------------------------------
ipums_val_labels(cps$STATEFIP)
cps$STATEFIP <- lbl_clean(cps$STATEFIP)

ipums_val_labels(cps$STATEFIP)
cps$STATEFIP <- as_factor(cps$STATEFIP)

## ------------------------------------------------------------------------
# Caution: R defaults to printing large numbers like 99999999 in rounded 
# exponential format (1e+08) but that's not how they are actually stored
ipums_val_labels(cps$INCTOT)

# All of these are equivalent
INCTOT1 <- lbl_na_if(cps$INCTOT, ~.val >= 99999990)
INCTOT2 <- lbl_na_if(cps$INCTOT, ~.lbl %in% c("Missing.", "N.I.U. (Not in Universe)."))
INCTOT3 <- lbl_na_if(cps$INCTOT, function(.val, .lbl) {
  is_missing <- .val == 99999998
  is_niu <- .lbl == "N.I.U. (Not in Universe)."
  return(is_missing | is_niu)
})

# Change to a factor in the original cps data.frame
cps$INCTOT <- lbl_na_if(cps$INCTOT, ~.val >= 9999990)
cps$INCTOT <- as_factor(cps$INCTOT)

## ------------------------------------------------------------------------
ipums_val_labels(cps$EDUC)
# %/% is integer division, which divides by the number but doesn't keep the remainder
cps$EDUC <- lbl_collapse(cps$EDUC, ~.val %/% 10)

ipums_val_labels(cps$EDUC)

cps$EDUC <- as_factor(cps$EDUC)

## ------------------------------------------------------------------------
ipums_val_labels(cps$MIGRATE1)

cps$MIGRATE1 <- lbl_relabel(
  cps$MIGRATE1,
  lbl(0, "NIU / Missing / Unknown") ~ .val %in% c(0, 2, 9),
  lbl(1, "Stayed in state") ~ .val %in% c(1, 3, 4)
)

ipums_val_labels(cps$MIGRATE1)

cps$MIGRATE1 <- as_factor(cps$MIGRATE1)

## ------------------------------------------------------------------------
x <- haven::labelled(
  c(100, 200, 105, 990, 999, 230),
  c(`Unknown` = 990, NIU = 999)
)

lbl_add(x, lbl(100, "$100"), lbl(105, "$105"), lbl(200, "$200"), lbl(230, "$230"))
lbl_add_vals(x, ~paste0("$", .))

## ------------------------------------------------------------------------
# Reload cps data so that INCTOT is a labelled class again
cps <- read_ipums_micro(ddi, verbose = FALSE)

# Try to set all values above 1000000 to NA
test1 <- lbl_na_if(cps$INCTOT, ~.val > 1000000)
test1 <- zap_labels(test1)
max(test1, na.rm = TRUE)
# Didn't work

## ------------------------------------------------------------------------
test2 <- lbl_add_vals(cps$INCTOT)
test2 <- lbl_na_if(test2, ~.val > 1000000)
test2 <- zap_labels(test2)
max(test2, na.rm = TRUE)

