## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(ripums)

## ---- echo = FALSE-------------------------------------------------------
cps_ddi_file <- ripums_example("cps_00011.xml")
cps_data_file <- ripums_example("cps_00011.dat.gz")

## ---- eval = FALSE-------------------------------------------------------
#  # Change these filepaths to the filepaths of your downloaded extract
#  cps_ddi_file <- "C:/Users/My Name/My Documents/cps_00001.xml"
#  cps_data_file <- "C:/Users/My Name/My Documents/cps_00001.dat"

## ------------------------------------------------------------------------
cps_ddi <- read_ddi(cps_ddi_file) # Contains metadata, nice to have as separate object
cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)

## ------------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)

## ------------------------------------------------------------------------
# Can find on the website or from the data
ipums_val_labels(cps_ddi, FOODSTMP)

#    A: 0 = NIU, 1 = No, 2 = Yes

## ---- eval = FALSE-------------------------------------------------------
#  ipums_website(cps_ddi, "FOODSTMP")
#  
#  #    A: (Only available on website)
#  #       All interviewed households and group quarters.
#  #       Note the NIU on the codes page, this is a household variable and the
#  #       NIU cases are the vacant households.

## ------------------------------------------------------------------------
# We will be working with the FOODSTMP variable a lot, so 
# let's turn it into a factor
cps_data <- cps_data %>%
  mutate(FOODSTMP_factor = as_factor(FOODSTMP))

cps_data %>% 
  group_by(FOODSTMP_factor) %>%
  summarize(n_foodstmp = sum(WTSUPP)) %>%
  mutate(pct_foodstmp = n_foodstmp / sum(n_foodstmp))

#    A: 39,187,348

## ------------------------------------------------------------------------
#    A: 12.8% (found in code from previous question)

## ------------------------------------------------------------------------
cps_data %>% 
  group_by(SERIAL) %>%
  filter(row_number() == 1) %>%
  group_by(FOODSTMP_factor) %>%
  summarize(n_foodstmp = sum(HWTSUPP)) %>%
  mutate(pct_foodstmp = n_foodstmp / sum(n_foodstmp))

#    A: 12,855,283

## ------------------------------------------------------------------------
#    A: 10.7% (found in code from previous question)

## ---- eval = FALSE-------------------------------------------------------
#  ipums_website(cps_ddi, "EMPSTAT")
#  
#  #    A: Age 15+

## ------------------------------------------------------------------------
ipums_val_labels(cps_ddi, HEALTH)

#    A: 1 = Excellent, 2 = Very Good, 3 = Good, 4 = Fair, 5 = Poor

## ------------------------------------------------------------------------
cps_data %>%
  filter(HEALTH == 5) %>%
  summarize(emp_pct = weighted.mean(EMPSTAT == 10, WTSUPP))

#    A: 11.6%

## ------------------------------------------------------------------------
cps_data %>%
  filter(HEALTH == 2) %>%
  summarize(emp_pct = weighted.mean(EMPSTAT == 10, WTSUPP))

#    A: 51.6%

## ------------------------------------------------------------------------
ipums_val_labels(cps_ddi, EMPSTAT)

# 10 is the code for "At work"

pct_emp_by_health <- cps_data %>%
  filter(AGE >= 15) %>%
  mutate(HEALTH_factor = as_factor(HEALTH)) %>% 
  group_by(HEALTH_factor) %>%
  summarize(emp_pct = weighted.mean(EMPSTAT == 10, WTSUPP))

pct_emp_by_health

#    A: 11.8%

## ------------------------------------------------------------------------
#    A: 64.0% (found in code from previous question)

## ---- eval = FALSE-------------------------------------------------------
#  ipums_website(cps_ddi, "AHRSWORK")
#  
#  #     A: Civilians age 15+, at work last week

## ------------------------------------------------------------------------
avg_hrs_by_health <- cps_data %>% 
  filter(AGE >= 15 & AHRSWORKT < 999) %>%
  mutate(HEALTH_factor = as_factor(HEALTH)) %>% 
  group_by(HEALTH_factor) %>%
  summarize(mean_hours_worked = weighted.mean(AHRSWORKT, WTSUPP))

avg_hrs_by_health 

#     A: Excellent  38.4
#        Very good  38.7
#        Good       37.8
#        Fair       35.7
#        Poor       32.4

## ---- fig.height = 4, fig.width = 7--------------------------------------
library(ggplot2)

x_label <- ipums_var_label(cps_data, HEALTH)
source_info <- ipums_file_info(cps_ddi)$ipums_project

ggplot(pct_emp_by_health, aes(x = HEALTH_factor, y = emp_pct)) + 
  geom_bar(stat = "identity", fill = "#00263a") + 
  scale_x_discrete(x_label) + 
  scale_y_continuous("Percent employed", labels = scales::percent) + 
  labs(
    title = "Low Self-Reported Health Status Correlated with Unemployment", 
    subtitle = "Among age 15+ from CPS 2011 ASEC sample",
    caption = paste0("Source: ", source_info)
  )


## ---- fig.height = 6, fig.width = 7--------------------------------------
# Age is likely correlated with self-reported health and employment, so a good 
# analysis would control for this.

# One way to do so graphically is to make faceted plots by age group
pct_emp_by_health_age <- cps_data %>%
  filter(AGE >= 15) %>%
  mutate(
    AGE_factor = cut(
      AGE, 
      c(15, 25, 35, 45, 55, 65, max(AGE)), 
      c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
      include.lowest = TRUE
    ),
    HEALTH_factor = as_factor(HEALTH)
  ) %>% 
  group_by(HEALTH_factor, AGE_factor) %>%
  summarize(emp_pct = weighted.mean(EMPSTAT == 10, WTSUPP))

x_label <- ipums_var_label(cps_data, HEALTH)
source_info <- ipums_file_info(cps_ddi)$ipums_project

ggplot(pct_emp_by_health_age, aes(x = HEALTH_factor, y = emp_pct)) + 
  geom_bar(stat = "identity", fill = "#00263a") + 
  scale_x_discrete(x_label) + 
  scale_y_continuous("Percent employed", labels = scales::percent) + 
  facet_wrap(~AGE_factor, ncol = 2) + 
  labs(
    title = "Low Self-Reported Health Status Correlated with Unemployment", 
    subtitle = "Among age 15+ from CPS 2011 ASEC sample",
    caption = paste0("Source: ", source_info)
  )


