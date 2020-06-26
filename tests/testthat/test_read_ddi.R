context("Read IPUMS DDI file")

test_that("can read DDI with labeled string variable", {
  ddi <- read_ipums_ddi(ipums_example("mtus_00002.xml"))
  sample_val_labels <- ddi$var_info[
    ddi$var_info$var_name == "SAMPLE",
    "val_labels",
    drop = TRUE
  ][[1]]
  expect_type(sample_val_labels$val, "character")
})
