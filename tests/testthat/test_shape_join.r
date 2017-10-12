context("ipums_shape_*_join work")

test_that("Basic join works (sf)", {
  data <- read_nhgis(ripums_example("nhgis0008_csv.zip"), verbose = FALSE)
  shape <- read_ipums_sf(ripums_example("nhgis0008_shape_small.zip"), verbose = FALSE)

  joined <- ipums_shape_inner_join(data, shape, by = "GISJOIN")

  expect_null(join_failures(joined))
  expect_equal(nrow(data), nrow(joined))
})

test_that("Basic join works (sp)", {
  data <- read_nhgis(ripums_example("nhgis0008_csv.zip"), verbose = FALSE)
  shape <- read_ipums_sp(ripums_example("nhgis0008_shape_small.zip"), verbose = FALSE)

  joined <- ipums_shape_inner_join(data, shape, by = "GISJOIN")

  expect_null(join_failures(joined))
  expect_equal(nrow(data), nrow(joined))
})

test_that("suffix argument works (sf)", {
  data <- read_nhgis(ripums_example("nhgis0008_csv.zip"), verbose = FALSE)
  shape <- read_ipums_sf(ripums_example("nhgis0008_shape_small.zip"), verbose = FALSE)

  data$test <- 1
  shape$test <- 2

  joined <- ipums_shape_inner_join(data, shape, by = "GISJOIN", suffix = c("_d", "_s"))
  expect_equal(data$test, joined$test_d)
  expect_equal(shape$test, joined$test_s)
})


test_that("complicated by works (sf)", {
  data <- read_nhgis(ripums_example("nhgis0008_csv.zip"), verbose = FALSE)
  shape <- read_ipums_sf(ripums_example("nhgis0008_shape_small.zip"), verbose = FALSE)

  joined_regular <- ipums_shape_inner_join(data, shape, by = "GISJOIN")

  data$join_split1 <- stringr::str_sub(data$GISJOIN, 1, 1)
  data$join_split2 <- as.numeric(stringr::str_sub(data$GISJOIN, 2, -1))

  shape$join_split1 <- stringr::str_sub(shape$GISJOIN, 1, 1)
  shape$join_split_xxx <- stringr::str_sub(shape$GISJOIN, 2, -1)
  shape$GISJOIN <- NULL

  joined <- ipums_shape_inner_join(
    data, shape, by = c("join_split1", "join_split2" = "join_split_xxx")
  )

  joined <- dplyr::select(joined, -dplyr::one_of("join_split1", "join_split2"))
  expect_true(identical(joined, joined_regular))
})

test_that("error for missing a by variable (sf)", {
  data <- read_nhgis(ripums_example("nhgis0008_csv.zip"), verbose = FALSE)
  shape <- read_ipums_sf(ripums_example("nhgis0008_shape_small.zip"), verbose = FALSE)

  shape$GISJOIN <- NULL
  expect_error(joined <- ipums_shape_inner_join(data, shape, by = "GISJOIN"))
})

# test_that("Join failures are mentioned", {
#
# })
