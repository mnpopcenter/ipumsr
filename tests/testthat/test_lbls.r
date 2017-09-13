context("label helpers")

x <- haven::labelled(
  c(10, 10, 11, 20, 30, 99, 30, 10),
  c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
)
attr(x, "label") <- "Test label"

x_na <- haven::labelled(
  c(10, 10, 11, 20, NA, NA, NA, 10),
  c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20)
)
attr(x_na, "label") <- "Test label"

test_that("lbl_na_if: Simple example with both .lbl and .val keywords", {
  expect_equal(
    lbl_na_if(x, ~.val >= 90 | .lbl %in% c("Maybe")),
    x_na
  )
})

test_that("lbl_na_if: Respects environment", {
  upper_val <- 90
  expect_equal(
    lbl_na_if(x, ~.val >= upper_val | .lbl %in% c("Maybe")),
    x_na
  )
})

test_that("lbl_na_if: Explicit function", {
  expect_equal(
    lbl_na_if(x, function(.val, .lbl) .val >= 90 | .lbl %in% c("Maybe")),
    x_na
  )
})

test_that("lbl_na_if: Function name", {
  test_f <- function(.val, .lbl) .val >= 90 | .lbl %in% c("Maybe")
  expect_equal(
    lbl_na_if(x, "test_f"),
    x_na
  )
})

test_that("lbl_collapse: basic", {
  x_basic_collapse <- haven::labelled(
    c(10, 10, 10, 20, 30, 90, 30, 10),
    c(Yes = 10, No = 20, Maybe = 30, NIU = 90)
  )
  attr(x_basic_collapse, "label") <- "Test label"

  expect_equal(
    lbl_collapse(x, ~(.val %/% 10) * 10),
    x_basic_collapse
  )
})

test_that("lbl_collapse: if recoding to old value it is maintained (even if it's not first)", {
  x_basic_collapse <- haven::labelled(
    c(11, 11, 11, 20, 30, 99, 30, 11),
    c(`Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
  )
  attr(x_basic_collapse, "label") <- "Test label"

  expect_equal(
    lbl_collapse(x, ~ifelse(.val == 10, 11, .val)),
    x_basic_collapse
  )
})

# test_that("lbl_relabel: basic", {
#   x_basic_relabel <- haven::labelled(
#     c(10, 10, 10, 20, 30, 90, 30, 10),
#     c(Yes = 10, No = 20, `Garbage codes` = 90)
#   )
#   attr(x_basic_relabel, "label") <- "Test label"
#
#   expect_equal(
#     lbl_relabel(x, lbl(90, "Garbage codes") ~ .val == 99 | .lbl == "Maybe"),
#     x_basic_relabel
#   )
# })


#test_that("lbl_add: ")
