context("label helpers: lbl_na_if")

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

test_that("Simple example with both .lbl and .val keywords", {
  expect_equal(
    lbl_na_if(x, ~.val >= 90 | .lbl %in% c("Maybe")),
    x_na
  )
})

test_that("Respects environment", {
  upper_val <- 90
  expect_equal(
    lbl_na_if(x, ~.val >= upper_val | .lbl %in% c("Maybe")),
    x_na
  )
})

test_that("Explicit function", {
  expect_equal(
    lbl_na_if(x, function(.val, .lbl) .val >= 90 | .lbl %in% c("Maybe")),
    x_na
  )
})

test_that("Function name", {
  test_f <- function(.val, .lbl) .val >= 90 | .lbl %in% c("Maybe")
  expect_equal(
    lbl_na_if(x, "test_f"),
    x_na
  )
})
