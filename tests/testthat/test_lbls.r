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

x_inc <- haven::labelled(
  c(100, 200, 105, 990, 999, 230),
  c(`Unknown` = 990, NIU = 999)
)
attr(x_inc, "label") <- "Test label"

x_unused <- haven::labelled(
  c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  c(Q1 = 1, Q2 = 2, Q3 = 3, Q4= 4)
)
attr(x_unused, "label") <- "Test label"

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
  x_collapse <- haven::labelled(
    c(10, 10, 10, 20, 30, 90, 30, 10),
    c(Yes = 10, No = 20, Maybe = 30, NIU = 90)
  )
  attr(x_collapse, "label") <- "Test label"

  expect_equal(
    lbl_collapse(x, ~(.val %/% 10) * 10),
    x_collapse
  )
})

test_that("lbl_collapse: if recoding to old value it is maintained (even if it's not first)", {
  x_collapse <- haven::labelled(
    c(11, 11, 11, 20, 30, 99, 30, 11),
    c(`Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
  )
  attr(x_collapse, "label") <- "Test label"

  expect_equal(
    lbl_collapse(x, ~ifelse(.val == 10, 11, .val)),
    x_collapse
  )
})

test_that("lbl_relabel: basic", {
  x_relabel <- haven::labelled(
    c(10, 10, 11, 20, 90, 90, 90, 10),
    c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, `Garbage codes` = 90)
  )
  attr(x_relabel, "label") <- "Test label"

  expect_equal(
    lbl_relabel(x, lbl(90, "Garbage codes") ~ .val == 99 | .lbl == "Maybe"),
    x_relabel
  )
})

test_that("lbl_relabel: multiple dots", {
  x_relabel <- haven::labelled(
    c(10, 10, 10, 20, 90, 90, 90, 10),
    c(`Yes/Yes-ish` = 10, No = 20, `???` = 90)
  )
  attr(x_relabel, "label") <- "Test label"

  expect_equal(
    lbl_relabel(
      x,
      lbl(10, "Yes/Yes-ish") ~ .val %in% c(10, 11),
      lbl(90, "???") ~ .val == 99 | .lbl == "Maybe"
    ),
    x_relabel
  )
})

test_that("lbl_relabel: to existing by just value", {
  x_relabel <- haven::labelled(
    c(10, 10, 10, 20, 30, 99, 30, 10),
    c(Yes = 10, No = 20, Maybe = 30, `NIU` = 99)
  )
  attr(x_relabel, "label") <- "Test label"

  expect_equal(
    lbl_relabel(x, 10 ~ .val == 11),
    x_relabel
  )
})

test_that("lbl_relabel: to existing by single unnamed argument to label", {
  x_relabel <- haven::labelled(
    c(10, 10, 10, 20, 30, 99, 30, 10),
    c(Yes = 10, No = 20, Maybe = 30, `NIU` = 99)
  )
  attr(x_relabel, "label") <- "Test label"

  expect_equal(
    lbl_relabel(x, lbl("Yes") ~ .val == 11),
    x_relabel
  )
})

test_that("lbl_relabel: to existing by single named argument to value", {
  x_relabel <- haven::labelled(
    c(10, 10, 10, 20, 30, 99, 30, 10),
    c(Yes = 10, No = 20, Maybe = 30, `NIU` = 99)
  )
  attr(x_relabel, "label") <- "Test label"

  expect_equal(
    lbl_relabel(x, lbl(.val = 10) ~ .val == 11),
    x_relabel
  )
})

test_that("lbl_relabel: error when to nonexisting by single unnamed argument", {
  expect_error(
    lbl_relabel(x, lbl(.val = -1) ~ .val == 11)
  )
})

test_that("lbl_relabel: error when to existing value with new label", {
  expect_error(
    lbl_relabel(x, lbl(10, "Yes also") ~ .val == 11)
  )
})

test_that("lbl_add: can add a single label", {
  x_newlabel <- haven::labelled(
    c(10, 10, 11, 20, 30, 99, 30, 10),
    c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, `New Label` = 90, NIU = 99)
  )
  attr(x_newlabel, "label") <- "Test label"
  expect_equal(
    lbl_add(x, lbl(90, "New Label")),
    x_newlabel
  )
})

test_that("lbl_add: can add more than one label", {
  x_newlabel <- haven::labelled(
    c(10, 10, 11, 20, 30, 99, 30, 10),
    c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, `New Label` = 90, `n2` = 91, NIU = 99)
  )
  attr(x_newlabel, "label") <- "Test label"

  expect_equal(
    lbl_add(x, lbl(90, "New Label"), lbl(91, "n2")),
    x_newlabel
  )
})

test_that("lbl_add: error if trying to add new label for old value", {
  expect_error(lbl_add(x, lbl(30, "New Label")))
})


test_that("lbl_add_vals: basic", {
  x_inc_new <- haven::labelled(
    c(100, 200, 105, 990, 999, 230),
    c(`$100` = 100, `$105` = 105, `Unknown` = 990, NIU = 999)
  )
  attr(x_inc_new, "label") <- "Test label"

  expect_equal(lbl_add_vals(x_inc, ~paste0("$", .), c(100, 105)), x_inc_new)
})


test_that("lbl_clean: basic", {
  x_used <- haven::labelled(
    c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    c(Q1 = 1, Q2 = 2, Q3 = 3)
  )
  attr(x_used, "label") <- "Test label"

  expect_equal(lbl_clean(x_unused), x_used)
})
