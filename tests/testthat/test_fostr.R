context("fostr - stringr replacements")

test_that("fostr_detect works as expected", {
  expect_equal(ipumsr:::fostr_detect(c("abcdef", "xyz", "def abc"), "abc"), c(TRUE, FALSE, TRUE))
})

test_that("fostr_named_capture works as expected", {
  expect_equal(
    ipumsr:::fostr_named_capture(
      c("this is a bike", "this is cool", "these are fine"),
      "^this is( a)? (?<var>.+)$"
    ),
    dplyr::tibble(var = c("bike", "cool", ""))
  )

  expect_equal(
    ipumsr:::fostr_named_capture(
      c("this is a bike", "this is cool", "these are fine"),
      "^this is( a)? (?<var>.+)$",
      TRUE
    ),
    dplyr::tibble(var = c("bike", "cool"))
  )

  expect_equal(
    ipumsr:::fostr_named_capture(
      c("this is a bike", "this is cool", "these are fine"),
      "^(?<article>this) is( a)? (?<var>.+)$"
    ),
    dplyr::tibble(article = c("this", "this", ""), var = c("bike", "cool", ""))
  )
})

test_that("fostr_named_capture_single works as expected", {
  expect_equal(
    ipumsr:::fostr_named_capture_single(
      c("this is a bike", "this is cool", "these are fine"),
      "^this is( a)? (?<var>.+)$"
    ),
     c("bike", "cool", "")
  )
})

test_that("fostr_replace works as expected", {
  expect_equal(ipumsr:::fostr_replace("abcdef", "b", "x"), "axcdef")
  expect_equal(ipumsr:::fostr_replace("abcdef", "[bf]", "x"), "axcdef")
})

test_that("fostr_replace_all works as expected", {
  expect_equal(ipumsr:::fostr_replace_all("abcdef", "b", "x"), "axcdef")
  expect_equal(ipumsr:::fostr_replace_all("abcdef", "[bf]", "x"), "axcdex")
})

test_that("fostr_split works as expected", {
  expect_equal(ipumsr:::fostr_split("this is cool", " ")[[1]], c("this", "is", "cool"))
})

test_that("fostr_sub works as expected", {
  expect_equal(ipumsr:::fostr_sub("abcdef", 3), "cdef")
  expect_equal(ipumsr:::fostr_sub("abcdef", end = 3), "abc")
  expect_equal(ipumsr:::fostr_sub("abcdef", 2, 4), "bcd")
})

test_that("fostr_subset works as expected", {
  expect_equal(ipumsr:::fostr_subset(c("abcdef", "xyz", "def abc"), "abc"), c("abcdef", "def abc"))
})

test_that("fostr_wrap works as expected", {
  expect_equal(ipumsr:::fostr_wrap("ab cdef", 3), "ab\ncdef")
})
