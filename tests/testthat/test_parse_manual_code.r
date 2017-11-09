context("Parsing manualcodes to labels")

test_that("Numeric parsing works on examples", {
  x <- c("1 = x", "2  y", "3 - z", "-4 = a", "abcd", "1962-1975 xyz")
  vt <- "numeric"
  expected <- dplyr::data_frame(
    val = c(1, 2, 3, -4),
    lbl = c("x", "y", "z", "a")
  )

  expect_equal(
    ipumsr:::parse_code_regex(x, vt),
    expected
  )
})

test_that("Character parsing works on examples", {
  x <- c("a = x", "abc - y", "ab de = z", "abcd", "1962-1975 xyz")
  vt <- "character"
  expected <- dplyr::data_frame(
    val = c("a", "abc"),
    lbl = c("x", "y")
  )

  expect_equal(
    ipumsr:::parse_code_regex(x, vt),
    expected
  )
})
