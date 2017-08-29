context("Converting rectypes")

test_that("Converting rectypes works.",
          expect_equal(
            ripums:::convert_rectype(
              c(`1` = "H", `2` = "P", `3` = "I"),
              c(1, 3, 3, 2, 1)
            ),
            c("H", "I", "I", "P", "H")
          )
)
