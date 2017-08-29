context("select_var_rows")
test_that(
  "select_var_rows respects dplyr-select style syntax",
  {
    # Note that select_var_rows takes the quosure as an argument
    # Seems okay because this is an internal function, but possibly
    # is a little weird.

    vars <- c("RECTYPE", "SEX", "REPWT1", "REPWT2")
    test_df <- dplyr::data_frame(
      var_name = vars,
      x = seq_along(vars)
    )

    # Bare column names
    expect_equal(
      ripums:::select_var_rows(test_df, rlang::quo(c(RECTYPE, SEX))),
      dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
    )

    # String
    expect_equal(
      ripums:::select_var_rows(test_df, rlang::quo(c("RECTYPE", "SEX"))),
      dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
    )

    # variable from environment
    my_vars <- c("RECTYPE", "SEX")
    expect_equal(
      ripums:::select_var_rows(test_df, rlang::quo(my_vars)),
      dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
    )

    # dplyr::select helpers
    expect_equal(
      ripums:::select_var_rows(test_df, rlang::quo(starts_with("REP"))),
      dplyr::filter(test_df, var_name %in% c("REPWT1", "REPWT2"))
    )

    # NULL returns full dataframe
    expect_equal(
      ripums:::select_var_rows(test_df, rlang::quo(NULL)),
      test_df
    )
  }
)
