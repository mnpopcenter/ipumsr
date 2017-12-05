context("select-style functions")
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
      ipumsr:::select_var_rows(test_df, rlang::quo(c(RECTYPE, SEX))),
      dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
    )

    # String
    expect_equal(
      ipumsr:::select_var_rows(test_df, rlang::quo(c("RECTYPE", "SEX"))),
      dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
    )

    # variable from environment
    my_vars <- c("RECTYPE", "SEX")
    expect_equal(
      ipumsr:::select_var_rows(test_df, rlang::quo(my_vars)),
      dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
    )

    # dplyr::select helpers
    expect_equal(
      ipumsr:::select_var_rows(test_df, rlang::quo(starts_with("REP"))),
      dplyr::filter(test_df, var_name %in% c("REPWT1", "REPWT2"))
    )

    # NULL returns full dataframe
    expect_equal(
      ipumsr:::select_var_rows(test_df, rlang::quo(NULL)),
      test_df
    )
  }
)

test_that(
  "find_files_in respects dplyr-select style syntax",
  {
    # mpcstats doesn't have zip installed correctly...
    # So check and skip this test if zip doesn't run
    temp_dir_zip_test <- tempfile()
    dir.create(temp_dir_zip_test)
    file.create(file.path(temp_dir_zip_test, "test1.txt"))
    test_zip <- zip(temp_dir_zip_test, temp_dir_zip_test, flags = "-q")
    if (test_zip != 0) skip("zip doesn't work")

    # Again it's kind of weird that it takes quosures, but
    # since it's internal I think it's okay.

    # Make an example zip extract
    temp_dir <- tempfile()
    dir.create(temp_dir)
    file.create(file.path(temp_dir, "test1.txt"))
    file.create(file.path(temp_dir, "test2.txt"))
    file.create(file.path(temp_dir, "test.csv"))
    file.create(file.path(temp_dir, "abc.txt"))

    file_names <- c("test1.txt", "test2.txt", "test.csv", "abc.txt")
    zip_file <- file.path(temp_dir, "test.zip")

    # Mess with wd because zip includes unwanted folders otherwise
    old_wd <- getwd()
    setwd(temp_dir)

    zip(
      zip_file,
      files = file_names,
      flags = "-q"
    )
    setwd(old_wd)

    # Gets all files
    expect_equal(
      ipumsr:::find_files_in(zip_file, multiple_ok = TRUE),
      file_names
    )

    # Errors if multiple is not okay
    expect_error(
      ipumsr:::find_files_in(zip_file, multiple_ok = FALSE)
    )

    # Can filter on extension
    expect_equal(
      ipumsr:::find_files_in(zip_file, name_ext = "csv", multiple_ok = TRUE),
      "test.csv"
    )

    # Bare file names
    expect_equal(
      ipumsr:::find_files_in(
        zip_file,
        name_select = rlang::quo(c(test1.txt, test2.txt)),
        multiple_ok = TRUE
      ),
      c("test1.txt", "test2.txt")
    )

    # String
    expect_equal(
      ipumsr:::find_files_in(
        zip_file,
        name_select = rlang::quo(c("test1.txt", "test2.txt")),
        multiple_ok = TRUE
      ),
      c("test1.txt", "test2.txt")
    )

    # variable from environment
    my_vars <- c("test1.txt", "test2.txt")
    expect_equal(
      ipumsr:::find_files_in(
        zip_file,
        name_select = rlang::quo(my_vars),
        multiple_ok = TRUE
      ),
      c("test1.txt", "test2.txt")
    )


    # dplyr::select helpers
    expect_equal(
      ipumsr:::find_files_in(
        zip_file,
        name_select = rlang::quo(starts_with("test")),
        multiple_ok = TRUE
      ),
      c("test1.txt", "test2.txt", "test.csv")
    )
  }
)
