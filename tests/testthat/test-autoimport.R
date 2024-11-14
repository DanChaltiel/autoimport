

test_that("autoimport works", {
  ai = test_autoimport(files="sample_funs.R",
                       verbose=0) %>%
    suppressMessages()


  #*WARNING* loading a library before running tests manually can cause
  #namespace problems with additional imports. For instance, run `library(broom)`
  # session_info = attr(ai, "session_info")
  # expect_false("broom" %in% names(session_info$otherPkgs))


  #test attributes: attributes(ai) %>% names()
  review_dir = attr(ai, "review_dir")
  expect_true(dir.exists(review_dir))
  target_dir = attr(ai, "target_dir")
  target_file = path(target_dir, "sample_funs.R")
  expect_true(file_exists(target_dir))


  #test output
  out1 = readLines(target_file)

  #private functions, should not be imported
  expect_not_imported(out1, "dplyr", "mutate")
  expect_not_imported(out1, "dplyr", "filter")

  #explicit calls, should not be imported
  expect_not_imported(out1, "dplyr", "arrange")
  expect_not_imported(out1, "knitr", "asis_output")

  #base function, should not be imported
  expect_not_imported(out1, ".*", "sum")
  expect_not_imported(out1, ".* ", "date") #not lubridate (IMPORTLIST)

  #inner/private functions, should not be imported
  expect_not_imported(out1, "inner", ".*")
  expect_not_imported(out1, "autoimport_test", ".*")

  #other functions, should be imported
  expect_imported(out1, "purrr", "map")
  expect_imported(out1, "purrr", "set_names") #not rlang (IMPORTLIST)
  expect_imported(out1, "shiny", "div")
  expect_imported(out1, "tidyr", "pivot_longer")
  expect_not_imported(out1, "htmltools", "div")
  expect_not_imported(out1, "lubridate", "date")

  #leave trailing comment
  expect_in(c("#this is", "#a trailing comment"), out1)
})


