
test_that("autoimport", {
  test_autoimport(files=test_path("source/sample_funs.R")) %>%
    suppressMessages()

  #test
  out1 = readLines(test_path("output/sample_funs.R"))

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




#TODO utiliser seulement ces packages
# cli,
# desc,
# devtools,
# diffviewer,
# digest,
# dplyr,
# glue,
# lifecycle,
# purrr,
# readr,
# rlang,
# rstudioapi,
# shiny,
# stringr,
# tibble,
# tidyr,
# utils,
# withr
