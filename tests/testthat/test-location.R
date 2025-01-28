

test_that("autoimport works at package level", {
  ai = test_autoimport(files="sample_funs.R",
                       location="package",
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
  expect_true(file_exists(target_dir))
  target_file = path(target_dir, "sample_funs.R")
  target_pkg_lvl_doc = path(target_dir, "autoimport_test-package.R")


  #test output
  out1 = readLines(target_file)
  out_pld = readLines(target_pkg_lvl_doc)

  #no imports at function-level documentation
  expect_not_imported(out1, ".*", ".*")

  expect_imported(out_pld, "purrr", "map")
  expect_imported(out_pld, "purrr", "set_names") #not rlang (IMPORTLIST)
  expect_imported(out_pld, "shiny", "div")
  expect_imported(out_pld, "tidyr", "pivot_longer")

  # import_review(review_dir, target_dir)
})


