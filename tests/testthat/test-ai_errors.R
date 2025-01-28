
test_that("autoimport warnings", {
  ai = test_autoimport(files="sample_funs2.R") %>%
    suppressMessages() %>%
    expect_classed_conditions(warning_class=c("autoimport_duplicate_warn",
                                              "autoimport_fun_not_in_desc_warn",
                                              "autoimport_fun_not_found_warn"))

  target_dir = attr(ai, "target_dir")
  target_file = path(target_dir, "sample_funs2.R")
  expect_true(file_exists(target_dir))

  #test output
  out1 = readLines(target_file)
  expect_in(c("#this is", "#a trailing comment"), out1)
})


test_that("autoimport errors", {
  test_autoimport(files="sample_error.R") %>%
    suppressMessages() %>%
    expect_warning(class="autoimport_duplicate_warn") %>%
    expect_error(class="autoimport_conflict_import_private_error")

  test_autoimport(files=test_path("source/sample_error.R"),
                  bad_ns=TRUE) %>%
    suppressMessages() %>%
    expect_error(class="autoimport_namespace_dup_error")
})
