
test_that("autoimport warnings", {
  test_autoimport(files="sample_funs2.R") %>%
    suppressMessages() %>%
    expect_warning(class="autoimport_duplicate_warn") %>%
    expect_warning(class="autoimport_fun_not_found_warn")
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
