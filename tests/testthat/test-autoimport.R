
dir_source_save = test_path("source_save")
dir_source = test_path("source")
dir_output = test_path("output")
namespace_file = test_path("inst/NAMESPACE")
bad_namespace_file = test_path("inst/BAD_NAMESPACE")
description_file = test_path("inst/DESCRIPTION")
importlist_file = test_path("inst/IMPORTLIST")


test_that("autoimport", {
  #set options
  local_reproducible_output(width=125)
  withr::local_options(
    autoimport_target_dir = dir_output,
    autoimport_importlist = importlist_file,
    rlang_backtrace_on_error = "full",
    autoimport_testing_ask_save_importlist = 2 #2=No, 1=Yes
  )

  #load the test namespace
  pkgload::load_all(path=dir_source_save, helpers=FALSE, quiet=TRUE)

  #restart folders
  unlink(glue::glue("{dir_output}/*"), recursive=TRUE, force=TRUE)
  unlink(glue::glue("{dir_source}/*"), recursive=TRUE, force=TRUE)
  expect_length(dir(dir_source), 0)
  file.copy(dir(dir_source_save, full.names=TRUE, recursive=TRUE), to=dir_source, overwrite=TRUE)

  #snapshot
  expect_snapshot({
    autoimport(files=dir(dir_source, full.names=TRUE, pattern="\\.R$"),
               pkg_name="autoimport_test",
               ignore_package=TRUE,
               use_cache=FALSE,
               namespace_file=namespace_file,
               description_file=description_file,
               ask=FALSE, verbose=2)

    poor_diff("sample_code-package.R")
    poor_diff("sample_funs.R")
    poor_diff("sample_funs2.R")
  })

  # withr::deferred_clear()
})


test_that("import_review", {
  # TODO: test the shiny app
  expect_true(TRUE)
})


test_that("test iscom", {
  expect_false(is_com("xxx"))
  expect_true(is_com("#' xxx"))
  #TODO expect_true(is_com("  #' xxx"))
})


# test_that("parse_namespace", {
#   parse_namespace(bad_namespace_file) %>%
#     expect_error(class="autoimport_namespace_dup")
#
#   expect_snapshot({
#     ns = parse_namespace(namespace_file)
#     ns
#   })
# })

