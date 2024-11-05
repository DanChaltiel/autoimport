
dir_source_bak=test_path("source_bak")
dir_source=test_path("source")
dir_output=test_path("output")
namespace_file=test_path("inst/NAMESPACE")
bad_namespace_file=test_path("inst/BAD_NAMESPACE")
description_file=test_path("inst/DESCRIPTION")
importlist_file=test_path("inst/IMPORTLIST")


test_that("autoimport", {

  withr::local_options(autoimport_target_dir = dir_output)
  withr::local_options(autoimport_importlist=importlist_file)
  withr::local_options(rlang_backtrace_on_error="full")
  withr::local_options(autoimport_testing_ask_save_importlist=1) #Yes
  # withr::local_options(autoimport_testing_ask_save_importlist=2) #No

  #restart folders
  unlink(glue::glue("{dir_output}/*"), recursive=T, force=T)
  unlink(glue::glue("{dir_source}/*"), recursive=T, force=T)
  expect_length(dir(dir_source), 0)
  file.copy(dir(dir_source_bak, full.names=TRUE), to=dir_source, overwrite=TRUE)

  # browser()
  expect_snapshot({
    autoimport(files=dir(dir_source, full.names=TRUE),
               pkg_name="autoimport",
               ignore_package=TRUE,
               use_cache=FALSE,
               namespace_file=namespace_file,
               description_file=description_file,
               ask=FALSE, verbose=2)
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

