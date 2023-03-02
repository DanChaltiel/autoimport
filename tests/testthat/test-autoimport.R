

test_that("autoimport", {
  withr::local_options(autoimport_target_dir = dir_new)

  #restart folders
  unlink(glue("{dir_new}/*"), recursive=T, force=T)
  unlink(glue("{dir_old}/*"), recursive=T, force=T)
  file.copy(dir(dir_old_bak, full.names=TRUE), to=dir_old, overwrite=TRUE)

  # expect_snapshot({
    autoimport(files=dir(dir_old, full.names=TRUE),
               pkg_name="autoimport",
               namespace_file=namespace_file,
               description_file=description_file,
               ask=FALSE, verbose=2)
  # })


  import_review(review_files(dir_old))

})



