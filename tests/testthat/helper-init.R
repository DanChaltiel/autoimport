
# browseURL(".")

# Options -------------------------------------------------------------------------------------

Sys.setenv(LANGUAGE = "en")
Sys.setenv(TZ='Europe/Paris')

options(
  encoding="UTF-8",
  # warn=0, #default, stacks
  warn=1, #immediate.=TRUE
  # warn=2, #error
  rlang_backtrace_on_error = "full",
  stringsAsFactors=FALSE,
  dplyr.summarise.inform=FALSE,
  tidyverse.quiet=TRUE,
  tidyselect_verbosity ="verbose",#quiet or verbose
  lifecycle_verbosity="warning", #NULL, "quiet", "warning" or "error"
  testthat.progress.max_fails = 50,
  rlang_backtrace_on_error = "full"
)

snapshot_review_bg = function(...){
  brw = Sys.getenv("R_BROWSER")
  callr::r_bg(function() testthat::snapshot_review(...),
              package=TRUE,
              env = c(R_BROWSER = brw))
}

v=utils::View
#'@source https://stackoverflow.com/a/52066708/3888000
shhh = function(expr) suppressPackageStartupMessages(suppressWarnings(expr))
shhh(library(tidyverse))
shhh(library(rlang))


# Namespace loading ---------------------------------------------------------------------------

covr::azure
testthat::auto_test
knitr::all_labels
shiny::a
digest::sha1


# Directories ---------------------------------------------------------------------------------

dir_source_bak = test_path("source_bak")
dir_source = test_path("source")
dir_output = test_path("output")
namespace_file = test_path("inst/NAMESPACE")
bad_namespace_file = test_path("inst/BAD_NAMESPACE")
description_file = test_path("inst/DESCRIPTION")
importlist_file = test_path("inst/IMPORTLIST")

# if(!is_testing()){
#   dir_new=paste0("tests/testthat/", dir_new)
#   dir_old=paste0("tests/testthat/", dir_old)
#   dir_old_bak=paste0("tests/testthat/", dir_old_bak)
#   namespace_file=paste0("tests/testthat/", namespace_file)
#   description_file=paste0("tests/testthat/", description_file)
#   bad_namespace_file=paste0("tests/testthat/", bad_namespace_file)
#   options(autoimport_importlist="tests/testthat/inst/IMPORTLIST")
# } else {
#
# }
options(
  autoimport_importlist=NULL,
  autoimport_testing_ask_save_importlist=NULL,
  autoimport_target_dir=NULL
)

#restart folders (doesn't work :-( )
# unlink(glue("{dir_new}/*"), recursive=T, force=T)
# unlink(glue("{dir_old}/*"), recursive=T, force=T)
# file.copy(dir(dir_old_bak, full.names=TRUE), to=dir_old, overwrite=TRUE)


# Helpers -------------------------------------------------------------------------------------

poor_diff = function(file){
  file_old = test_path("source", file)
  file_new = test_path("output", file)
  assert_file_exists(file_old)
  if(!file.exists(file_new)) return(NULL)

  a = readLines(file_old)
  b = readLines(file_new)
  common = intersect(a, b)
  adds = setdiff(b, a)
  removals = setdiff(a, b)

  lst(common, adds, removals)
}

# All clear! ----------------------------------------------------------------------------------

cli::cli_inform(c(v="Initializer {.file tests/testthat/helper-init.R} loaded",
                  "is_testing={is_testing()}, is_parallel={is_parallel()}, interactive={interactive()}"))
