
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

test_path = function(path){
  if(!dir.exists(path) && !file.exists(path)) path = paste0("tests/testthat/", path)
  if(!dir.exists(path) && !file.exists(path)) stop(path)
  path
}

dir_source_save = test_path("source_save")
dir_source = test_path("source")
dir_output = test_path("output")
namespace_file = test_path("inst/NAMESPACE")
bad_namespace_file = test_path("inst/BAD_NAMESPACE")
description_file = test_path("inst/DESCRIPTION")
importlist_file = test_path("inst/IMPORTLIST")


options(
  autoimport_importlist=NULL,
  autoimport_testing_ask_save_importlist=NULL,
  autoimport_target_dir=NULL
)

if(!is_testing()){
  options(autoimport_importlist=importlist_file)
}


#restart folders (doesn't work :-( )
# unlink(glue("{dir_new}/*"), recursive=T, force=T)
# unlink(glue("{dir_old}/*"), recursive=T, force=T)
# file.copy(dir(dir_old_bak, full.names=TRUE), to=dir_old, overwrite=TRUE)


# Helpers -------------------------------------------------------------------------------------

#helper for snapshots
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


expect_imported = function(output, pkg, fun){
  needle = glue("^#' ?@importFrom.*{pkg}.*{fun}")
  msg = cli::format_inline("Function `{fun}` not imported from `{pkg}`.")
  expect(any(str_detect(output, needle)),
         failure_message=msg)
  invisible(output)
}
expect_not_imported = function(output, pkg, fun){
  needle = glue("^#' ?@importFrom.*{pkg}.*{fun}")
  x = str_detect(output, needle)
  faulty = line = NULL
  if(any(x)){
    line = min(which(str_detect(output, needle)))
    faulty = output[line]
  }
  msg = cli::format_inline("Function `{fun}` imported from `{pkg}` on line {line}: {.val {faulty}}.")
  expect(!any(x), failure_message=msg)

  invisible(faulty)
}

test_autoimport = function(files, bad_ns=FALSE){
  #reset file paths
  dir_source_save = test_path("source_save")
  dir_source = test_path("source")
  dir_output = test_path("output")
  namespace_file = test_path("inst/NAMESPACE")
  bad_namespace_file = test_path("inst/BAD_NAMESPACE")
  description_file = test_path("inst/DESCRIPTION")
  importlist_file = test_path("inst/IMPORTLIST")
  ns = if(bad_ns) bad_namespace_file else namespace_file

  #set options
  withr::local_options(
    autoimport_target_dir = dir_output,
    autoimport_importlist = importlist_file,
    rlang_backtrace_on_error = "full",
    autoimport_testing_ask_save_importlist = 2 #2=No, 1=Yes
  )

  #load the whole test namespace
  pkgload::load_all(path=dir_source_save, helpers=FALSE, quiet=TRUE)

  #restart folders
  unlink(glue::glue("{dir_output}/*"), recursive=TRUE, force=TRUE)
  unlink(glue::glue("{dir_source}/*"), recursive=TRUE, force=TRUE)
  stopifnot(length(dir(dir_source)) == 0)
  file.copy(dir(dir_source_save, full.names=TRUE, recursive=TRUE), to=dir_source, overwrite=TRUE)

  #run
  autoimport(files=files,
             pkg_name="autoimport_test",
             ignore_package=TRUE,
             use_cache=FALSE,
             namespace_file=ns,
             description_file=description_file,
             ask=FALSE, verbose=2)
}

condition_overview = function(expr){
  tryCatch2(expr) %>% attr("overview")
}
tryCatch2 = function(expr){
  errors = list()
  warnings = list()
  messages = list()
  rtn = withCallingHandlers(tryCatch(expr, error = function(e) {
    errors <<- c(errors, list(e))
    return("error")
  }), warning = function(w) {
    warnings <<- c(warnings, list(w))
    invokeRestart("muffleWarning")
  }, message = function(m) {
    messages <<- c(messages, list(m))
    invokeRestart("muffleMessage")
  })
  attr(rtn, "errors") = unique(map_chr(errors, conditionMessage))
  attr(rtn, "warnings") = unique(map_chr(warnings, conditionMessage))
  attr(rtn, "messages") = unique(map_chr(messages, conditionMessage))
  x = c(errors, warnings, messages) %>% unique()
  attr(rtn, "overview") = tibble(type = map_chr(x, ~ifelse(inherits(.x,
                                                                    "error"), "Error", ifelse(inherits(.x, "warning"), "Warning",
                                                                                              "Message"))), class = map_chr(x, ~class(.x) %>% glue::glue_collapse("/")),
                                 message = map_chr(x, ~conditionMessage(.x)))
  rtn
}


# All clear! ----------------------------------------------------------------------------------

cli::cli_inform(c(v="Initializer {.file tests/testthat/helper-init.R} loaded",
                  "is_testing={is_testing()}, is_parallel={is_parallel()}, interactive={interactive()}"))
