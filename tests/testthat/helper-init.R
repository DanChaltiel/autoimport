
# Options -------------------------------------------------------------------------------------

Sys.setenv(LANGUAGE = "en")
Sys.setenv(TZ='Europe/Paris')

options(
  encoding="UTF-8",
  warn=1, #0=stacks (default), 1=immediate=TRUE, 2 =error
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


# Directories ---------------------------------------------------------------------------------

test_path = function(path){
  if(!str_detect(getwd(), "testthat")){
    path = paste0("tests/testthat/", path)
  }
  path
}

options(
  autoimport_warnings_files_basename=TRUE,
  autoimport_testing_ask_save_importlist=NULL,
  autoimport_testing_dont_ask_select_first=NULL,
  autoimport_importlist=NULL,
  autoimport_target_dir=NULL
)


# Helpers -------------------------------------------------------------------------------------

#helper for snapshots
poor_diff = function(file){
  file_old = test_path("source", file)
  file_new = test_path("output", file)
  assert_file_exists(file_old)
  if(!file_exists(file_new)) return(NULL)

  a = readLines(file_old)
  b = readLines(file_new)
  common = intersect(a, b)
  adds = setdiff(b, a)
  removals = setdiff(a, b)

  lst(common, adds, removals)
}

expect_imported = function(output, pkg, fun){
  needle = glue("^#' ?@importFrom.*{pkg}.*{fun}")
  a = str_extract(output, glue("^#' ?@importFrom(.*){fun}"), group=1) %>%
    na.omit() %>% stringr::str_trim()
  b = if(length(a)>0) (", but from {{{a}}}.") else "."
  msg = cli::format_inline("Function {.fn {fun}} not imported from {{{pkg}}}", b)
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

test_autoimport = function(files, bad_ns=FALSE, use_cache=FALSE, root=NULL, verbose=2){
  #reset file paths
  if(is.null(root)){
    dir_source = test_path("source") %>% normalizePath()
    nm = paste0("autoimport_test_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    root = path(tempdir(), nm)
    unlink(root, recursive=TRUE)
    dir_create(root)
    file.copy(dir(dir_source, full.names=TRUE), to=root, recursive=TRUE)
    # dir(root, full.names=TRUE, recursive=TRUE)
  }
  wd = setwd(root)
  on.exit(setwd(wd))

  #load the whole test namespace
  pkgload::load_all(path=root, helpers=FALSE, quiet=TRUE)

  #set options
  rlang::local_options(
    rlang_backtrace_on_error = "full",
    autoimport_testing_dont_ask_select_first = TRUE,
    autoimport_testing_ask_save_importlist = 2 #2=No, 1=Yes
  )

  #run
  ns = if(bad_ns) "BAD_NAMESPACE" else "NAMESPACE"
  autoimport(
    root=root,
    files=files,
    ignore_package=TRUE,
    use_cache=use_cache,
    namespace_file=ns,
    verbose=verbose
  )

}

#diapo 3 donc en non-binding on est surpuissant ou c'est juste une paramétrisation ?



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
