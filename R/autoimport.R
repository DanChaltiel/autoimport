
#' Automatically add `@importFrom` tags
#'
#' Automatically read all `R` files and add appropriate `@importFrom` tags in the roxygen headers.
#'
#' @param files files to read. Default to the `R/` folder.
#' @param pkg_name name of the package (character)
#' @param namespace_file path to the NAMESPACE file
#' @param description_file path to the DESCRIPTION file
#' @param use_cache use the cache system. Can only be "read" or "write".
#' @param ask whether to ask the user when multiple choices arise
#' @param ignore_package ignore files ending with `-package.R`
#' @param verbose the higher, the more output printed. Slows the process a bit.
#'
#' @return Nothing, used for side effects.
#' @export
#'
#' @importFrom cli cli_abort cli_h1 cli_inform
#' @importFrom purrr map walk
#' @importFrom rlang check_installed current_env set_names
autoimport = function(root=".",
                      files=get_R_dir(root),
                      pkg_name=get_package_name(root),
                      namespace_file=file.path(root, "NAMESPACE"),
                      description_file=file.path(root, "DESCRIPTION"),
                      use_cache=TRUE, ask=TRUE, ignore_package=TRUE,
                      verbose=2){
  target_dir = get_target_dir()
  ns = parse_namespace(namespace_file)
  importlist_path = getOption("autoimport_importlist", file.path(root, "inst/IMPORTLIST"))
  cache_path = getOption("autoimport_cache_path", file.path(root, "inst/autoimport_cache.rds"))

  deps = desc::desc(file=description_file)$get_deps()
  main_caller$env = current_env()
  if(isTRUE(use_cache)) use_cache = c("read", "write")

  cli_h1("Init")
  ns_loading = deps$package %>% setdiff("R")
  check_installed(ns_loading)
  walk(ns_loading, register_namespace)
  cli_inform(c(v="Registered namespaces of {length(ns_loading)} dependencies."))


  if(any(!file.exists(files))){
    cli_abort("Couldn't find file{?s} {.file {files[!file.exists(files)]}}")
  }

  files = set_names(files)
  lines_list = map(files, readr::read_lines)

  ref_list = autoimport_read(lines_list, verbose)

  data_imports = autoimport_parse(ref_list, cache_path, use_cache, pkg_name,
                                  ns, deps, verbose)

  ai_ask = autoimport_ask(ai_parse, ask, ns, importlist_path)
  data_imports = autoimport_ask(data_imports, ask, ns, importlist_path)

  # browser()
  ai_write = autoimport_write(ai_parse, ref_list, lines_list,
                              ai_ask, ignore_package,
  ai_write = autoimport_write(data_imports, ref_list, lines_list,
                              ignore_package, pkg_name, target_dir, verbose)

  cli_h1("Finished")

  data_files = review_files(dirname(files))
  if(!any(data_files$changed)){
    cli_inform(c(v="No changes to review."))
  } else {
    cli_inform(c(v="To view the diff and choose whether or not accepting the changes, run:",
                 i='{.run autoimport::import_review("{dirname(files)[1]}")}'))
  }

  invisible(data_imports)
}
