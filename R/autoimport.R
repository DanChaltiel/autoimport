
#' Automatically add `@importFrom` tags
#'
#' Automatically read all `R` files and add appropriate `@importFrom` tags in the roxygen headers.
#'
#' @param files files to read. Default to the `R/` folder.
#' @param pkg_name name of the package (character)
#' @param namespace_file path to the NAMESPACE file
#' @param description_file path to the DESCRIPTION file
#' @param use_cache use the cache system
#' @param ask whether to ask the user when multiple choices arise
#' @param ignore_package ignore files ending with `-package.R`
#' @param verbose the higher, the more output printed
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
  cache_dir = get_target_dir("cache")
  ns = parse_namespace(namespace_file)
  importlist_path = file.path(root, "inst/IMPORTLIST")
  deps = desc::desc(file=description_file)$get_deps()
  main_caller$env = current_env()

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

  ai_read = autoimport_read(lines_list, verbose)

  ai_parse = autoimport_parse(ai_read$ref_list, cache_dir, use_cache, pkg_name,
                              ns, deps, ask, importlist_path, verbose)
  ai_write = autoimport_write(ai_parse$import_list, ai_read$ref_list, lines_list,
                              ai_parse$user_choice, ignore_package,
                              pkg_name, target_dir, verbose)

  cli_h1("Finished")

  data_files = review_files(dirname(files))
  if(!any(data_files$changed)){
    cli_inform(c(v="No changes to review."))
    rtn = FALSE
  } else {
    cli_inform(c(v="To view the diff and choose whether or not accepting the changes, run:",
                 i='{.run autoimport::import_review("{dirname(files)[1]}")}'))
    rtn = TRUE
  }

  invisible(rtn)
}
