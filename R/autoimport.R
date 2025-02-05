
#' Automatically compute `@importFrom` tags
#'
#' Automatically read all `R` files and compute appropriate `@importFrom` tags in the roxygen2 headers.
#' The tags can be added to the source files using the [import_review()] shiny app afterward.
#'
#' @param root Path to the root of the package.
#' @param location Whether to add `@importFrom` dispatched above each function, or centralized at the package level.
#' @param files Files to read. Default to the `R/` folder.
#' @param namespace_file Path to the NAMESPACE file
#' @param description_file Path to the DESCRIPTION file
#' @param use_cache Whether to use the cache system. Can only be "read" or "write".
#' @param ignore_package Whether to ignore files ending with `-package.R`
#' @param verbose The higher, the more output printed. May slow the process a bit.
#' @param ... unused
#'
#' @return Mostly used for side effects. Invisibly returns a dataframe summarizing the function imports, with input arguments as attributes.
#' @export
#'
#' @section Limitations:
#' Autoimport is based on [utils::getSrcref()] and share the same limits.
#' Therefore, some function syntaxes are not recognized and `autoimport` will try to remove their `@importFrom` from individual functions:
#'
#' - Operators (`@importFrom dplyr %>%`, `@importFrom rlang :=`, ...)
#' - Functions called by name (e.g. `sapply(x, my_fun))`
#' - Functions used inside strings (e.g. `glue("my_fun={my_fun(x)}")`)
#'
#' To keep them imported, you should either use a prefix (`pkg::my_fun`) or import them in your package-level documentation, as this file is ignored by default (with `ignore_package=TRUE`).
#'
#' @importFrom cli cli_abort cli_h1 cli_inform
#' @importFrom dplyr setdiff
#' @importFrom fs file_exists path path_dir
#' @importFrom purrr map walk
#' @importFrom rlang check_dots_empty check_installed current_env set_names
#' @importFrom utils sessionInfo
autoimport = function(root=".",
                      ...,
                      location=c("function", "package"),
                      files=get_R_dir(root),
                      namespace_file="NAMESPACE",
                      description_file="DESCRIPTION",
                      use_cache=TRUE, ignore_package=TRUE,
                      verbose=2){
  target_dir = get_target_dir()
  check_dots_empty()
  ns = parse_namespace(namespace_file)
  location = match.arg(location)
  importlist_path = getOption("autoimport_importlist", path(root, "inst/IMPORTLIST"))
  cache_path = get_cache_path(root)
  if(file_exists(path(root, namespace_file))) namespace_file = path(root, namespace_file)
  if(file_exists(path(root, description_file))) description_file = path(root, description_file)
  if(!all(file_exists(files))) files = path(root, "R", files)

  description = desc::desc(file=description_file)
  deps = description$get_deps()
  pkg_name = unname(description$get("Package"))

  main_caller$env = current_env()
  if(isTRUE(use_cache)) use_cache = c("read", "write")

  ns_loading = deps$package %>% setdiff("R")
  check_installed(ns_loading)
  walk(ns_loading, register_namespace)
  if(verbose>0){
    cli_h1("Init")
    cli_inform(c("Autoimporting for package {.pkg {pkg_name}} at {.path {root}}"))
    cli_inform(c(v="Registered namespaces of {length(ns_loading)} dependencies."))
  }
  if(any(!file_exists(files))){
    cli_abort("Couldn't find file{?s} {.file {files[!file_exists(files)]}}")
  }

  files = set_names(files)
  lines_list = map(files, readr::read_lines)

  ref_list = autoimport_read(lines_list, verbose)

  data_imports = autoimport_parse(ref_list, cache_path, use_cache, pkg_name,
                                  ns, deps, verbose)

  data_imports = autoimport_ask(data_imports, ns, importlist_path, verbose)

  ai_write = autoimport_write(data_imports, ref_list, lines_list, location,
                              ignore_package, pkg_name, target_dir, verbose)
  if(verbose>0) cli_h1("Finished")

  data_files = review_files(path_dir(files))
  review_dir = unique(path_dir(files))[1]
  if(verbose>0){
    if(!any(data_files$changed)){
      cli_inform(c(v="No changes to review."))
    } else {
      cli_inform(c(v="To view the diff and choose whether or not accepting the changes, run:",
                   i='{.run autoimport::import_review("{review_dir}")}'))
    }
  }

  data_imports = structure(
    data_imports,
    root=normalizePath(root),
    files=unname(files),
    namespace_file=namespace_file,
    description_file=description_file,
    pkg_name=pkg_name,
    use_cache=use_cache, ignore_package=ignore_package,
    verbose=verbose,

    target_dir=target_dir,
    review_dir=review_dir,
    cache_path=cache_path,
    session_info=sessionInfo()
  )

  invisible(data_imports)
}
