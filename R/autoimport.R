
#' Automatically compute `@importFrom` tags
#'
#' Automatically read all `R` files and compute appropriate `@importFrom` tags in the roxygen headers.
#' Choose which tags to add in the shiny app [import_review()] afterward.
#'
#' @param root Path to the root of the package.
#' @param files Files to read. Default to the `R/` folder.
#' @param namespace_file Path to the NAMESPACE file
#' @param description_file Path to the DESCRIPTION file
#' @param use_cache Whether to use the cache system. Can only be "read" or "write".
#' @param ignore_package Whether to ignore files ending with `-package.R`
#' @param verbose The higher, the more output printed. May slow the process a bit.
#'
#' @return Mostly used for side effects. Invisibly returns a dataframe summarizing the function imports, with input arguments as attributes.
#' @export
#'
#' @importFrom cli cli_abort cli_h1 cli_inform
#' @importFrom purrr map walk
#' @importFrom rlang check_installed current_env set_names
autoimport = function(root=".",
                      files=get_R_dir(root),
                      namespace_file="NAMESPACE",
                      description_file="DESCRIPTION",
                      use_cache=TRUE, ignore_package=TRUE,
                      verbose=2){
  target_dir = get_target_dir()
  ns = parse_namespace(namespace_file)
  importlist_path = getOption("autoimport_importlist", file.path(root, "inst/IMPORTLIST"))
  cache_path = get_cache_path(root)
  if(!file.exists(namespace_file)) namespace_file = file.path(root, namespace_file)
  if(!file.exists(description_file)) description_file = file.path(root, description_file)
  if(!all(file.exists(files))) files = file.path(root, "R", files)

  description = desc::desc(file=description_file)
  deps = description$get_deps()
  pkg_name = unname(description$get("Package"))

  main_caller$env = current_env()
  if(isTRUE(use_cache)) use_cache = c("read", "write")

  cli_h1("Init")
  ns_loading = deps$package %>% setdiff("R")
  check_installed(ns_loading)
  walk(ns_loading, register_namespace)
  cli_inform(c("Autoimporting for package {.pkg {pkg_name}} at {.path {root}}"))
  cli_inform(c(v="Registered namespaces of {length(ns_loading)} dependencies."))

  if(any(!file.exists(files))){
    cli_abort("Couldn't find file{?s} {.file {files[!file.exists(files)]}}")
  }

  files = set_names(files)
  lines_list = map(files, readr::read_lines)

  ref_list = autoimport_read(lines_list, verbose)

  data_imports = autoimport_parse(ref_list, cache_path, use_cache, pkg_name,
                                  ns, deps, verbose)

  data_imports = autoimport_ask(data_imports, ns, importlist_path)

  ai_write = autoimport_write(data_imports, ref_list, lines_list,
                              ignore_package, pkg_name, target_dir, verbose)

  cli_h1("Finished")

  data_files = review_files(dirname(files))
  review_dir = unique(dirname(files))[1]
  if(!any(data_files$changed)){
    cli_inform(c(v="No changes to review."))
  } else {
    cli_inform(c(v="To view the diff and choose whether or not accepting the changes, run:",
                 i='{.run autoimport::import_review("{review_dir}")}'))
  }

  data_imports = structure(
    data_imports,
    root=normalizePath(root),
    files=unname(files),
    namespace_file=namespace_file,
    description_file=description_file,
    use_cache=use_cache, ignore_package=ignore_package,
    verbose=verbose,

    target_dir=target_dir,
    review_dir=review_dir,
    cache_path=cache_path,
    session_info=sessionInfo()
  )

  invisible(data_imports)
}
