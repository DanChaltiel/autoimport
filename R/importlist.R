
#' Update the `IMPORTLIST` file
#'
#' Update the `IMPORTLIST` file, which forces the import of some packages without asking.
#'
#' @param imports a list of imports with `key=function` and `value=package`
#' @param path path to the `IMPORTLIST` file
#'
#' @return nothing
#' @export
#'
#' @importFrom cli cli_inform
#' @importFrom tibble deframe
#' @importFrom utils modifyList
update_importlist = function(imports, path=NULL){
  if(is.null(path)) path = getOption("autoimport_importlist", "inst/IMPORTLIST")
  # path = normalizePath(path, mustWork = FALSE)
  if(!file.exists(path)){
    dir.create(dirname(path), showWarnings=FALSE)
    file.create(path)
  }
  old_imports = get_importlist(path) %>% deframe() %>% as.list()
  new_imports = imports %>% deframe() %>% as.list()
  if(length(new_imports)==0){
    cli::cli_inform(c(i="No change needed to {.file {path}}"))
    return(FALSE)
  }

  file_content = modifyList(old_imports, new_imports)
  file_content = file_content[order(names(file_content))]
  output = paste0(names(file_content), " = ", file_content)
  writeLines(output, path)
  cli::cli_inform(c(i="{length(new_imports)} line{?s} added to {.file {path}}"))
  TRUE
}


#' @rdname update_importlist
#' @importFrom checkmate assert
#' @importFrom purrr map map_chr
#' @importFrom stringr str_split_1 str_squish
#' @importFrom tibble tibble
get_importlist = function(path=NULL){
  if(is.null(path)) path = getOption("autoimport_importlist", "inst/IMPORTLIST")
  if(!file.exists(path)) return(tibble(fun=NA, pref_pkg=NA))

  lines = readLines(path, warn=FALSE, encoding="UTF-8") %>%
    map(~str_split_1(.x, "=")) %>%
    map(~str_squish(.x))
  checkmate::assert(all(lengths(lines)==2))

  #TODO check that file is correct and warn for xxx=unkwown_package
  tibble(fun=map_chr(lines, 1), pref_pkg=map_chr(lines, 2))
}
