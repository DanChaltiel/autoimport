
#' Update the IMPORTLIST file
#'
#' Update the IMPORTLIST file in order
#'
#' @param user_asked
#' @param target the path
#'
#' @return
#' @export
#'
#' @examples
update_importlist = function(user_asked, path=NULL){
  if(is.null(path)) path = getOption("autoimport_importlist", "inst/IMPORTLIST")
  # file = file.path(root, "inst/IMPORTLIST")
  # path = normalizePath(path, mustWork = FALSE)
  if(!file.exists(path)){
    dir.create(dirname(path))
    file.create(path)
  }
  old_imports = get_importlist(root) %>% deframe() %>% as.list()
  new_imports = user_asked %>% deframe() %>% as.list()
  if(length(new_imports)==0){
    cli::cli_inform(c(i="No change needed to {.file inst/IMPORTLIST}"))
    return(FALSE)
  }

  file_content = modifyList(old_imports, new_imports)
  file_content = file_content[order(names(file_content))]
  output = paste0(names(file_content), " = ", file_content)
  writeLines(output, path)
  cli::cli_inform(c(i="{length(new_imports)} line{?s} added to {.file inst/IMPORTLIST}"))
  TRUE
}


#' @rdname update_importlist
get_importlist = function(target=NULL){
  if(is.null(target)) target = getOption("autoimport_importlist", "inst/IMPORTLIST")
  if(!file.exists(target)) return(NULL)

  lines = readLines(target, warn=FALSE, encoding="UTF-8") %>%
    map(str_split_1, "=") %>%
    map(str_squish)
  checkmate::assert(all(lengths(lines)==2))

  #TODO check that file is correct and warn for xxx=unkwown_package
  tibble(fun=map_chr(lines, 1), pref_pkg=map_chr(lines, 2))
}
