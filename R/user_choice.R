
#' @importFrom cli cli_inform
#' @importFrom dplyr distinct filter
#' @importFrom purrr list_rbind map map2
#' @importFrom rlang set_names
#' @noRd
get_user_choice = function(import_list, ask, ns){
  if(!is.data.frame(import_list[[1]])){
    import_list = import_list %>% map(list_rbind)
  }
  # unsure_funs %>%
  #   select(fun, pkg) %>%
  #   deframe()
  #
  # user_choice = unsure_funs %>%
  #   filter(!fun %in% pref_importlist$fun) %>%
  #   get_user_choice(ask=ask, ns=ns)
  #

  unsure_funs = import_list %>%
    list_rbind(names_to="parent_fun") %>%
    filter(action=="ask_user") %>%
    distinct(fun, pkg)

  if(nrow(unsure_funs)==0) return(list())

  browser()

  if(ask){
    selected = user_input_packages(unsure_funs)
  } else {
    cli_inform(c(i="Automatically attributing {nrow(unsure_funs)} functions imports, as {.arg ask==FALSE}"), )
    selected = 2
  }
  if(selected==0 || selected==3){
    stop("abort mission")
  }

  user_asked = unsure_funs$fun %>%
    set_names() %>%
    map2(unsure_funs$pkg, ~{
      i = 1
      if(selected==1) i = user_input_1package(.x, .y, ns)
      if(i==0) return(NA)
      .y[i]
    })

  ask_update_importlist(user_asked)

  user_asked
}

#' @importFrom glue glue
#' @importFrom utils menu
#' @noRd
user_input_packages = function(unsure_funs){
  title = glue("\n\nThere are {nrow(unsure_funs)} functions that can be imported from several packages. What do you want to do?")
  choices = c("Choose the package for each", "Choose for me please", "Abort mission")
  menu(choices=choices, title=title)
}

#' @importFrom glue glue
#' @importFrom purrr map_int
#' @importFrom stringr str_pad
#' @importFrom utils menu
#' @noRd
user_input_1package = function(fun, pkg, ns){
  ni = map_int(pkg, ~sum(ns$importFrom$from==.x))
  label = glue(" ({n} function{s} imported)", n=str_pad(ni, max(nchar(ni))), s = ifelse(ni>1, "s", ""))
  label[pkg=="base"] = ""
  title = glue("`{fun}()` can be found in several packages.\n From which one do you want to import it:")
  choices = glue("{pkg}{label}")
  i=menu(choices=choices, title=title)
  # choices[i]
  i
}


ask_update_importlist = function(user_asked){
  title = glue("\n\nDo you want to save your choices about these {length(user_asked)} functions in `inst/IMPORTLIST`?")
  choices = c("Yes", "No")
  resp = menu(choices=choices, title=title)
  if(resp==1){
    update_importlist(user_asked)
  }
  invisible(NULL)
}


get_importlist = function(root=NULL){
  if(is.null(root)) root = getOption("autoimport_root", ".")
  file = file.path(root, "inst/IMPORTLIST")
  if(!file.exists(file)) return(NULL)

  lines = readLines(file, warn=FALSE, encoding="UTF-8") %>%
    map(stringr::str_split_1, "=") %>%
    map_depth(1, stringr::str_squish)
  checkmate::assert(all(lengths(lines)==2))

  #TODO check that file is correct and warn for xxx=unkwown_package
  tibble(fun=map_chr(lines, 1), pref_pkg=map_chr(lines, 2))
}

update_importlist = function(user_asked, root=NULL){
  if(is.null(root)) root = getOption("autoimport_root", ".")
  file = file.path(root, "inst/IMPORTLIST")
  file = normalizePath(file.path(root, "inst/IMPORTLIST"), mustWork = FALSE)
  if(!file.exists(file)){
    dir.create(dirname(file))
    file.create(file)
  }
  browser()
  old_imports = get_importlist(root)
  # osef = setdiff(names(old_imports), names(user_asked))
  new_imports = user_asked[setdiff(names(user_asked), names(old_imports))]
  if(length(new_imports)==0){
    cli::cli_inform(c(i="No change needed to {.file inst/IMPORTLIST}"))
    return(FALSE)
  }

  file_content = c(old_imports, new_imports)
  file_content = file_content[order(names(file_content))]
  output = paste0(names(file_content), " = ", file_content)
  writeLines(output, file)
  cli::cli_inform(c(i="{length(new_imports)} line{?s} added to {.file inst/IMPORTLIST}"))
  TRUE
}
