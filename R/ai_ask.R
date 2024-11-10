

#' Take a dataframe from `autoimport_parse()`, finds the functions for
#' which the source package is uncertain, and asks the user interactively
#' about them.
#' Returns the input dataframe, with column `pkg` being a single-value character
#'
#' @importFrom cli cli_h1 cli_inform
#' @importFrom dplyr distinct filter left_join mutate pull rowwise ungroup
#' @importFrom purrr map2_chr
#' @noRd
autoimport_ask = function(data_imports, ask, ns, importlist_path){
  pref_importlist = get_importlist(importlist_path)
  unsure_funs = data_imports %>%
    filter(action=="ask_user") %>%
    distinct(fun, pkg) %>%
    left_join(pref_importlist, by="fun")

  if(nrow(unsure_funs)==0) return(data_imports)

  cli_h1("Attributing")
  # rtn = list()
  defined_funs = unsure_funs %>%
    filter(!is.na(pref_pkg))
  undefined_funs = unsure_funs %>%
    filter(is.na(pref_pkg))

  if(nrow(defined_funs)>0){
    cli_inform(c(i="Automatically attributing {nrow(defined_funs)} function import{?s}
                    as predefined in {.file {importlist_path}}"))
  }

  if(nrow(undefined_funs)>0){
    if(ask){
      selected = user_input_pkg_choose(undefined_funs)
    } else {
      cli_inform(c(i="Automatically attributing {nrow(undefined_funs)} non-predefined
                      function import{?s}, as {.arg ask==FALSE}"))
      selected = 2
    }
    if(selected==0 || selected==3) stop("Abort mission")

    unsure_funs = unsure_funs %>%
      rowwise() %>%
      mutate(
        defined_in_importlist = !is.na(pref_pkg),
        pref_pkg = ifelse(defined_in_importlist, pref_pkg,
                          user_input_1package(fun, pkg, ns, select_first=selected==2))
      ) %>%
      ungroup()

    ask_update_importlist(unsure_funs, importlist_path)
  }

  fun_replace_list = unsure_funs %>% pull(pref_pkg, name=fun) %>% as.list()

  data_imports %>%
    mutate(
      pkg = map2_chr(pkg, fun, ~ifelse(length(.x)>1, fun_replace_list[[.y]], .x))
    )
}

#' @importFrom glue glue
#' @importFrom utils menu
#' @noRd
user_input_pkg_choose = function(unsure_funs){
  title = glue("\n\nThere are {nrow(unsure_funs)} functions that can be imported from several packages. What do you want to do?")
  choices = c("Choose the package for each", "Choose for me please", "Abort mission")
  menu(choices=choices, title=title)
}

#' @importFrom glue glue
#' @importFrom purrr map_int
#' @importFrom stringr str_pad
#' @importFrom utils menu
#' @noRd
user_input_1package = function(fun, pkg, ns, select_first){
  ni = map_int(pkg, ~sum(ns$importFrom$from==.x))
  pkg = pkg[order(ni, decreasing=TRUE)]
  ni = ni[order(ni, decreasing=TRUE)]
  if(select_first) return(pkg[1])
  label = glue(" ({n} function{s} imported)", n=str_pad(ni, max(nchar(ni))), s = ifelse(ni>1, "s", ""))
  label[pkg=="base"] = ""
  title = glue("`{fun}()` can be found in several packages.\n From which one do you want to import it:")
  choices = glue("{pkg}{label}")
  i = menu(choices=choices, title=title)
  if(i==0) return(NA)
  pkg[i]
}


#' @importFrom cli cli_inform
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom utils menu
ask_update_importlist = function(user_asked, path="inst/IMPORTLIST"){
  user_asked = user_asked %>% filter(!defined_in_importlist)
  resp = getOption("autoimport_testing_ask_save_importlist")
  if(!is.null(resp)){
    stopifnot(resp==1 || resp==2)
    x = if(resp==1) "" else "not "
    cli_inform(c(i="TESTING: {x}saving choices in {.file {path}}"))
  } else {
    s = if(nrow(user_asked)>1) "s" else ""
    title = glue("\n\nDo you want to save your choices about these {nrow(user_asked)} function{s} in `{path}`?")
    choices = c("Yes", "No")
    resp = menu(choices=choices, title=title)
  }

  if(resp==1){
    update_importlist(user_asked, path)
  }
  invisible(NULL)
}
