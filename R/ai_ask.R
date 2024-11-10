

#' Take a dataframe from `autoimport_parse()`, finds the functions for
#' which the source package is uncertain, and asks the user interactively
#' about them.
#' Returns the input dataframe, with column `pkg` being a single-value character
#'
#' @importFrom cli cli_h1 cli_inform
#' @importFrom dplyr distinct filter left_join mutate select
#' @importFrom purrr list_rbind map map2_chr
#' @importFrom tibble deframe
#' @noRd
autoimport_ask = function(data_imports, ask, ns, importlist_path){
  pref_importlist = get_importlist(importlist_path)

  unsure_funs = data_imports %>%
    filter(action=="ask_user") %>%
    distinct(fun, pkg) %>%
    left_join(pref_importlist, by="fun")

  if(nrow(unsure_funs)==0) return(list())

  cli_h1("Attributing")
  rtn = list()
  defined_funs = unsure_funs %>%
    filter(!is.na(pref_pkg))
  if(nrow(defined_funs)>0){
    cli_inform(c(i="Automatically attributing {nrow(defined_funs)} functions imports
                    from {.file {importlist_path}}"))
    rtn = defined_funs %>% select(fun, package=pref_pkg) %>%
      deframe() %>%
      as.list()
  }

  undefined_funs = unsure_funs %>%
    filter(is.na(pref_pkg))
  if(nrow(undefined_funs)==0) return(rtn)

  if(ask){
    selected = user_input_pkg_choose(undefined_funs)
  } else {
    cli_inform(c(i="Automatically attributing {nrow(undefined_funs)} functions imports,
                    as {.arg ask==FALSE}"))
    selected = 2
  }
  if(selected==0 || selected==3) stop("abort mission")

  user_asked = undefined_funs %>%
    mutate(package = map2_chr(fun, pkg,
                              ~user_input_1package(.x, .y, ns, select_first=selected==2))) %>%
    select(fun, package)

  ask_update_importlist(user_asked, importlist_path)

  user_choice = c(rtn, deframe(user_asked))
  data_imports %>%
    mutate(
      pkg = map2_chr(pkg, fun, ~ifelse(length(.x)>1, user_choice[[.y]], .x))
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
#' @importFrom glue glue
#' @importFrom utils menu
ask_update_importlist = function(user_asked, path="inst/IMPORTLIST"){
  resp = getOption("autoimport_testing_ask_save_importlist")
  if(!is.null(resp)){
    stopifnot(resp==1 || resp==2)
    x = if(resp==1) "" else "not "
    cli_inform(c(i="TESTING: {x}saving choices in {.file {path}}"))
  } else {
    title = glue("\n\nDo you want to save your choices about these {nrow(user_asked)} functions in `{path}`?")
    choices = c("Yes", "No")
    resp = menu(choices=choices, title=title)
  }

  if(resp==1){
    update_importlist(user_asked, path)
  }
  invisible(NULL)
}
