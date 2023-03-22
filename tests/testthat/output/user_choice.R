
#' @importFrom cli cli_h1 cli_inform
#' @importFrom dplyr bind_rows distinct filter left_join mutate select
#' @importFrom purrr list_rbind map map2_chr
#' @importFrom tibble deframe
#' @noRd
get_user_choice = function(import_list, ask, ns){
  if(!is.data.frame(import_list[[1]])){
    import_list = import_list %>% map(list_rbind)
  }

  pref_importlist = get_importlist()
  unsure_funs = import_list %>%
    list_rbind(names_to="parent_fun") %>%
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
                    from {.file inst/IMPORTLIST}"))
    rtn = defined_funs %>% select(fun, package=pref_pkg) %>%
      deframe() %>%
      as.list()
  }

  undefined_funs = unsure_funs %>%
    filter(is.na(pref_pkg))
  if(nrow(undefined_funs)==0) return(rtn)

  if(ask){
    selected = user_input_packages(undefined_funs)
  } else {
    cli_inform(c(i="Automatically attributing {nrow(undefined_funs)} functions imports,
                    as {.arg ask==FALSE}"))
    selected = 2
  }
  if(selected==0 || selected==3) stop("abort mission")

  user_asked = undefined_funs %>%
    mutate(
      package = map2_chr(fun, pkg, ~{
        i = 1
        if(selected==1) i = user_input_1package(.x, .y, ns)
        if(i==0) return(NA)
        .y[i]
      })
    ) %>%
    select(fun, package)

  ask_update_importlist(user_asked)

  rtn = bind_rows(rtn, user_asked) %>%
    deframe() %>%
    as.list()
  rtn
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
user_input_1package = function(fun, pkg, ns, rtnVal=FALSE){
  ni = map_int(pkg, ~sum(ns$importFrom$from==.x))
  label = glue(" ({n} function{s} imported)", n=str_pad(ni, max(nchar(ni))), s = ifelse(ni>1, "s", ""))
  label[pkg=="base"] = ""
  title = glue("`{fun}()` can be found in several packages.\n From which one do you want to import it:")
  choices = glue("{pkg}{label}")
  i=menu(choices=choices, title=title)
  if(rtnVal) return(choices[i])
  i
}


#' @importFrom cli cli_inform
#' @importFrom glue glue
#' @importFrom utils menu
ask_update_importlist = function(user_asked){
  resp = getOption("autoimport_testing_ask_save_importlist")
  if(!is.null(resp)){
    stopifnot(resp==1 || resp==2)
    x = if(resp==1) "" else "not "
    cli_inform(c(i="TESTING: {x}saving choices in `inst/IMPORTLIST`"))
  } else {
    title = glue("\n\nDo you want to save your choices about these {nrow(user_asked)} functions in `inst/IMPORTLIST`?")
    choices = c("Yes", "No")
    resp = menu(choices=choices, title=title)
  }

  if(resp==1){
    update_importlist(user_asked)
  }
  invisible(NULL)
}
