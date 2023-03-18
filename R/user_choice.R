
#' @importFrom cli cli_inform
#' @importFrom dplyr distinct filter
#' @importFrom purrr list_rbind map map2
#' @importFrom rlang set_names
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

  defined_funs = unsure_funs %>%
    filter(!is.na(pref_pkg))
  undefined_funs = unsure_funs %>%
    filter(is.na(pref_pkg))

  if(nrow(defined_funs)>0){
    cli_inform(c(i="Automatically attributing {nrow(defined_funs)} functions imports
                    from {.file inst/IMPORTLIST}"))
    rtn = defined_funs %>% select(fun, package=pref_pkg)
  }


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


ask_update_importlist = function(user_asked){
  # if(is.data.frame(user_asked)) user_asked=deframe(user_asked) %>% as.list()
  # title = glue("\n\nDo you want to save your choices about these {length(user_asked)} functions in `inst/IMPORTLIST`?")
  title = glue("\n\nDo you want to save your choices about these {nrow(user_asked)} functions in `inst/IMPORTLIST`?")
  choices = c("Yes", "No")
  resp = menu(choices=choices, title=title)
  if(resp==1){
    update_importlist(user_asked)
  }
  invisible(NULL)
}


