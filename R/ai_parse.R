

#' @importFrom cli cli_h1 cli_inform
#' @importFrom digest digest
#' @importFrom purrr map map_dbl map_depth
#' @importFrom rlang set_names
#' @importFrom stringr str_replace
#' @importFrom tibble lst
#' @noRd
#' @keywords internal
autoimport_parse = function(ref_list, cache_dir, use_cache, pkg_name, ns,
                            deps, ask, importlist_path, verbose) {

  if(verbose>0) cli_h1("Parsing")

  files = names(ref_list) %>% set_names()
  cache_list = files %>% map(~{
    filename = basename(.x) %>% str_replace("\\.R|r$", ".rds")
    path = file.path(cache_dir, filename)
    if(!file.exists(path)) return(NULL)
    readRDS(path)
  })

  import_list = files %>%
    map(~{
      ref = ref_list[[.x]]
      cache = cache_list[[.x]]
      # dig = digest_list[[.x]]
      dig = digest(.x, file=TRUE)
      if(verbose>1) cli_inform(c(">"="File {.file {.x}}"))

      filename = basename(.x) %>% str_replace("\\.R|r$", ".rds")
      cache_path = file.path(cache_dir, filename)

      if(isTRUE(use_cache) && !is.null(cache) && dig==cache$dig){
        rtn = cache$cache
        verb = "Reading"
      } else {
        rtn = list_importFrom(ref, pkg_name=pkg_name, ns=ns, deps=deps, verbose=verbose>1) #long call
        verb = "Updating"
        saveRDS(list(dig=dig, cache=rtn), cache_path)
      }
      if(verbose>1){
        s = rtn %>% map_dbl(nrow) %>% sum()
        cli_inform(c("!"="{verb} cache",
                     "i"="Found {s} function{?s} to import in {length(rtn)} function{?s} or code chunk{?s}."))
      }

      rtn
    })

  n_imports = import_list %>% map_depth(2, nrow) %>% unlist() %>% sum()
  if(verbose>0) cli_inform(c(v="Found a total of {n_imports} potential function{?s} to import"))
  warn_not_found(import_list, verbose)

  user_choice = get_user_choice(import_list, ask=ask, ns=ns, importlist_path=importlist_path)

  lst(import_list, user_choice)
}


# Utils ---------------------------------------------------------------------------------------


#' used in [autoimport_parse()], calls [(parse_function)]
#' @importFrom cli cli_inform
#' @importFrom purrr imap
#' @importFrom stringr str_starts
#' @noRd
#' @keywords internal
list_importFrom = function(refs, pkg_name, ns, deps, verbose=FALSE){
  rslt = refs %>%
    imap(~{
      if(verbose){
        if(str_starts(.y, "unnamed_")){
          cli_inform(c(i="Parsing code block {.code {.y}}"))
        } else {
          cli_inform(c(i="Parsing function {.fun {.y}}"))
        }
      }
      parse_function(.x, pkg_name=pkg_name, ns=ns, deps=deps)
    })
  rslt
}


#' used in [list_importFrom()], calls [parse_ref()]
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange filter mutate pull
#' @importFrom glue glue
#' @importFrom purrr imap list_rbind map_chr
#' @importFrom tibble tibble
#' @noRd
#' @keywords internal
parse_function = function(ref, pkg_name, ns, deps){
  empty_ref = structure(list(fun = character(0), pkg = list(), pkg_str = character(0),
                             action = character(0), reason = character(0), pkgs = list()),
                        row.names = integer(0), class = "data.frame")
  loc = parse_ref(ref, pkg_name, ns, deps)
  if(is.null(loc)) return(empty_ref)
  if(nrow(loc)==0) return(loc)


  rslt = loc %>%
    split(.$fun) %>%
    imap(~{
      rtn = list(.x$pkg)
      action = "nothing"
      # if(.y=="mutate") browser()

      if(nrow(.x)==1) {
        if(isTRUE(.x$fun_is_inner)) {
          reason = glue("`{.y}()` is declared inside `fun()`.")
        } else if(is.na(.x$pkg)) {
          action = "warn"
          reason = glue("`{.y}()` not found in any loaded package.")
        } else if(.x$pkg==pkg_name) {
          reason = glue("`{.x$fun}()` is internal to {pkg_name}")
        } else if(.x$pkg=="base") {
          reason = glue("`{.x$fun}()` is base R")
        } else if(isTRUE(.x$fun_already_imported)) {
          reason = glue("`{.x$label}()` is unique and already imported.")
        } else {
          action = "add_pkg"
          reason = glue("`{.y}()` only found in package `{.x$pkg}`.")
        }

      } else {
        imported = .x %>% filter(fun_already_imported)
        base = .x %>% filter(pkg=="base")

        if(nrow(imported)>1) {
          dups = .x %>% filter(fun_already_imported) %>% pull(label)
          #should never happen, `autoimport_namespace_dup_error` first
          cli_abort(c("There are duplicates in NAMESPACE.", i="Functions: {.fun {dups}}"),
                    .internal=TRUE)
        } else if(nrow(imported)==1){
          rtn = list(imported$pkg)
          reason = glue("`{imported$label}()` already imported.")
        } else {
          action = "ask_user"
          reason = "Multiple choices"
        }
      }
      tibble(fun=.y, pkg=rtn, action=action, reason=reason, pkgs=list(.x))
    }) %>%
    list_rbind() %>%
    mutate(pkg_str = map_chr(pkg, paste, collapse="/"), .after=pkg) %>%
    arrange(action)

  rslt
}


#' Used in [parse_function()], calls [get_function_source]
#'
#' @param ref a ref
#' @param pkg_name package name (character)
#' @param ns result of `parse_namespace()`
#' @importFrom dplyr arrange bind_rows desc filter lag lead mutate pull select starts_with
#' @importFrom purrr map map_int
#' @importFrom rlang set_names
#' @importFrom stringr str_detect str_subset
#' @importFrom utils getParseData
#' @noRd
#' @keywords internal
parse_ref = function(ref, pkg_name, ns, deps){
  ignore = "#.*autoimport_ignore"
  ref_chr = as.character(ref, useSource=TRUE) %>%
    str_subset(ignore, negate=TRUE)

  .fun = paste(ref_chr, collapse="\n")

  pd = getParseData(parse(text=.fun, keep.source=TRUE))
  non_comment = pd %>% filter(token!="COMMENT") %>% pull(text) %>% paste(collapse="")
  nms = pd$text[pd$token == "SYMBOL_FUNCTION_CALL"] %>% unique()

  inner_vars = pd %>%
    filter(token!="expr") %>%
    filter(str_detect(lead(token), "ASSIGN") & token=="SYMBOL") %>%
    pull(text)

  if(getOption("ignore_prefixed", TRUE)){
    nms_prefixed = pd$token == "SYMBOL_FUNCTION_CALL" & lag(pd$token,n=2)=="SYMBOL_PACKAGE"
    nms_prefixed = pd$text[nms_prefixed]
    nms = setdiff(nms, nms_prefixed)
  }

  if(length(nms)==0) return(NULL)
  loc = nms %>%
    set_names() %>%
    map(~get_function_source(fun=.x, pkg=pkg, ns=ns, pkg_name=pkg_name)) %>%
    bind_rows() %>%
    arrange(fun) %>%
    mutate(
      fun_is_inner = fun %in% inner_vars,
      pkg = ifelse(fun_is_inner, "inner", pkg),
      label = ifelse(is.na(pkg), NA, paste(pkg, fun, sep="::")),
      pkg_in_desc = pkg %in% deps$package,
      pkg_n_imports = map_int(pkg, ~sum(ns$importFrom$from==.x)),
      fun_is_private = pkg==pkg_name,
      fun_is_base = pkg %in% get_base_packages()
    ) %>%
    select(fun, pkg, label, starts_with("pkg_"), starts_with("fun_")) %>%
    arrange(fun,
            desc(fun_is_inner),
            desc(fun_is_private),
            desc(fun_already_imported),
            desc(pkg_in_desc),
            desc(pkg_n_imports),
            fun_is_base) #base packages last
  loc
}


#' used in [parse_ref()]
#' get function source, with prioritizing known source if
#' function is already imported or if it is private to the
#' tested package
#' @importFrom cli cli_abort
#' @importFrom tibble tibble
#' @noRd
#' @keywords internal
get_function_source = function(fun, pkg, ns, pkg_name){
  # if(fun=="abort") browser()
  pkg = get_anywhere(fun, add_pkgs=unique(ns$importFrom$from))
  already_imported = ns$importFrom$what==fun
  is_private = is_exported(fun, pkg=pkg_name, type=":::")
  if(isTRUE(is_private)) {
    pkg = pkg_name
  }
  if(any(already_imported)) {
    pkg = ns$importFrom$from[already_imported]
  }
  if(length(pkg)==0) {
    pkg = NA
  }
  if(isTRUE(is_private) && any(already_imported)){
    cli_abort("Function {.fn {fun}} is both imported from {.pkg {pkg}} in
                NAMESPACE and declared as a private function in {.pkg {pkg_name}}.",
              class="autoimport_conflict_import_private_error",
              call=main_caller$env)
  }

  tibble(fun=fun, pkg=pkg, fun_is_private=is_private, fun_already_imported=FALSE)
}


#' used in [autoimport_parse()]
#' @importFrom cli cli_h2 cli_warn format_inline
#' @importFrom dplyr as_tibble bind_rows filter n_distinct pull select summarise
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom stringr str_remove
#' @noRd
#' @keywords internal
warn_not_found = function(import_list, verbose, remove_dir=FALSE){
  not_found = import_list %>%
    map(bind_rows, .id="file") %>%
    bind_rows(.id="source") %>%
    as_tibble() %>%
    #filter(map_lgl(pkg, ~any(is.na(.x))))
    filter(is.na(pkg)) %>%
    select(fun, source)

  folder = dirname(not_found$source)
  if(isFALSE(remove_dir) && n_distinct(folder)==1){
    not_found$source = str_remove(not_found$source, paste0(folder, "/?"))
  }

  if(nrow(not_found)>0){
    cli_h2("Warning - Not found")
    txt = "{qty(fun)}Function{?s} {.fn {fun}} (in {.file {unique(source)}})"
    i = not_found %>%
      summarise(label = format_inline(txt),
                .by=source) %>%
      pull(label) %>%
      set_names("i")
    cli_warn(c("Functions not found:", i),
             class="autoimport_fun_not_found_warn")
  }
  invisible(TRUE)
}
