
#' because base::parseNamespaceFile() is not very handy for my use.
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange filter mutate rename select
#' @importFrom purrr map map_chr
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @noRd
parse_namespace = function(file){
  test_file = getOption("test_file")
  if(!is.null(test_file)) file = test_file

  directives = parse(file, keep.source = FALSE, srcfile = NULL) %>% as.list()
  rtn = tibble(operator = map_chr(directives, ~as.character(.x[1])),
               value = map_chr(directives, ~as.character(.x[2])),
               details = map_chr(directives, ~as.character(.x[3]))) %>%
    mutate(operator=factor(operator, levels=c("export", "import", "importFrom"))) %>%
    complete(operator) %>%
    split(.$operator) %>%
    map(~.x %>% filter(!is.na(value)))

  rtn$export = rtn$export %>% select(-details)
  rtn$import = rtn$import %>% rename(except=details)
  rtn$importFrom = rtn$importFrom %>% rename(from=value, what=details)

  if(anyDuplicated(rtn$importFrom$what)!=0){
    x = rtn$importFrom %>%
      filter(what %in% what[duplicated(what)]) %>%
      arrange(what, from)
    label = paste(x$from, x$what, sep="::")
    cli_abort(c("Duplicate `importFrom` mention in {.file {file}}",
                i="{.fun {label}}"),
              class="autoimport_namespace_dup_error")
  }
  rtn
}


#' Used in [parse_function()]
#'
#' @param ref a ref
#' @param pkg_name package name (character)
#' @param ns result of `parse_namespace()`
#' @importFrom dplyr arrange desc distinct filter lag mutate pull
#' @importFrom purrr map map_chr map_int map2_lgl
#' @importFrom stringr str_subset
#' @importFrom tibble as_tibble_col
#' @importFrom tidyr unchop
#' @importFrom utils getParseData
#' @noRd
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
      label = ifelse(is.na(pkg), NA, paste(pkg, fun, sep="::")),
      pkg_in_desc = pkg %in% deps$package,
      pkg_n_imports = map_int(pkg, ~sum(ns$importFrom$from==.x)),
      fun_is_inner = fun %in% inner_vars,
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


#' used in [list_importFrom()]
#' calls [parse_ref()]
#' @importFrom cli cli_warn
#' @importFrom dplyr arrange filter mutate pull
#' @importFrom glue glue
#' @importFrom purrr imap list_rbind map_chr
#' @importFrom tibble tibble
#' @noRd
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

#' used in [parse_ref()]
#' get function source, with prioritizing known source if
#' function is already imported or if it is private to the
#' tested package
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
              class="autoimport_conflict_import_private_error")
  }

  tibble(fun=fun, pkg=pkg, fun_is_private=is_private, fun_already_imported=FALSE)
}

#' used in [autoimport_parse()]
#' @importFrom cli cli_inform
#' @importFrom purrr imap
#' @importFrom stringr str_starts
#' @noRd
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


#' @importFrom dplyr cur_group filter group_by if_else mutate pull summarise
#' @importFrom purrr modify_if
#' @noRd
get_inserts = function(.x, user_choice, exclude){
  if(is.null(.x)) return(NULL)
  if(nrow(.x)==0) return(NULL)

  .x %>%
    mutate(
      tmp = user_choice[fun] %>% modify_if(is.null, ~"error"),
      pkg = if_else(lengths(pkg)>1, tmp, pkg) %>% unlist()) %>%
    group_by(pkg) %>%
    summarise(label = paste(cur_group(), paste(sort(fun), collapse=" "))) %>%
    filter(!is.na(pkg) & !pkg %in% exclude) %>%
    pull(label)
}

#' @importFrom glue glue
#' @importFrom stringr str_starts
#' @noRd
get_lines2 = function(src_ref, imports){
  fun_c = as.character(src_ref)
  if(length(imports)==0) return(fun_c)
  insert = glue("#' @importFrom {imports}")

  if(is_reexport(fun_c)){
    #TODO improve reexport management
    return(fun_c)
  }

  rmv = str_starts(fun_c, "#+' *@importFrom")
  if(any(rmv)){
    pos = min(which(rmv))
    fun_c = fun_c[!rmv]
  } else {
    x = parse(text=fun_c, keep.source=TRUE) %>% get_srcref_lines()
    stopifnot(length(x)==1)
    pos = x[[1]]$first_line_fun
  }
  insert_line(fun_c, insert, pos=pos)
}

#' @param lines result of [read_lines()]
#' @param insert line to insert
#' @param pos insert before this position
#' @noRd
insert_line = function(lines, insert, pos){
  if(length(lines)==1 || pos==1){
    return(c(insert, lines))
  }

  c(
    lines[seq(1, pos-1)],
    insert,
    lines[seq(pos, length(lines))]
  )
}

#' @importFrom dplyr last
#' @importFrom stringr str_detect
#' @noRd
is_reexport = function(fun_c){
  last_call = last(fun_c)
  str_detect(last_call, "(\\w+):{1,3}(?!:)(.+)") &&
    !str_detect(last_call, "(^|\\W)function\\(")
}
