
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

  directives = parse(file, keep.source = FALSE, srcfile = NULL)

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
              class="autoimport_namespace_dup")
  }
  rtn
}


#' Used in [parse_function()]
#'
#' @param ref a ref
#' @param pkg_name package name (character)
#' @param ns result of `parse_namespace()`
#' @importFrom dplyr arrange desc distinct filter mutate pull
#' @importFrom purrr map map_chr map_int map2_lgl
#' @importFrom tibble as_tibble_col
#' @importFrom tidyr unchop
#' @importFrom utils getParseData
#' @noRd
parse_ref = function(ref, pkg_name, ns, deps){
  ref_chr = as.character(ref, useSource=TRUE) %>%
    str_subset("#.*autoimport_ignore", negate=TRUE)

  .fun = paste(ref_chr, collapse="\n")
  pd = getParseData(parse(text=.fun))
  # pd = getParseData(parse(text=str_replace_all(.fun, "~", "")))
  non_comment = pd %>% filter(token!="COMMENT") %>% pull(text) %>% paste(collapse="")
  nms = pd$text[pd$token == "SYMBOL_FUNCTION_CALL"] %>% unique()

  if(getOption("ignore_prefixed", TRUE)){
    nms_prefixed = pd$token == "SYMBOL_FUNCTION_CALL" & lag(pd$token,n=2)=="SYMBOL_PACKAGE"
    nms_prefixed = pd$text[nms_prefixed]
    nms = setdiff(nms, nms_prefixed)
  }

  if(length(nms)==0) return(NULL)

  imported_from = function(fun, ns){
    x=ns$importFrom$from[ns$importFrom$what==fun]
    if(length(x)==0) x=NA
    x
  }

  loc = nms %>%
    as_tibble_col(column_name="fun") %>%
    mutate(pkg_bak = map(fun, ~get_anywhere(.x, prefer=c(pkg_name, ".GlobalEnv")))) %>%
    unchop(pkg_bak, keep_empty=TRUE) %>%
    mutate(
      pkg = map_chr(fun, ~imported_from(.x, ns)),
      pkg = ifelse(is.na(pkg), pkg_bak, pkg),
      label = ifelse(is.na(pkg), NA, paste(pkg, fun, sep="::")),
      pkg_in_desc = pkg %in% deps$package,
      pkg_n_imports = map_int(pkg, ~sum(ns$importFrom$from==.x)),
      fun_imported = map2_lgl(pkg, fun, ~{any(ns$importFrom$from==.x & ns$importFrom$what==.y)}),
    ) %>%
    # distinct(label, .keep_all=TRUE) %>% print(n=50) %>%
    distinct(pkg, fun, .keep_all=TRUE) %>%
    arrange(fun, desc(fun_imported), desc(pkg_n_imports), pkg_in_desc)

  # browser()
  loc
}

empty_ref = structure(list(fun = character(0), pkg = list(), pkg_str = character(0),
                           action = character(0), reason = character(0), pkgs = list()),
                      row.names = integer(0), class = "data.frame")

#' @importFrom cli cli_warn
#' @importFrom dplyr arrange filter mutate pull
#' @importFrom glue glue
#' @importFrom purrr imap list_rbind map_chr
#' @importFrom tibble tibble
#' @noRd
parse_function = function(ref, pkg_name, ns, deps){
  loc = parse_ref(ref, pkg_name, ns, deps)
  if(is.null(loc)) return(empty_ref)
  if(nrow(loc)==0) return(loc)

  rslt = loc %>%
    split(.$fun) %>%
    imap(~{
      rtn = list(.x$pkg)
      action = "nothing"

      ###-- TESTING --###
      # .y is the name of the function
      # if(.y=="get_importlist") browser()

      if(nrow(.x)==1) {
        if(is.na(.x$pkg)) {
          action = "warn"
          reason = glue("`{.y}()` not found in any loaded package.")
        } else if(.x$pkg==pkg_name) {
          reason = glue("`{.x$fun}()` is internal to {pkg_name}")
        } else if(.x$pkg=="base") {
          reason = glue("`{.x$fun}()` is base R")
        } else if(isTRUE(.x$fun_imported)) {
          reason = glue("`{.x$label}()` is unique and already imported.")
        # } else if(isFALSE(.x$pkg_in_desc)) {
        #TODO warning si pas dans description
        #   action = "add_description"
        #   reason = glue("`{.y}()` only found in package `{.x$pkg}`,
        #                     but your should add it to DESCRIPTION first.")
        } else {
          action = "add_pkg"
          reason = glue("`{.y}()` only found in package `{.x$pkg}`.")
        }

      } else {
        imported = .x %>% filter(fun_imported)
        base = .x %>% filter(pkg=="base")

        if(nrow(imported)>1) {
          dups = .x %>% filter(fun_imported) %>% pull(label)
          cli_warn(c("There are duplicates in NAMESPACE??", i="Functions: {.fun {dups}}"))
        } else if(nrow(imported)==1){
          rtn = list(imported$pkg)
          reason = glue("`{imported$label}()` already imported.")
        # } else if(nrow(base)>0){
        #   stopifnot(nrow(base)==1)
        #   rtn = list(base$pkg)
        #   reason = glue("`base::{.y}()` preferred")
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
