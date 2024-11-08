

#' @importFrom cli cli_h1 cli_inform
#' @importFrom purrr imap map pmap
#' @importFrom stringr str_ends
#' @importFrom tibble tibble
#' @noRd
#' @keywords internal
autoimport_write = function(import_list, ref_list, lines_list, user_choice, ignore_package,
                            pkg_name, target_dir, verbose) {

  if(verbose>0) cli_h1("Writing")

  stopifnot(names(import_list)==names(lines_list))
  stopifnot(names(ref_list)==names(lines_list))

  diffs = pmap(
    list(lines_list, ref_list, import_list, names(import_list)),
    function(lines, comments_refs, imp_list, file){
      if(str_ends(file, "-package.[Rr]") && ignore_package){
        if(verbose>0) cli_inform(c(v="Ignoring {.file {file}}. Use {.code ignore_package=FALSE} to override."))
        return(NULL)
      }
      if(length(lines)==0){
        if(verbose>0) cli_inform(c(">"="Nothing done in {.file {file}} (file is empty)"))
        return(NULL)
      }

      inserts = imp_list %>% map(~get_inserts(.x, user_choice, exclude=c("base", "inner", pkg_name)))
      cli_inform(c(i="{length(unlist(inserts))} inserts in {.file {file}}"))

      lines2 = comments_refs %>%
        imap(~get_lines2(.x, inserts[[.y]])) %>%
        unname() %>% unlist() %>%
        add_trailing_comment_lines(lines)

      if(identical(lines, lines2)){
        if(verbose>0) cli_inform(c(">"="Nothing done in {.file {file}} (all is already OK)"))
        return(NULL)
      }

      n_new = setdiff(lines2, lines) %>% length()
      n_old = setdiff(lines, lines2) %>% length()
      out = file.path(target_dir, basename(file))

      write_utf8(out, lines2)

      if(verbose>0) cli_inform(c(v="Added {n_new} and removed {n_old} line{?s} from {.file {file}}."))
      tibble(file, out)
    }
  )

  diffs
}


# Utils ---------------------------------------------------------------------------------------


#' If the file ends with comments, these would be ignored by the parser
#' This functions adds them manually
#' @importFrom stringr str_detect
#' @noRd
#' @keywords internal
add_trailing_comment_lines = function(lines2, lines){
  if(identical(lines, lines2)) return(lines2)

  m = max(which(lines %in% lines2))
  if(m==length(lines)) return(lines2)

  trailing_lines = lines[m:length(lines)]
  are_comments_or_spaces = str_detect(trailing_lines, "^\\s*$|^\\s*#.*$")
  if(all(are_comments_or_spaces)){
    lines2 = c(lines2, trailing_lines)
  }

  lines2
}


#' @importFrom dplyr cur_group filter group_by if_else mutate pull summarise
#' @importFrom purrr modify_if
#' @noRd
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
is_reexport = function(fun_c){
  last_call = last(fun_c)
  str_detect(last_call, "(\\w+):{1,3}(?!:)(.+)") &&
    !str_detect(last_call, "(^|\\W)function\\(")
}
