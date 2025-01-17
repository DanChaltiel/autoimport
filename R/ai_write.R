

#' Take a dataframe from `autoimport_ask()`, a reflist from `autoimport_read()`, and
#' a list of lines from `readr::read_lines()`, and compute for each file what importFrom
#' lines should be removed or inserted.
#' Writes the correct lines in `target_dir` so they can be reviewed in `import_review()`.
#' Returns nothing of use.
#' @noRd
#' @keywords internal
#' @importFrom cli cli_h1 cli_inform
autoimport_write = function(data_imports, ref_list, lines_list, location,
                            ignore_package, pkg_name, target_dir, verbose){

  stopifnot(is.data.frame(data_imports))
  stopifnot(is.character(data_imports$pkg))
  stopifnot(names(ref_list)==names(lines_list))

  if(location=="function"){
    if(verbose>0) cli_h1("Writing at function level")
    if(verbose>1) cli_inform(c(">"="Temporarily writing to {.path {target_dir}}."))
    .autoimport_write_lvl_fn(data_imports, ref_list, lines_list,
                             ignore_package, pkg_name, target_dir, verbose)
  } else {
    if(verbose>0) cli_h1("Writing at package level")
    if(verbose>1) cli_inform(c(">"="Temporarily writing to {.path {target_dir}}."))
    .autoimport_write_lvl_pkg(data_imports, ref_list, lines_list,
                              ignore_package, pkg_name, target_dir, verbose)
  }
}

#' @noRd
#' @keywords internal
#' @importFrom dplyr filter mutate
#' @importFrom fs path
#' @importFrom glue glue
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_ends
.autoimport_write_lvl_pkg = function(data_imports, ref_list, lines_list,
                                     ignore_package, pkg_name, target_dir, verbose) {
  #merge all functions inserts into one (by setting source_fun)
  imports = data_imports |>
    filter(!(ignore_package & str_ends(file, "-package.[Rr]"))) |>
    mutate(source_fun="package_level") |>
    get_inserts(exclude=c("base", "inner", pkg_name)) |>
    unlist()
  inserts = glue("#' @importFrom {imports}")

  cur_package_doc = path("R", paste0(pkg_name, "-package"), ext="R")
  new_package_doc = path(target_dir, paste0(pkg_name, "-package"), ext="R")
  write_lines(read_lines(cur_package_doc), file=new_package_doc)

  .add_autoimport_package_doc(new_package_doc)
  .update_package_doc(new_package_doc, inserts)
  .remove_fun_lvl_imports(lines_list, target_dir, except=cur_package_doc)
  TRUE
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_inform
#' @importFrom dplyr setdiff
#' @importFrom fs path
#' @importFrom purrr imap map
#' @importFrom stringr str_ends
#' @importFrom tibble tibble
.autoimport_write_lvl_fn = function(data_imports, ref_list, lines_list,
                                    ignore_package, pkg_name, target_dir, verbose) {

  # data_imports %>% filter(fun=="writeLines")
  # .x %>% filter(fun=="writeLines")
  #list of paths input/output
  #not used, could be a walk()
  paths = data_imports %>%
    split(list(.$file)) %>%
    map(~{
      cur_file = unique(.x$file)
      target_file = path(target_dir, basename(cur_file))
      stopifnot(length(cur_file)==1)
      lines = lines_list[[cur_file]]
      comments_refs = ref_list[[cur_file]]

      if(str_ends(cur_file, "-package.[Rr]") && ignore_package){
        if(verbose>0) cli_inform(c(v="Ignoring {.file {cur_file}}.
                                      Use {.code ignore_package=FALSE} to override."))
        return(NULL)
      }
      if(length(lines)==0){
        if(verbose>0) cli_inform(c(">"="Nothing done in {.file {cur_file}} (file is empty)"))
        return(NULL)
      }

      inserts = get_inserts(.x, exclude=c("base", "inner", pkg_name))
      if(verbose>0) cli_inform(c(i="{length(unlist(inserts))} insert{?s} in
                                    {.file {basename(cur_file)}}"))

      lines2 = comments_refs %>%
        imap(~get_lines2(.x, inserts[[.y]])) %>%
        unname() %>% unlist() %>%
        add_trailing_comment_lines(lines)

      if(identical(lines, lines2)){
        if(verbose>0) cli_inform(c(">"="Nothing done in {.file {cur_file}} (all is already OK)"))
        unlink(target_file)
        return(NULL)
      }

      n_new = setdiff(lines2, lines) %>% length()
      n_old = setdiff(lines, lines2) %>% length()

      write_utf8(target_file, lines2)

      if(verbose>0) cli_inform(c(v="Added {n_new} and removed {n_old} line{?s}
                                    from {.file {cur_file}}."))
      tibble(file=cur_file, target_file)

    })

  paths
}


# Utils pkg-level -----------------------------------------------------------------------------



#' @noRd
#' @keywords internal
#' @importFrom cli cli_inform
#' @importFrom fs file_exists
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_detect
.add_autoimport_package_doc = function(package_doc){
  if(!file_exists(package_doc)){
    cli_inform("Adding package-level documentation {.path {package_doc}}.")
    content = ""
  } else {
    content = read_lines(package_doc)
  }
  if(any(str_detect(content, "autoimport namespace: start"))){
    return(TRUE)
  }

  content = c(content, "",
              "# The following block is used by autoimport.",
              "## autoimport namespace: start",
              "## autoimport namespace: end",
              "NULL")
  write_lines(content, package_doc)
}

#' @noRd
#' @keywords internal
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_detect
.update_package_doc = function(package_doc, inserts){
  content = read_lines(package_doc)
  start = str_detect(content, "autoimport namespace: start") |> which()
  stop  = str_detect(content, "autoimport namespace: end")  |> which()
  if(length(start)==0) start = length(content)
  if(length(stop)==0)  stop = length(content)

  new_content = c(content[1:start], inserts, content[stop:length(content)])
  write_lines(new_content, package_doc)
}

#' remove all `@importFrom` tags from source
#' @importFrom fs path path_abs
#' @importFrom purrr imap
#' @importFrom readr write_lines
#' @importFrom stringr str_starts
#' @noRd
#' @keywords internal
.remove_fun_lvl_imports = function(lines_list, target_dir, except){
  lines_list |>
    imap(function(lines, filename){
      if(path_abs(filename) %in% path_abs(except)) return(FALSE)
      target_file = path(target_dir, basename(filename))
      rmv = str_starts(lines, "#+' *@importFrom")
      new_lines = lines[!rmv]
      write_lines(new_lines, target_file)
      TRUE
    })
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


#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom purrr map
#' @noRd
#' @keywords internal
get_inserts = function(.x, exclude){
  .x %>%
    filter(!is.na(pkg) & !pkg %in% exclude) %>%
    mutate(label = paste(pkg, paste(sort(unique(fun)), collapse=" ")),
           .by=c(pkg, source_fun)) %>%
    distinct(source_fun, label) %>%
    arrange(source_fun, label) %>%
    split(.$source_fun) %>%
    map(~.x$label)
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
#' @param insert lines to insert
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
