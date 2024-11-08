
#' Automatically add `@importFrom` tags
#'
#' Automatically read all `R` files and add appropriate `@importFrom` tags in the roxygen headers.
#'
#' @param files files to read. Default to the `R/` folder.
#' @param pkg_name name of the package (character)
#' @param namespace_file path to the NAMESPACE file
#' @param description_file path to the DESCRIPTION file
#' @param use_cache use the cache system
#' @param ask whether to ask the user when multiple choices arise
#' @param ignore_package ignore files ending with `-package.R`
#' @param verbose the higher, the more output printed
#'
#' @return Nothing, used for side effects.
#' @export
#'
#' @importFrom cli cli_abort cli_h1 cli_inform
#' @importFrom purrr map walk
#' @importFrom rlang check_installed set_names
#' @importFrom stringr str_subset
autoimport = function(root=".",
                      files=get_R_dir(root),
                      pkg_name=get_package_name(root),
                      namespace_file=file.path(root, "NAMESPACE"),
                      description_file=file.path(root, "DESCRIPTION"),
                      use_cache=TRUE, ask=TRUE, ignore_package=TRUE,
                      verbose=2){
  target_dir = get_target_dir()
  cache_dir = get_target_dir("cache")
  ns = parse_namespace(namespace_file)
  importlist_path = file.path(root, "inst/IMPORTLIST")
  deps = desc::desc(file=description_file)$get_deps()

  cli_h1("Init")
  ns_loading = deps$package %>% setdiff("R")
  check_installed(ns_loading)
  walk(ns_loading, register_namespace)
  cli_inform(c(v="Registered namespaces of {length(ns_loading)} dependencies."))


  if(any(!file.exists(files))){
    cli_abort("Couldn't find file{?s} {.file {files[!file.exists(files)]}}")
  }

  files = set_names(files)
  lines_list = map(files, readr::read_lines)

  ai_read = autoimport_read(lines_list, verbose)

  ai_parse = autoimport_parse(ai_read$ref_list, cache_dir, use_cache, pkg_name,
                              ns, deps, ask, importlist_path, verbose)
  ai_write = autoimport_write(ai_parse$import_list, ai_read$ref_list, lines_list,
                              ai_parse$user_choice, ignore_package,
                              pkg_name, target_dir, verbose)

  cli_h1("Finished")

  data_files = review_files(dirname(files))
  if(!any(data_files$changed)){
    cli_inform(c(v="No changes to review."))
    rtn = FALSE
  } else {
    cli_inform(c(v="To view the diff and choose whether or not accepting the changes, run:",
                 i='{.run autoimport::import_review("{dirname(files)[1]}")}'))
    rtn = TRUE
  }

  invisible(rtn)
}


# Utils ---------------------------------------------------------------------------------------



#' @importFrom cli cli_h1 cli_inform
#' @importFrom purrr imap
#' @importFrom tibble lst
#' @importFrom utils getSrcref
autoimport_read = function(lines_list, verbose) {
  if(verbose>0) cli_h1("Reading")

  ref_list = lines_list %>%
    imap(function(lines, file){
      parsed = parse(text=lines, keep.source=TRUE)
      comments_refs = getSrcref(parsed) %>% comments() %>% set_names_ref()
      if(verbose>1) cli_inform(c(i="Found {length(comments_refs)} function{?s} in
                                 file {.file {file}} ({length(lines)} lines)"))
      comments_refs
    })
  tot_lines = sum(lengths(lines_list))
  tot_refs = sum(lengths(ref_list))
  if(verbose>0) cli_inform(c(v="Found a total of {tot_refs} internal functions
                             in {length(lines_list)} files ({tot_lines} lines)."))

  check_duplicated(ref_list, verbose)
  lst(ref_list)
}




#' @importFrom cli cli_h1 cli_inform
#' @importFrom digest digest
#' @importFrom purrr map map_dbl map_depth
#' @importFrom rlang set_names
#' @importFrom stringr str_replace
#' @importFrom tibble lst
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

  user_choice = get_user_choice(import_list, ask=ask, ns=ns, importlist_path=importlist_path)

  lst(import_list, user_choice)
}




#' @importFrom cli cli_h1 cli_inform
#' @importFrom glue glue
#' @importFrom purrr imap map pmap
#' @importFrom stringr str_ends
#' @importFrom tibble tibble
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

      inserts = imp_list %>% map(~get_inserts(.x, user_choice, exclude=c("base", pkg_name)))
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

      glue('{{.run diffviewer::visual_diff("{file}", "{out}")}}')
      tibble(file, out)
    }
  )

  diffs
}

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


#' @importFrom cli cli_h2 cli_warn
#' @importFrom dplyr arrange filter mutate rename
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom utils stack
#' @noRd
check_duplicated = function(ref_list, verbose) {
  dups = ref_list %>%
    map(names) %>% stack() %>%
    filter(values %in% values[duplicated(values)],
           !str_detect(values, "^unnamed_\\d+$")) %>%
    rename(fun=values, file=ind) %>%
    mutate(fun=paste0(fun, "()")) %>%
    arrange(fun)
  if(nrow(dups)>0){
    cli_h2("Warning - Duplicates")
    cli_warn(c("x"="There is several functions with the same name:"),
             class="autoimport_duplicate_warn")
    message(paste0(capture.output(dups), collapse = "\n"))
  }
  TRUE
}
