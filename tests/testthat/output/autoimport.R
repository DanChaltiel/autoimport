
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
#' @importFrom dplyr desc
#' @importFrom purrr map
#' @importFrom rlang set_names
autoimport = function(files=dir("R/", pattern="\\.[Rr]$|", full.names=TRUE),
                      pkg_name=get_package_name(),
                      namespace_file="./NAMESPACE",
                      description_file="./DESCRIPTION",
                      use_cache=TRUE, ask=TRUE, ignore_package=TRUE,
                      verbose=2){
  target_dir = get_target_dir()
  cache_dir = get_target_dir("cache")
  ns = parse_namespace(namespace_file)
  deps = desc::desc(file=description_file)$get_deps()

  if(any(!file.exists(files))){
    cli_abort("Couldn't find file{?s} {.file {files[!file.exists(files)]}}")
  }

  files = set_names(files)
  lines_list = map(files, read_lines)

  ai_read = autoimport_read(lines_list, verbose)
  ai_parse = autoimport_parse(ai_read$ref_list, cache_dir, use_cache, pkg_name,
                              ns, deps, ask, verbose)
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
                 i="{.run autoimport::import_review()}"))
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
      if(verbose>1) cli_inform(c(i="Found {length(comments_refs)} function{?s} in file {.file {file}} ({length(lines)} lines)"))
      comments_refs
    })
  tot_lines = sum(lengths(lines_list))
  tot_refs = sum(lengths(ref_list))
  if(verbose>0) cli_inform(c(v="Found a total of {tot_refs} internal functions in {length(lines_list)} files ({tot_lines} lines)."))

  check_duplicated(ref_list, verbose)
  lst(ref_list)
}




#' @importFrom cli cli_h1 cli_inform
#' @importFrom purrr map map_dbl map_depth
#' @importFrom rlang set_names
#' @importFrom stringr str_replace
#' @importFrom tibble lst
autoimport_parse = function(ref_list, cache_dir, use_cache, pkg_name, ns, deps, ask, verbose) {

  if(verbose>0) cli_h1("Parsing")

  files = names(ref_list) %>% set_names()

  digest_list = map(files, digest, file=TRUE)

  cache_list = files %>% map(~{
    filename = basename(.x) %>% str_replace("\\.R|r$", ".rds")
    path = file.path(cache_dir, filename)
    if(!file.exists(path)) return(NULL)
    readRDS(path)
  })

  # cache_list %>%
  #   imap(~{
  #     dig = digest::digest(.y, file=TRUE)
  #     identical(dig, .x$dig)
  #   })
  #TODO on pourrait même faire un cache au niveau du digest de la ref elle-même!

  import_list = files %>%
    map(~{
      ref = ref_list[[.x]]
      cache = cache_list[[.x]]
      dig = digest_list[[.x]]
      if(verbose>1) cli_inform(c(">"="File {.file {.x}}"))

      filename = basename(.x) %>% str_replace("\\.R|r$", ".rds")
      cache_path = file.path(cache_dir, filename)

      if(isTRUE(use_cache) && !is.null(cache) && dig==cache$dig){
        rtn = cache$cache
        if(verbose>1){
          s = rtn %>% map_dbl(nrow) %>% sum()
          cli_inform(c("!"="Reading cache",
                       "i"="Found {s} function{?s} to import in {length(rtn)} function{?s} or code chunk{?s}."))
        }
      } else {
        rtn = list_importFrom(ref, pkg_name=pkg_name, ns=ns, deps=deps, verbose=verbose>1) #long call
        # if(verbose>1) cli_inform(c("!"="Updating cache"))
        if(verbose>1){
          s = rtn %>% map_dbl(nrow) %>% sum()
          cli_inform(c("!"="Updating cache",
                       "i"="Found {s} function{?s} to import in {length(rtn)} function{?s} or code chunk{?s}."))
        }
        saveRDS(list(dig=dig, cache=rtn), cache_path)
      }

      rtn
    })

  n_imports = import_list %>% map_depth(2, nrow) %>% unlist() %>% sum()

  if(verbose>0) cli_inform(c(v="Found a total of {n_imports} potential function{?s} to import"))

  user_choice = get_user_choice(import_list, ask=ask, ns=ns)

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


      lines2 = comments_refs %>% imap(~get_lines2(.x, inserts[[.y]])) %>% unname() %>% unlist()
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
    cli_warn(c("x"="There is several functions with the same name:"))
    print(dups)
  }
  TRUE
}
