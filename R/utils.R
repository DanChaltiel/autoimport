

ref_names = c("first_line", "first_byte", "last_line", "last_byte", "first_column",
              "last_column", "first_parsed", "last_parsed")

#TODO usethis:::read_utf8() ?

#' @source usethis:::write_utf8
#' @noRd
write_utf8 = function (path, lines, append=FALSE, line_ending="\n") {
  stopifnot(is.character(path))
  stopifnot(is.character(lines))
  file_mode = if (append) "ab" else "wb"
  con = file(path, open=file_mode, encoding="utf-8")
  on.exit(close(con))
  lines = gsub("\r?\n", line_ending, lines)
  writeLines(enc2utf8(lines), con, sep = line_ending,
             useBytes = TRUE)
  invisible(TRUE)
}


#' roxygen2:::comments
#' @noRd
#' @importFrom purrr map
comments = function (refs) {
  if(length(refs)==0) return(list())
  stopifnot(length(map(refs, ~attr(.x, "srcfile")) %>% unique())==1)
  srcfile = attr(refs[[1]], "srcfile")

  com = vector("list", length(refs))
  for (i in seq_along(refs)) {
    if (i == 1) {
      first_line = 1
    } else {
      first_line = refs[[i - 1]][3] + 1 #modif: +1
    }
    if (i == length(refs)){#add trailing lines
      last_line = length(srcfile$lines)
      last_byte = 1e8
    } else {
      last_line = refs[[i]][3]
      last_byte = refs[[i]][4]
    }
    lloc = c(first_line, first_byte=1, last_line, last_byte)
    com[[i]] = srcref(srcfile, lloc)
    attr(com[[i]], "lines") = c(first_line, last_line)
  }
  com
}


#' @importFrom purrr map map2
#' @importFrom utils getSrcref
#' @noRd
#' @examples
#' lines = read_lines(file)
#' parsed = parse(text=lines, keep.source=TRUE)
get_srcref_lines = function(parsed){
  refs = getSrcref(parsed) %>% set_names_ref()
  comments_refs = comments(refs) %>% set_names_ref()
  ref_names = c("first_line", "first_byte", "last_line", "last_byte", "first_column",
                "last_column", "first_parsed", "last_parsed")
  # lst(
  #   coms = comments_refs %>% map(~as.list(as.numeric(.x)) %>% set_names(ref_names)),
  #   funs = refs %>% map(~as.list(as.numeric(.x)) %>% set_names(ref_names)),
  # ) %>% transpose()

  comments_refs %>% map(~list(first_line_com=.x[1], last_line=.x[3]))
  refs %>% map(~list(first_line_fun=.x[1], last_line=.x[3]))

  rtn = map2(comments_refs, refs, ~{
    stopifnot(.x[3]==.y[3])
    list(first_line_com=.x[1], first_line_fun=.y[1], last_line=.x[3])
  })
  attr(rtn, "src") = comments_refs
  rtn
  # lst(
  #   coms = comments_refs %>% map(~list(first_line=.x[1], last_line=.x[3])),
  #   funs = refs %>% map(~list(first_line=.x[1], last_line=.x[3])),
  # ) %>% transpose()
}



#' @importFrom stringr str_starts
#' @noRd
is_com = function(x) str_starts(x, "#+'")

#' @importFrom purrr map_chr
#' @importFrom rlang set_names
#' @importFrom stringr regex str_extract str_starts
#' @noRd
set_names_ref = function(refs, warn_guess=FALSE){
  ref_names = refs %>%
    map_chr(~{
      src = as.character(.x, useSource=TRUE)
      src = src[!str_starts(src, "#")]
      src = src[nzchar(src)]
      # fun = paste(src, collapse="\n")
      # fun_name = str_extract(fun, regex("`?(.*?)`? *(?:=|<-) *function.*"), group=TRUE)
      fun_name = str_extract(src[1], regex("`?(.*?)`? *(?:=|<-) *function.*"), group=TRUE)
      # if(is.na(fun_name)){
      #   if(warn_guess) {
      #     cli_warn(c("Could not guess function name in code:", i="{.code {src}}"))
      #   }
      #   fun_name = "unknown"
      # }
      fun_name
    })
  ref_names[is.na(ref_names)] = paste0("unnamed_", seq_along(ref_names[is.na(ref_names)]))

  set_names(refs, ref_names)
}


#' A rewrite around [utils::getAnywhere()]
#'
#' Used in [parse_ref()], requires using `register_namespace()` beforehand.
#' Find all the packages that hold a function. `utils::getAnywhere()` annoyingly uses `find()` which yields false positives.
#'
#' @param fun a function name (character)
#' @param add_pkgs packages to look into, added to `loadedNamespaces()` (character)
#'
#' @return a character vector of package names
#' @importFrom purrr keep map_lgl
#' @importFrom rlang set_names
#' @noRd
get_anywhere = function(fun, add_pkgs=NULL){
  pkgs = c(loadedNamespaces(), add_pkgs) %>% unique() %>% set_names() %>%
    map_lgl(~is_exported(fun, pkg=.x)) %>% keep(isTRUE) %>% names() %>% sort()
  pkgs
}


#' @importFrom rlang ns_env
#' @noRd
register_namespace = function(name){
  suppressPackageStartupMessages(suppressWarnings(loadNamespace(name)))
  TRUE
}


#' is_exported("div", "htmltools")
#' is_exported("div", "shiny")
#' is_exported("dfsdsf", "shiny")
#' @importFrom cli cli_abort
#' @importFrom rlang is_installed
#' @noRd
is_exported = function(fun, pkg, type="::", fail=FALSE){
  if(!is_installed(pkg)){
    if(fail) cli_abort("{.pkg {pkg}} is not installed")
    return(FALSE)
  }
  text = paste0(pkg, type, fun)
  f = try(eval(parse(text=text)), silent=TRUE)
  is.function(f)
}


#' @noRd
#' @importFrom utils installed.packages
get_base_packages = function(){
  rownames(installed.packages(priority="base"))
}




# https://stackoverflow.com/a/31675695/3888000
#' @noRd
exists2 = function(x) {
  stopifnot(is.character(x) && length(x) == 1)

  split = strsplit(x, "::")[[1]]

  if (length(split) == 1) {
    exists(split[1])
  } else if (length(split) == 2) {
    exists(split[2], envir = asNamespace(split[1]))
  } else {
    stop(paste0("exists2 cannot handle ", x))
  }
}



#' @importFrom fs path_dir
#' @importFrom stringr regex str_remove
#' @noRd
get_new_file = function(file, path=path_dir(file), prefix="", suffix=""){
  f = str_remove(basename(file), regex("\\.[rR]"))
  rtn=paste0(path, "/", prefix, f, suffix, ".R")
  if(rtn==file){
    stop("overwriting?")
  }
  rtn
}


#' @noRd
#' @importFrom fs path
get_R_dir = function(root="."){
  path = path(root, "R")
  dir(path, pattern="\\.[Rr]$", full.names=TRUE)
}
#' @noRd
#' @importFrom fs dir_create path path_temp
get_target_dir = function(path=NULL){
  tmp = path_temp("autoimport_temp_target_dir")
  d = getOption("autoimport_target_dir", tmp)
  if(!is.null(path)) d = path(d, path)
  dir_create(d)
  d
}
#' @noRd
#' @importFrom fs path
get_cache_path = function(root="."){
  getOption("autoimport_cache_path", path(root, "inst/autoimport_cache.rds"))
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
clean_cache = function(root="."){
  cache_file = get_cache_path(root)
  rslt = unlink(cache_dir, recursive=TRUE)
  if(rslt==1){
    cli_abort("Could not remove {.file {cache_file}}.")
  }
  invisible(TRUE)
}


#' because base::parseNamespaceFile() is not very handy for my use.
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange filter mutate rename select
#' @importFrom purrr map map_chr
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @noRd
#' @keywords internal
parse_namespace = function(file){
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
              class="autoimport_namespace_dup_error",
              call=main_caller$env)
  }
  rtn
}


#' @source vctrs::`%0%`
#' @noRd
#' @keywords internal
`%0%` = function (x, y) {
  if(length(x)==0L) y else x
}
