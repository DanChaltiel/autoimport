

ref_names = c("first_line", "first_byte", "last_line", "last_byte", "first_column",
              "last_column", "first_parsed", "last_parsed")

#TODO usethis:::read_utf8() ?

#' usethis:::write_utf8
#' @importFrom withr defer
#' @noRd
write_utf8 = function (path, lines, append = FALSE, line_ending="\n") {
  stopifnot(is.character(path))
  stopifnot(is.character(lines))
  file_mode = if (append) "ab" else "wb"
  con = file(path, open=file_mode, encoding="utf-8")
  withr::defer(close(con))
  lines = gsub("\r?\n", line_ending, lines)
  writeLines(enc2utf8(lines), con, sep = line_ending,
             useBytes = TRUE)
  invisible(TRUE)
}


#' roxygen2:::comments
#' @noRd
comments = function (refs) {
  stopifnot(length(map(refs, ~attr(.x, "srcfile")) %>% unique())==1)
  srcfile = attr(refs[[1]], "srcfile")

  com = vector("list", length(refs))
  for (i in seq_along(refs)) {
    if (i == 1) {
      first_line = 1
    } else {
      first_line = refs[[i - 1]][3] + 1 #modif: +1
    }
    last_line = refs[[i]][3]
    last_byte = refs[[i]][4]
    lloc = c(first_line, first_byte=1, last_line, last_byte)
    com[[i]] = srcref(srcfile, lloc)
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


#' @param lines result of [read_lines()]
#' @param insert line to insert
#' @param pos insert before this position
#' @noRd
insert_line = function(lines, insert, pos){
  c(
    lines[seq(1, pos-1)],
    insert,
    lines[seq(pos, length(lines))]
  )
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
      # browser()
      src = as.character(.x, useSource=TRUE)
      src = src[!str_starts(src, "#")]
      src = src[nzchar(src)]
      fun = paste(src, collapse="\n")
      fun_name = str_extract(fun, regex("`?(.*?)`? *(?:=|=) *function.*"), group=TRUE)
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
#' Find all the packages that hold a function. `utils::getAnywhere()` annoyingly use `find()` which yield false positives.
#'
#' @param fun a function name (character)
#' @param prefer packages that should be prioritized. Usually the main package name and `.GlobalEnv`.
#'
#' @return a character vector of package names
#' @importFrom purrr map_lgl
#' @importFrom rlang set_names
#' @noRd
get_anywhere = function(fun, prefer){
  # pkgs = getAnywhere(fun)$where %>% str_remove("package:|namespace:") %>% unique()

  pkgs = loadedNamespaces() %>% set_names() %>% map_lgl(~{
    exists(fun, envir=asNamespace(.x), inherits=FALSE)
  })
  pkgs = sort(names(pkgs[pkgs]))

  pref = pkgs[pkgs %in% prefer]
  if(length(pref)>0) return(pref)

  exported = map_lgl(pkgs, ~is_exported(fun, .x))
  pkgs[exported]
}

#' @importFrom cli cli_abort
#' @importFrom rlang is_installed
#' @importFrom withr with_package
#' @noRd
is_exported = function(fun, pkg, fail=FALSE){
  if(!is_installed(pkg)){
    if(fail) cli_abort("{.pkg {pkg}} is not installed")
    return(FALSE)
  }
  l = withr::with_package(pkg, try(ls(paste0("package:",pkg)), silent=TRUE))
  if(inherits(l, "try-error")) return(FALSE)
  fun %in% l
}

#' @importFrom devtools as.package
#' @noRd
get_package_name = function(pkg=NULL){
  if(is.null(pkg)){
    default = devtools::as.package(".")$package
    pkg = getOption("autoimport_pkg", default)
  }
  pkg
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



#' @importFrom stringr regex str_remove
#' @noRd
get_new_file = function(file, path=dirname(file), prefix="", suffix=""){
  f = str_remove(basename(file), regex("\\.[rR]"))
  rtn=paste0(path, "/", prefix, f, suffix, ".R")
  if(rtn==file){
    stop("overwriting?")
  }
  rtn
}


#' @noRd
get_target_dir = function(path=NULL){
  tmp = file.path(tempdir(), "autoimport")
  d = getOption("autoimport_target_dir", tmp)
  if(!is.null(path)) d = file.path(d, path)
  dir.create(d, recursive=TRUE, showWarnings=FALSE)
  d
}
