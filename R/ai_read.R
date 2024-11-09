
#' Read a list of lines into a list of `srcref` (source references)
#' See [base::srcfile()] for all methods and functions
#'
#' @importFrom cli cli_h1 cli_inform
#' @importFrom purrr imap
#' @importFrom tibble lst
#' @importFrom utils getSrcref
#' @noRd
#' @keywords internal
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

  warn_duplicated(ref_list, verbose)
  ref_list
}


# Utils ---------------------------------------------------------------------------------------


#' @importFrom cli cli_h2 cli_warn
#' @importFrom dplyr arrange filter mutate rename
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom utils capture.output stack
#' @noRd
#' @keywords internal
warn_duplicated = function(ref_list, verbose) {
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
  invisible(TRUE)
}

