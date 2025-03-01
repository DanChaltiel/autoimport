
#' Read a list of lines from `readr::read_lines()` (one per file)
#' Returns a list of source references (`srcref`, one per function)
#' See [base::srcfile()] for all methods and functions
#'
#' @importFrom cli cli_h1 cli_inform
#' @importFrom purrr imap
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
#' @importFrom glue glue_data
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom utils capture.output stack
#' @noRd
#' @keywords internal
warn_duplicated = function(ref_list, verbose) {
  ref_list %>% map(~map(.x, ~attr(.x, "lines")))
  if(length(ref_list)==0) return(FALSE)
  dups = ref_list %>%
    map(~{
      lines = map(.x, ~attr(.x, "lines"))
      tibble(fun=names(.x), first_line=map_dbl(lines, 1), last_line=map_dbl(lines, 2))
    }) %>%
    list_rbind(names_to="file") %>%
    filter(fun %in% fun[duplicated(fun)],
           !str_detect(fun, "^unnamed_\\d+$")) %>%
    mutate(fun=paste0(fun, "()"), file=basename(as.character(file))) %>%
    arrange(fun)

  if(nrow(dups)>0){
    dup_list = dups %>%
      glue_data("{fun} in {file} (lines {first_line}-{last_line})") %>%
      set_names("*")
    if(verbose>0) cli_h2("Warning - Duplicates")
    cli_warn(c("x"="There is several functions with the same name."),
             class="autoimport_duplicate_warn")
    if(verbose>0) cli_inform(dup_list)
  }
  invisible(TRUE)
}
