

#' @noRd
#' @keywords internal
#' @examples
#' assert(1+1==2)
#' assert(1+1==4)
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom rlang caller_arg
assert = function(x, msg=NULL){
  if(is.null(msg)){
    x_str = caller_arg(x)
    msg = glue("`{x_str}` is FALSE")
  }
  if(!x){
    cli_abort(msg)
  }
  invisible(TRUE)
}


#' @noRd
#' @keywords internal
#' @examples
#' assert_file_exists(c("R/assertions.R", "R/autoimport.R"))
#' assert_file_exists(c("R/assertions.SAS", "R/autoimport.SAS", "R/autoimport.R"))
#' @importFrom cli cli_abort
#' @importFrom fs file_exists
assert_file_exists = function(x, msg=NULL){
  not_found = x[!file_exists(x)]
  if(length(not_found)>0){
    cli_abort("File{?s} do{?es/} not exist: {.file {not_found}}")
  }
  invisible(TRUE)
}
