

#' @noRd
#' @keywords internal
#' @examples
#' assert(1+1==2)
#' assert(1+1==4)
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
#' assert_file_exists("R/assertions.R")
#' assert_file_exists("R/assertions.SAS")
assert_file_exists = function(x, msg=NULL){
  if(is.null(msg)){
    x_str = caller_arg(x)
    msg = glue("{x_str} doesn't exist.")
  }
  if(!file.exists(x)){
    cli_abort(msg)
  }
  invisible(TRUE)
}

