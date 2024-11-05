1



#' Title f1
#'
#' a description
#'
#' @param x c
#'
#' @return ee
#'
#' @section a section:
#' content
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map
#' @export
#'

#
f1 = function(x){
  x = date(x)
  x = mutate(x, a=0)
  x = write_utf8(x, a=0)
  x = filter(x, TRUE)
  x = select(x, TRUE)
  x = knitr::asis_output(all(x), TRUE)
  x = "#' @importFrom dplyr mutate"
  f = function(a) a
  x = dezdezde(x, TRUE)
  x = map(x, TRUE)
  stop("ok")
}


#' Title f2
#'
#' This is f2
#'
#' @return ee
#' @export
#' @examples
#' x=1
#' @importFrom dplyr select
f2 <- function(x){
  x = select(x, TRUE)
  stop("ok")
}


#' @importFrom dplyr select
f3 <- function(x){
  x = select(x, TRUE)
  stop("ok")
}

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

1

#this is
#a trailing comment
