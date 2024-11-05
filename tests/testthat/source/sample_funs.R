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
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @export
#' @importFrom forcats as_factor
#'

#
f1 = function(x){
  x = date(x)
  x = mutate(x, a=0) #private function, should not be imported
  x = write_utf8(x, a=0)
  x = filter(x, TRUE)  #private function, should not be imported
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
f2 <- function(x){
  x = select(x, TRUE)
  stop("ok")
}


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
