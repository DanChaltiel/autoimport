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

#this is a useless comment line
f1 = function(x){
  #private functions, should not be imported
  x = mutate(x, a=0) #remove existing import
  x = assert(x, TRUE)
  x = filter(x, TRUE)
  #explicit calls, should not be imported
  x = dplyr::arrange(x, TRUE)
  x = glue::glue(x, TRUE)
  #base function, should not be imported
  x = sum(x)
  x = date(x) #not from lubridate (IMPORTLIST)
  #other functions, should be imported
  x = pivot_longer(x, a=0)
  x = set_names(map(x), TRUE)
  x = div(x, TRUE) #from shiny, not html
  #juste a variable, should be ignored
  x = "#' @importFrom dplyr mutate"
  #inner function, should be ignored
  f = function(a) a
  x = f()
  g = if(TRUE) na.omit else identity
  x = g()
  #R6 function, should be ignored
  x = x$met()
  stop("ok")
}


#' Title f2
#'
#' This is f2
#'
#' @return ee
#' @export
#' @examples
#' @importFrom rlang :=
#' x=1
f2 <- function(x){
  x := select(x, TRUE)
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

