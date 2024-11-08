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
  x = filter(x, TRUE)
  #explicit calls, should not be imported
  x = dplyr::arrange(x, TRUE)
  x = knitr::asis_output(x, TRUE)
  #base function, should not be imported
  x = mean(x)
  #other functions, should be imported
  x = date(x) #from lubridate, not base
  x = pivot_longer(x, a=0)
  x = set_names(map(x), TRUE)
  x = div(x, TRUE) #from shiny, not html
  #should be ignored
  x = "#' @importFrom dplyr mutate"
  f = function(a) a
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
