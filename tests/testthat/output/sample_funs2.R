



#duplicate function from another file
f2 = function(){
  1
}


#private function, should override dplyr::mutate
mutate = function(){
  filter("foo")
}

#private function, should override dplyr::filter
filter = function(){
  1
}


#function with inner function
#' @importFrom dplyr bind_rows
#' @importFrom pillar glimpse
foobar = function(){
  filter <- base::identity #inner function, should override autoimport::filter
  glimpse = function() 1

  filter("foo")
  glimpse("foo")
  abcdefgh()
  bind_rows()
}
