



#duplicate function
f2 = function(){
  1
}
f2 = function(){
  ggplot()
}


#from roxygen, not in DESCRIPTION
warn_in_desc = function(){
  x = rd_roclet()
}


#private function, should override dplyr::mutate
mutate = function(){
  filter("foo")
}

#private function, should override dplyr::filter
filter = function(){
  1
}

#private function, should not conflict with autoimport:::assert
assert = function(){
  1
}


#function with inner function
#' @importFrom dplyr filter
foobar = function(){
  filter <- base::identity #inner function, should override autoimport::filter
  glimpse = function() 1

  filter("foo")
  glimpse("foo")
  abcdefgh()
  wxyz()
  bind_rows()
}


#this is
#a trailing comment
#with only one empty line at EOF
