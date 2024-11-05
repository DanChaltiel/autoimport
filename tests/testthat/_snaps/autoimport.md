# autoimport

    Code
      autoimport(files = dir(dir_source, full.names = TRUE, pattern = "\\.R$"), pkg_name = "autoimport_test", ignore_package = TRUE,
      use_cache = FALSE, namespace_file = namespace_file, description_file = description_file, ask = FALSE, verbose = 2)
    Message
      
      -- Init ---------------------------------------------------------------------------------------------------------------------
      v Registered namespaces of 19 dependencies.
      
      -- Reading ------------------------------------------------------------------------------------------------------------------
      i Found 2 functions in file 'source/sample_code-package.R' (7 lines)
      i Found 6 functions in file 'source/sample_funs.R' (63 lines)
      i Found 4 functions in file 'source/sample_funs2.R' (32 lines)
      v Found a total of 12 internal functions in 3 files (102 lines).
      
      -- Warning - Duplicates --
      
    Condition
      Warning:
      x There is several functions with the same name:
    Output
         fun                  file
      1 f2()  source/sample_funs.R
      2 f2() source/sample_funs2.R
    Message
      
      -- Parsing ------------------------------------------------------------------------------------------------------------------
      > File 'source/sample_code-package.R'
      i Parsing code block `unnamed_1`
      i Parsing code block `unnamed_2`
      ! Updating cache
      i Found 0 functions to import in 2 functions or code chunks.
      > File 'source/sample_funs.R'
      i Parsing code block `unnamed_1`
      i Parsing function `f1()`
      i Parsing function `f2()`
      i Parsing function `f3()`
      i Parsing code block `unnamed_2`
      i Parsing code block `unnamed_3`
      ! Updating cache
      i Found 13 functions to import in 6 functions or code chunks.
      > File 'source/sample_funs2.R'
      i Parsing function `f2()`
      i Parsing function `mutate()`
      i Parsing function `filter()`
      i Parsing function `foobar()`
      ! Updating cache
      i Found 5 functions to import in 4 functions or code chunks.
      v Found a total of 18 potential functions to import
      
      -- Attributing --------------------------------------------------------------------------------------------------------------
      i Automatically attributing 1 functions imports from './inst/IMPORTLIST'
      i Automatically attributing 1 functions imports, as `ask==FALSE`
      i TESTING: not saving choices in './inst/IMPORTLIST'
      
      -- Writing ------------------------------------------------------------------------------------------------------------------
      v Ignoring 'source/sample_code-package.R'. Use `ignore_package=FALSE` to override.
      i 5 inserts in 'source/sample_funs.R'
      v Added 3 and removed 3 lines from 'source/sample_funs.R'.
      i 2 inserts in 'source/sample_funs2.R'
      v Added 2 and removed 1 line from 'source/sample_funs2.R'.
      
      -- Finished -----------------------------------------------------------------------------------------------------------------
      v To view the diff and choose whether or not accepting the changes, run:
      i `autoimport::import_review("source")`
    Code
      poor_diff("sample_code-package.R")
    Output
      NULL
    Code
      poor_diff("sample_funs.R")
    Output
      $common
       [1] "1"                                                               
       [2] ""                                                                
       [3] "#' Title f1"                                                     
       [4] "#'"                                                              
       [5] "#' a description"                                                
       [6] "#' @param x c"                                                   
       [7] "#' @return ee"                                                   
       [8] "#' @section a section:"                                          
       [9] "#' content"                                                      
      [10] "#' @export"                                                      
      [11] "#"                                                               
      [12] "f1 = function(x){"                                               
      [13] "  x = date(x)"                                                   
      [14] "  x = mutate(x, a=0) #private function, should not be imported"  
      [15] "  x = write_utf8(x, a=0)"                                        
      [16] "  x = filter(x, TRUE)  #private function, should not be imported"
      [17] "  x = select(x, TRUE)"                                           
      [18] "  x = knitr::asis_output(all(x), TRUE)"                          
      [19] "  x = \"#' @importFrom dplyr mutate\""                           
      [20] "  f = function(a) a"                                             
      [21] "  x = dezdezde(x, TRUE)"                                         
      [22] "  x = map(x, TRUE)"                                              
      [23] "  stop(\"ok\")"                                                  
      [24] "}"                                                               
      [25] "#' Title f2"                                                     
      [26] "#' This is f2"                                                   
      [27] "#' @examples"                                                    
      [28] "#' x=1"                                                          
      [29] "f2 <- function(x){"                                              
      [30] "f3 <- function(x){"                                              
      [31] "#' @importFrom dplyr %>%"                                        
      [32] "dplyr::`%>%`"                                                    
      [33] "#this is"                                                        
      [34] "#a trailing comment"                                             
      
      $adds
      [1] "#' @importFrom autoimport write_utf8" "#' @importFrom dplyr select"          "#' @importFrom purrr map"            
      
      $removals
      [1] "#' @importFrom dplyr mutate"      "#' @importFrom dplyr mutate_all"  "#' @importFrom forcats as_factor"
      
    Code
      poor_diff("sample_funs2.R")
    Output
      $common
       [1] ""                                                                              
       [2] "#duplicate function from another file"                                         
       [3] "f2 = function(){"                                                              
       [4] "  1"                                                                           
       [5] "}"                                                                             
       [6] "#private function, should override dplyr::mutate"                              
       [7] "mutate = function(){"                                                          
       [8] "  filter(\"foo\")"                                                             
       [9] "#private function, should override dplyr::filter"                              
      [10] "filter = function(){"                                                          
      [11] "#function with inner function"                                                 
      [12] "foobar = function(){"                                                          
      [13] "  filter <- base::identity #inner function, should override autoimport::filter"
      [14] "  glimpse = function() 1"                                                      
      [15] "  glimpse(\"foo\")"                                                            
      [16] "  abcdefgh()"                                                                  
      [17] "  bind_rows()"                                                                 
      
      $adds
      [1] "#' @importFrom dplyr bind_rows" "#' @importFrom pillar glimpse" 
      
      $removals
      [1] "#' @importFrom dplyr filter"
      

