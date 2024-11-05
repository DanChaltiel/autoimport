# autoimport

    Code
      autoimport(files = dir(dir_source, full.names = TRUE), pkg_name = "autoimport", ignore_package = TRUE, use_cache = FALSE,
      namespace_file = namespace_file, description_file = description_file, ask = FALSE, verbose = 2)
    Message
      
      -- Init ---------------------------------------------------------------------------------------------------------------------
      v Registered namespaces of 19 dependencies.
      
      -- Reading ------------------------------------------------------------------------------------------------------------------
      i Found 2 functions in file 'source/sample_code-package.R' (7 lines)
      i Found 6 functions in file 'source/sample_funs.R' (63 lines)
      i Found 2 functions in file 'source/sample_funs2.R' (12 lines)
      v Found a total of 10 internal functions in 3 files (82 lines).
      
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
      ! Updating cache
      i Found 1 function to import in 2 functions or code chunks.
      v Found a total of 14 potential functions to import
      
      -- Attributing --------------------------------------------------------------------------------------------------------------
      i Automatically attributing 2 functions imports from './inst/IMPORTLIST'
      
      -- Writing ------------------------------------------------------------------------------------------------------------------
      v Ignoring 'source/sample_code-package.R'. Use `ignore_package=FALSE` to override.
      i 4 inserts in 'source/sample_funs.R'
      v Added 3 and removed 3 lines from 'source/sample_funs.R'.
      i 0 inserts in 'source/sample_funs2.R'
      > Nothing done in 'source/sample_funs2.R' (all is already OK)
      
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
       [1] "1"                                      ""                                      
       [3] "#' Title f1"                            "#'"                                    
       [5] "#' a description"                       "#' @param x c"                         
       [7] "#' @return ee"                          "#' @section a section:"                
       [9] "#' content"                             "#' @export"                            
      [11] "#"                                      "f1 = function(x){"                     
      [13] "  x = date(x)"                          "  x = mutate(x, a=0)"                  
      [15] "  x = write_utf8(x, a=0)"               "  x = filter(x, TRUE)"                 
      [17] "  x = select(x, TRUE)"                  "  x = knitr::asis_output(all(x), TRUE)"
      [19] "  x = \"#' @importFrom dplyr mutate\""  "  f = function(a) a"                   
      [21] "  x = dezdezde(x, TRUE)"                "  x = map(x, TRUE)"                    
      [23] "  stop(\"ok\")"                         "}"                                     
      [25] "#' Title f2"                            "#' This is f2"                         
      [27] "#' @examples"                           "#' x=1"                                
      [29] "f2 <- function(x){"                     "f3 <- function(x){"                    
      [31] "#' @importFrom dplyr %>%"               "dplyr::`%>%`"                          
      [33] "#this is"                               "#a trailing comment"                   
      
      $adds
      [1] "#' @importFrom dplyr filter mutate select" "#' @importFrom purrr map"                 
      [3] "#' @importFrom dplyr select"              
      
      $removals
      [1] "#' @importFrom dplyr mutate"      "#' @importFrom dplyr mutate_all"  "#' @importFrom forcats as_factor"
      
    Code
      poor_diff("sample_funs2.R")
    Output
      NULL

