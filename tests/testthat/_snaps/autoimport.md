# autoimport

    Code
      autoimport(files = dir(dir_source, full.names = TRUE), pkg_name = "autoimport",
      ignore_package = TRUE, use_cache = FALSE, namespace_file = namespace_file,
      description_file = description_file, ask = FALSE, verbose = 2)
    Message
      
      -- Init ------------------------------------------------------------------------
      v Registered namespaces of 19 dependencies.
      
      -- Reading ---------------------------------------------------------------------
      i Found 2 functions in file 'source/sample_code-package.R' (7 lines)
      i Found 1 function in file 'source/sample_dup.R' (6 lines)
      i Found 7 functions in file 'source/sample_funs.R' (64 lines)
      v Found a total of 10 internal functions in 3 files (77 lines).
      
      -- Warning - Duplicates --
      
    Condition
      Warning:
      x There is several functions with the same name:
    Output
         fun                 file
      1 f2()  source/sample_dup.R
      2 f2() source/sample_funs.R
      3 f3() source/sample_funs.R
      4 f3() source/sample_funs.R
    Message
      
      -- Parsing ---------------------------------------------------------------------
      > File 'source/sample_code-package.R'
      i Parsing code block `unnamed_1`
      i Parsing code block `unnamed_2`
      ! Updating cache
      i Found 0 functions to import in 2 functions or code chunks.
      > File 'source/sample_dup.R'
      i Parsing function `f2()`
      ! Updating cache
      i Found 1 function to import in 1 function or code chunk.
      > File 'source/sample_funs.R'
      i Parsing code block `unnamed_1`
      i Parsing function `f1()`
      i Parsing function `f2()`
      i Parsing function `f3()`
      i Parsing function `f3()`
      i Parsing code block `unnamed_2`
      i Parsing code block `unnamed_3`
      ! Updating cache
      i Found 13 functions to import in 7 functions or code chunks.
      v Found a total of 14 potential functions to import
      
      -- Attributing -----------------------------------------------------------------
      i Automatically attributing 2 functions imports from './inst/IMPORTLIST'
      
      -- Writing ---------------------------------------------------------------------
      v Ignoring 'source/sample_code-package.R'. Use `ignore_package=FALSE` to override.
      i 1 inserts in 'source/sample_dup.R'
      v Added 1 and removed 0 lines from 'source/sample_dup.R'.
      i 4 inserts in 'source/sample_funs.R'
      v Added 3 and removed 3 lines from 'source/sample_funs.R'.
      
      -- Finished --------------------------------------------------------------------
      v To view the diff and choose whether or not accepting the changes, run:
      i `autoimport::import_review("source")`

