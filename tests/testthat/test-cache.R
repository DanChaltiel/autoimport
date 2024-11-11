

test_that("autoimport cache works", {

  #STEP 1: create cache

  files = c("sample_funs.R", "sample_funs2.R")
  ai_1 = test_autoimport(files, use_cache="write",
                         verbose=0) %>%
    suppressWarnings() %>%
    expect_silent() #check that verbose=0 is silent

  expect_setequal(ai_1$ai_source, "file")
  root = attr(ai_1, "root")
  cache_path_1 = attr(ai_1, "cache_path")
  expect_true(file.exists(cache_path_1))


  #STEP 2: read cache from file

  ai_2 = test_autoimport(files, use_cache=TRUE, root=root,
                         verbose=0) %>%
    suppressMessages() %>%
    suppressWarnings()

  expect_equal(attr(ai_2, "root"), root)

  expect_setequal(ai_2$ai_source, "cache_file")

  expect_equal(normalizePath(attr(ai_1, "cache_path")),
               normalizePath(attr(ai_2, "cache_path")))


  #STEP 3: modify a single function in a file

  mod_file_path = attr(ai_2, "files") %>% str_subset("funs2")
  mod_file = readLines(mod_file_path)
  fun_line = mod_file %>% str_detect("function()") %>% which() %>% min()
  add_line = "  x = abs(x)"
  mod_file2 = c(mod_file[1:fun_line+1], add_line, mod_file[(fun_line+2):length(mod_file)])
  writeLines(mod_file2, mod_file_path)


  #STEP 4: read cache from file & refs, except for 1 function

  ai_3 = test_autoimport(files, use_cache="read", root=root,
                         verbose=0) %>%
    suppressMessages() %>%
    suppressWarnings()

  cnt = ai_3 %>% count(ai_source) %>% pull(n, name=ai_source) %>% as.list()
  expect_true(cnt$file == 1)      #only one function parsed again from file
  expect_true(cnt$cache_file > 1) #~15 functions read from the cached file
  expect_true(cnt$cache_ref > 1)  #~7  functions read from cached refs

})
