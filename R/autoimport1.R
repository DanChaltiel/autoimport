
autoimport1 = function(file, out=get_new_file(file), ask=TRUE, verbose=2){
  if(!interactive()){
    cli_abort("Only interactive!")
  }
  if (!file.exists(file)) {
    cli_abort("Couldn't find file {.file {file}}")
  }
  if (!str_ends(file, "\\.[Rr]")) {
    cli_warn("expecting *.R file, will try to proceed.")
  }
  
  lines = read_lines(file)
  parsed = parse(text=lines, keep.source=TRUE)
  comments_refs = getSrcref(parsed) %>% comments() %>% set_names_ref()
  if(verbose>0) cli_inform(c(v="Found {length(import_list)} functions in file {.file {file}}."))
  
  import_list = list_importFrom(comments_refs, verbose=verbose>1)
  n_imports = sum(map_dbl(import_list, nrow))
  if(verbose>0) cli_inform(c(v="Found {n_imports} potential import{?s} in {.file {file}}"))
  
  user_choice = get_user_choice(import_list, ask=ask)
  
  # .x=import_list$f1
  inserts = import_list %>% 
    map(~{
      if(is.null(.x)) return(NULL)
      if(nrow(.x)==0) return(NULL)
      .x %>% 
        mutate(pkg = if_else(lengths(pkg)>1, user_choice[fun], pkg) %>% unlist()) %>% 
        group_by(pkg) %>% 
        summarise(label = paste(cur_group(), paste(fun, collapse=" "))) %>% 
        filter(!is.na(pkg) & pkg!="base") %>% 
        pull(label)
    })
  
  lines2 = comments_refs %>% 
    imap(~{
      insert = glue("#' @importFrom {inserts[[.y]]}")
      f1_c = as.character(.x)
      rmv = str_starts(f1_c, "#+' *@importFrom")
      if(any(rmv)){
        pos = min(which(rmv))
        f1_c = f1_c[!rmv]
      } else {
        x = parse(text=f1_c, keep.source=TRUE) %>% get_srcref_lines()
        stopifnot(length(x)==1)
        pos = x[[1]]$first_line_fun
      }
      insert_line(f1_c, insert, pos=pos)
    }) %>% unname() %>% unlist()
  
  if(identical(lines, lines2)){
    if(verbose>0) cli_inform(c(v="Nothing done in {.file {file}}"))
    return(FALSE)
  }
  
  n_new = setdiff(lines2, lines) %>% length()
  n_old = setdiff(lines, lines2) %>% length()
  
  if(verbose>0) cli_inform(c(v="Added {n_new} and removed {n_old} line{?s} from {.file {file}} to write {.file {out}}"))
  
  write_utf8(out, lines2)
  TRUE
}